# Convert a "cleaned-up" (tm) Corpus into an n-gram text prediction model.
#
# Author: Jon Leete
# 

# Increase JVM memory
options(java.parameters = "-Xms3g -Xmx3g -XX:MaxPermSize=512m")

library(tm)
library(hash)
library(RWeka)
library(dplyr)

# Build model as vector of dataframes, for increasing n-gram sizes
# (first is 1-gram prediction: i.e., most common word, etc.)
# 
BuildMultiGramModel <- function(corpus, max.gram.size = 3, min.count = 3) {
    require(tm)
    require(RWeka)
    print(paste(c("Building", gram.size, "gram multi-gram model from corpus, max gram size", max.gram.size), collapse = " "))
    
    # Required on Mac to prevent NGramTokenizer from failing.
    options(mc.cores = 1)
    
    print("=== Building TDMs...")
    dfs  <- list()
    
    for (i in 1:max.gram.size) {
        print(paste(c("Building TDM", i, "..."), collapse = " "))
        tdm <- BuildTdm(corpus, i)
        print(paste(c("Building dataframe", i, "..."), collapse = " "))
        df  <- ExtractDataFromTdm(tdm)
        tdm <- NULL # explicitly free space
        dfs[[as.character(i)]] <- df
    }
    
    print("Done building model dataframes.")
    dfs
}

BuildTdm <- function(corpus, gram.size) {
    require(tm)
    require(RWeka)
    
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = tokenizers[[gram.size]]))

#    if (gram.size == 1) {
#        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = Tokenizer1))
#    } else if (gram.size == 2) {
#        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = Tokenizer2))
#    } else if (gram.size == 3) {
#        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = Tokenizer3))
#    } else if (gram.size == 4) {
#        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = Tokenizer4))
#    }
}

# Build a dataframe from a TermDocumentMatrix, which contains the essentials of
# the original data:
# 
# term       : the n-gram
# total.freq : the number of occurences of the n-gram.
# 
ExtractDataFromTdm <- function(tdm) {
    
    # N-gram terms
    terms  <- tdm$dimnames$Terms
    # Dataframe of indexes into term list, term frequencies (per document -- which we'll need to collapse)
    df     <- data.frame(term.index = tdm$i, doc.index = tdm$j, term.freq = tdm$v)
    
    # Collapse dataframe: term index, total term count (drops doc index)
    df <- df %>% group_by(term.index) %>% summarize(total.freq = sum(term.freq))
    # Substitute terms for indices
    df <- df %>% mutate(term = terms[term.index])
    # Reorder by term (optional?)
    
    df
}

# Tokenizers for n-grams.
# 
tokenizers <- vector()
tokenizers <- append(tokenizers, function(doc) {
    require(RWeka)
    RWeka::NGramTokenizer(doc, control = Weka_control(min = 1, max = 1))
})
tokenizers <- append(tokenizers, function(doc) {
    require(RWeka)
    RWeka::NGramTokenizer(doc, control = Weka_control(min = 2, max = 2))
})
tokenizers <- append(tokenizers, function(doc) {
    require(RWeka)
    RWeka::NGramTokenizer(doc, control = Weka_control(min = 3, max = 3))
})
tokenizers <- append(tokenizers, function(doc) {
    require(RWeka)
    RWeka::NGramTokenizer(doc, control = Weka_control(min = 4, max = 4))
})


FindMostCommonFollowingTerm <- function(df) {
    require(dplyr)
    print("Finding most common following terms...")
    
    df %>%
        rowwise() %>%
        group_by(firstn) %>%
        arrange(desc(n)) %>%
        top_n(n = 1) %>%
        select(-n)
#        slice(which.max(n))
}

PredictMultiGramDfs <- function(multigram.model.dfs, text) {
    require(tm)
    
    match.found <- TRUE
    word.count <- 1
    best.matches <- c("")
    
    while (match.found) {
        words <- NormalizeInput(text, word.count) # ["hello", "world"] for word.count = 2
        df    <- multigram.model.dfs[[word.count]]
        
        ngram <- ""
        if (word.count > 1) {
            ngram <- AllButLast(words)
#            ngram <- paste(words[1:(word.count - 1)], collapse = " ") # "hello world"
        }
        ngram.len        <- nchar(ngram)               # 11
        
        if (ngram.len == 0) {
            matching.indices <- rep(TRUE, length(df$term))
        } else {
            ngram.plus.space <- paste(c(ngram, " "), collapse = "")
            matching.indices <- substr(df$term, 1, ngram.len + 1) == ngram.plus.space
            # matching.indices <- df$term == ngram
        }
        matching.terms   <- df[matching.indices, c("term", "total.freq")]
        
        if (length(matching.terms$term) > 0) {
            match.found <- TRUE
            word.count  <- word.count + 1
            
            most.matches <- max(matching.terms$total.freq)
            best.matches <- matching.terms[matching.terms$total.freq == most.matches, ]$term
            
            print(paste(best.matches, collapse = ", "))
        } else {
            match.found <- FALSE
        }
    }
    
    print("Found:")
    print(paste(best.matches, collapse = ", "))
    
    best.matches
}

# Normalize a text string to a string of words of the type to match a key in
# our hash model. (For use with prediction string, not model-building.)
# 
# words: text string, with at least as many words as gram.size
# gram.size: number of words to keep (from the end of the string)
# 
NormalizeInput <- function(words, gram.size) {
    require(tm)
    
    words <- tolower(words)
#    words <- removeWords(words, stopwords("en"))
    words <- removePunctuation(words)
    words <- gsub("^\\s+|\\s+$", "", words)
    words <- gsub("\\s+", " ", words)
    
    word.vec <- strsplit(words, "\\s+")[[1]]
    start.pos <- length(word.vec) - gram.size + 1
    word.vec <- word.vec[start.pos : length(word.vec)]
    
    model.key <- paste(word.vec, collapse = " ")
    
    model.key
}

AllButLast <- function(text) {
    v   <- gregexpr(" ", text)
    l   <- v[[1]]
    len <- length(l)
    t   <- substring(text, 1, l[[len]] - 1)
    t
}