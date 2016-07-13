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

gram.size <- 3

# Build a predictive model, in the form of a hash, that maps n-grams to
# predicted words.
# 
# corpus: A standard {tm} Corpus with one sentence per document, for splitting
#         into n-grams.
# gram.size: The size of an n-gram.
# min.count: The minimum number of occurrences of an n-gram to be noticed.
# 
BuildHashModel <- function(corpus, gram.size = 3, min.count = 3) {
    require(tm)
    require(RWeka)
    require(hash)
    print(paste(c("Building", gram.size, "gram hash model from corpus, min count", min.count), collapse = " "))
    
    # Required on Mac to prevent NGramTokenizer from failing.
    options(mc.cores = 1)
    
    print("=== Building TDM...")
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ModelTokenizer))
    print("=== Segmenting TDM into dataframe...")
    df  <- SegmentTextDocumentMatrix(tdm, gram.length, min.count)
    print("=== Building hash model...")
    hash.model <- HashModelFromDataframe(df)
    
    hash.model
}

BuildMultiGramModel <- function(corpus, max.gram.size = 3, min.count = 3) {
    require(tm)
    require(RWeka)
    require(hash)
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
        dfs[[i]] <- df
    }
    
    print("Done building model dataframes.")
    dfs
}

BuildTdm <- function(corpus, gram.size) {
    require(tm)
    require(RWeka)
    
    if (gram.size == 1) {
        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = Tokenizer1))
    } else if (gram.size == 2) {
        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = Tokenizer2))
    } else if (gram.size == 3) {
        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = Tokenizer3))
    } else if (gram.size == 4) {
        tdm <- TermDocumentMatrix(corpus, control = list(tokenize = Tokenizer4))
    }
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
#    o <- order(terms)
#    df <- df %>% mutate(term = terms[term.index[o]], total.freq = total.freq[o])
    #   TODO get the above working
    
    df
}

# Tokenizers for n-grams.
# 
Tokenizer1 <- function(doc) {
    require(RWeka)
    RWeka::NGramTokenizer(doc, control = Weka_control(min = 1, max = 1))
}
Tokenizer2 <- function(doc) {
    require(RWeka)
    RWeka::NGramTokenizer(doc, control = Weka_control(min = 2, max = 2))
}
Tokenizer3 <- function(doc) {
    require(RWeka)
    RWeka::NGramTokenizer(doc, control = Weka_control(min = 3, max = 3))
}
Tokenizer4 <- function(doc) {
    require(RWeka)
    RWeka::NGramTokenizer(doc, control = Weka_control(min = 4, max = 4))
}

# For hash model, just one tokenizer
ModelTokenizer <- Tokenizer3


# Split a TextDocumentMatrix with space-delimited, (n+1)-gram "terms" into a
# dataframe with "firstn" and "last1" elements that correspond to the first
# n words and the most common last word.
# 
# tdm: The TextDocumentMatrix.
# gram.length: The number of words to use in the final "firstn" element. The
#              input text.df$terms should have gram.length + 1 words in it.
# min.count: Drop all n-grams that occur fewer than this many times.
# 
SegmentTextDocumentMatrix <- function(tdm, gram.length, min.count) {    
    require(dplyr)
    print(paste(c("Finding most common next term for each", as.character(gram.length), "gram..."), collapse = " "))
    
    # Build dataframe from TermDocumentMatrix
    df <- DataframeFromTextDocumentMatrix(tdm)
    df <- CollapseTermsFromSeparateDocuments(df)
    df <- SplitDataframeByGrams(df, gram.length, min.count)
    df <- FindMostCommonFollowingTerm(df)
    
    df
}

# Convert a TDM into a dataframw with equivalent data, for easier processing.
# 
DataframeFromTextDocumentMatrix <- function(tdm) {
    print("Converting TextDocumentMatrix into Dataframe...")
    
    data.frame(
        term = tdm$dimnames$Terms[tdm$i],
        term.index = tdm$i,
        doc.no = tdm$j,
        reps.in.doc = tdm$v,
        stringsAsFactors = FALSE
    )
}

CollapseTermsFromSeparateDocuments <- function(df) {
    require(dplyr)
    print("Collapsing and counting (n+1)-grams...")
    
    df %>%
        group_by(term) %>%
        summarize(n = sum(reps.in.doc))
}

SplitDataframeByGrams <- function(df, gram.length, min.count) {
    require(dplyr)
    print(paste(c("Splitting (n+1)-grams into", gram.length, "grams and next terms, with counts..."), collapse = " "))
    
    df <- df %>%
        rowwise() %>%
        mutate(firstn = firstn(term)) %>%
        mutate(last1 = last1(term)) %>%
#        count(firstn, last1) %>%
        select(-term)
#        filter(n >= min.count)

    head(df)
    df
}

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

# Build hash model from dataframe.
# Hash model has ngrams for keys (e.g., "bears eat mostly"), and predicted
# next word as value ("people").
# The bulk of the processing has already gone into creating the dataframe, so
# this is just a reading into a hash used for prediction.
# 
# df: dataframe with (firstn, last1)
# 
HashModelFromDataframe <- function(df) {
    require(hash)
    print("Building hash model...")
    
    h <- hash()
    
    # segmented.df has elements firstn and last1.
    for (i in 1:length(df$firstn)) {
        h[[df$firstn[[i]]]] <- df$last1[[i]]
    }
    
    h
}

# Predict the next word after a sequence of words, with a hash model.
# 
# hash.model: a hash of noramlized n-grams (space-delimited and lower case)
# words: a text string (character vector)
# 
Predict <- function(hash.model, words) {
    require(tm)
    require(hash)
    
    gram.size <- length(strsplit(ls(hash.model)[1], " ", fixed = TRUE)[[1]])
    input.gram <- NormalizeInput(words, gram.size)
    print(input.gram)
    
    if (has.key(input.gram, hash.model)) {
        print(hash.model[[input.gram]])
    } else {
        print("<not found>")
    }
}

PredictMultiGram <- function(multigram.model, text) {
    require(tm)
    
    #    words <- strsplit(text, "\\s+")
    
    match.found <- TRUE
    word.count <- 1
    best.matches <- c("")
    
    while (match.found) {
        words <- NormalizeInput(text, word.count) # ["hello", "world"]
        ngram <- paste(words, collapse = " ")     # "hello world"
        ngram.len   <- nchar(ngram)               # 11
        
        matching.indices <- substr(multigram.model$term, 1, ngram.len) == ngram
        matching.terms   <- multigram.model[matching.indices, c("term", "total.freq")]
        
        if (length(matching.terms$term) > 0) {
            match.found <- TRUE
            word.count  <- word.count + 1
            
            most.matches       <- max(matching.terms$total.freq)
            best.matches <- matching.terms[matching.terms$total.freq == most.matches, ]$term
            
            print(paste(best.matches, collapse = ", "))
        } else {
            match.found <- FALSE
        }
    }
    
    print("Found:")
    print(paste(best.matches, collapse = ", "))
    
    df <- multigram.model[substr(multigram.model$term, 0, nchar(text))]
    
    df
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
            ngram <- paste(words[1:(word.count - 1)], collapse = " ") # "hello world"
        }
        ngram.len        <- nchar(ngram)               # 11
        
        if (ngram.len == 0) {
            matching.indices <- rep(TRUE, length(df$term))
        } else {
            matching.indices <- substr(multigram.model.dfs$term, 1, ngram.len) == ngram
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

firstn <- function(s) {
    substr(s, 0, LastSpace(s) - 1)
}

last1 <- function(s) {
    substr(s, LastSpace(s) + 1, nchar(s))
}

LastSpace <- function(s) {
    i <- nchar(s)
    while (substr(s, i, i) != " ") {
        i <- i - 1
    }
    i
}

#last1  <- function(tri, gram.length) strsplit(tri, " ", fixed = TRUE)[[1]][gram.length + 1]

#firstn <- function(tri, gram.length) paste(strsplit(tri, " ", fixed = TRUE)[[1]][1:gram.length], collapse = " ")
#last1  <- function(tri, gram.length) strsplit(tri, " ", fixed = TRUE)[[1]][gram.length + 1]

##############################################
# OBE

# now split into several functions (above)
SegmentDataframe <- function(df, gram.length, min.count) {
    # Aggregate term counts across documents
    df <- df %>%
        group_by(term) %>%
        summarize(n = sum(reps.in.doc))
    
    # Split terms into n-gram and next value
    df <- df %>%
        rowwise() %>%
        #        group_by(term) %>%
        #        count() %>%
        mutate(firstn = firstn(term)) %>%
        mutate(last1 = last1(term)) %>%
        count(firstn, last1) %>%
        filter(n >= min.count) %>%
        group_by(firstn, last1) %>%
        top_n(n = 1)
    
    df
}
