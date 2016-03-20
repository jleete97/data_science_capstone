
# Setup
library(stringi)
set.seed(314159)

tokenize.words.only = TRUE

# Regular expression denoting a word or equivalent that we want to treat as a single token.
token.re <- (
    function() {
        word.re     <- "(?:[:alpha:]+(?:\\.[:alpha:]?|\\'[:alpha:]+)*)"
        number.re   <- "(?:[:digit:]{1,3}(?:(?:(?:\\,[:digit:]{3})|[:digit:]{1,2})*(?:\\.[:digit:]+)?))"
        currency.re <- paste("(?:\\$", number.re, "[MB]?", ")", sep = "")
        
        if (tokenize.words.only) {
            re <- word.re
        } else {
            re <- paste(word.re, number.re, currency.re, sep = "|")
        }
        
        re
    })()

tokenize <- function(line) {
    # Convert a line of text into a vector of tokens.
    #
    # parameters:
    #   line:     the line of text (characters) to tokenize
    # returns: a character vector populated with tokens.
    tokens <- stri_match_all(line, regex = token.re, extended = TRUE)
    tokens <- lapply(tokens[[1]], normalize.token)
#    tokens <- paste(tokens, collapse = " ")
    
    tokens
}

normalize.token <- function(s) {
    # Remove a final period from words that don't look like abbreviations
    # (that have a vowel in them).
    
    if (grepl(".*[aeiouAEIOU].*\\.$", s)) {
        toupper(substring(s, 1, nchar(s) - 1))
    } else {
        toupper(s)
    }
}

populate.word.df <- function(src, word) {
    # Add the word to the word data frame.
    
    i <- which(word.df$word == word)
    
    if (length(i) == 0) {
        # Word not previously found: add row
        old.len = nrow(word.df$word)
        if (is.null(old.len)) {
            old.len = 0;
        }
        new.len <- old.len + 1
        current.width <- length(word.df)
        
        word.df[new.len, 1] <- word
        word.df[new.len, 2:current.width] <- integer(current.width - 1)
        word.df[new.len, current.width] <- 1
    } else {
        # Word previously found: increment
        word.df[i, src] <- word.df[i, src] + 1
    }
}

populate.ngram.df <- function(src, tokens, i) {
    # Add the appropriate n-gram (tokens i and i-1) to the n-gram data frame.
    
}

populate.dfs <- function(src, con, sample.fraction = 1.0) {
    # Filter the specified input file, populating data frames with
    # word and n-gram counts. Use randomly-selected lines in the input
    # file (with probability sample.fraction). All tokens are upper case.
    
    # Add column for source to word and n-gram DF's
#    word.df  <- cbind(word.df,  integer(length(word.df$word)))
#    ngram.df <- cbind(ngram.df, integer(length(ngram.df$ngram)))
#    # Name colum according to source
#    names(word.df)[length(word.df)] <- src
#    names(ngram.df)[length(ngram.df)] <- src
    
    # Tokenize randomly selected lines, add tokens to data frames
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        rand <- rbinom(n = 1, size = 1, prob = sample.fraction)
        
        if (rand == 1) {
            tokens <- tokenize(line)
            print(paste("adding tokens:", tokens))
            
            for (i in 1:length(tokens)) {
                populate.word.df(src, tokens[i])
                populate.ngram.df(src, tokens, i)
            }
        }
    }    
}

create.df <- function(first.col.name, srcs) {
    # Create a data frame with the given first column name,
    # plus a column for each source.
    
    all.colnames <- append(srcs, first.col.name, after = 0)
    df <- read.table(text = "", col.names = all.colnames, stringsAsFactors = FALSE)
    
    df
}

build.dfs <- function(srcs, sample.fraction = 1.0, dir = ".", locale = "en_US") {
    # Populate the word and ngram data frames from files
    # for each of the sources specified.
    
    for (src in srcs) {
        filename <- paste(locale, src, "txt", sep = ".")
        path <- paste(dir, filename, sep = "/")
        print(paste("Reading", path))
        con <- file(path, "r")
        
        populate.dfs(src, con, sample.fraction)
        
        close(con)
    }
}

run <- function(srcs, sample.fraction = 1.0, dir = ".", locale = "en_US") {
    
    build.dfs(srcs, sample.fraction, dir, locale)
    
    print(word.df)
}

word.df <- create.df("word", srcs)
ngram.df <- create.df("word", srcs)
