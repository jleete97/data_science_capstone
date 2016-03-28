#### Standard initialization

library(stringi)
library(plyr)
set.seed(314159)

#### Run parameters

# Use just words, or allow numbers and currency as tokens?
tokenize.words.only = TRUE

# Data file selection parameters
base.dir = "~/r/capstone"
locale = "en_US"

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

tokenize.line <- function(line) {
    # Convert a line of text into a vector of tokens.
    #
    # parameters:
    #   line:     the line of text (characters) to tokenize
    # returns: a character vector populated with tokens.
    tokens <- stri_match_all(line, regex = token.re, extended = TRUE)
    tokens <- lapply(tokens[[1]], normalize.token)
    
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

remove.token.files <- function(dir) {
    files <- list.files(path = dir, pattern = "\\.txt$")
    
    for (file in files) {
        path <- paste(dir, file, sep = "/")
        file.remove(path)
    }
}

process.file <- function(src, con, sample.fraction = 1.0, n = 3) {
    # Filter the specified input file, populating the specified data frame
    # with word or n-gram counts. Use randomly-selected lines in the input
    # file (with probability sample.fraction). All tokens are upper case.
    
    # Tokenize randomly selected lines, add tokens to data frames
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        rand <- rbinom(n = 1, size = 1, prob = sample.fraction)
        
        if (rand == 1) {
            tokens <- tokenize.line(line)
            cat(paste(tokens, sep = " "))
            cat(paste("\n"))
        }
    }
}

filter <- function(srcs, sample.fraction = 1.0, dir = "data", token.dir = "tokens", locale = "en_US", n = 3) {
    # Filter raw input files into tokenized files.
    
    for (src in srcs) {
        infilename <- paste(locale, src, "txt", sep = ".")
        outfilename <- paste(src, ".txt", sep = "")
        
        inpath <- paste(base.dir, dir, infilename, sep = "/")
        outpath <- paste(base.dir, token.dir, outfilename, sep = "/")
        con <- file(inpath, "r")
        
        sink(file = outpath, append = FALSE, type = "output")
        process.file(src, con, sample.fraction, n)
        sink(file = NULL)
        
        close(con)
    }
}
