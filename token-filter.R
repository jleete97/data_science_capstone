#### Standard initialization

library(stringi)
library(plyr)
set.seed(314159)

# Return the regular expression we use to recognize tokens.
# We can specify that we'll recognize words only ("cat", "we'll")
# or also numbers ("123,456.78") and currency amounts ("$12.99").
token.regex <- function(words.only) {
    word.re     <- "(?:[:alpha:]+(?:\\.[:alpha:]?|\\'[:alpha:]+)*)"
    number.re   <- "(?:[:digit:]{1,3}(?:(?:(?:\\,[:digit:]{3})|[:digit:]{1,2})*(?:\\.[:digit:]+)?))"
    currency.re <- paste("(?:\\$", number.re, "[MB]?", ")", sep = "")
    
    if (words.only) {
        re <- word.re
    } else {
        re <- paste(word.re, number.re, currency.re, sep = "|")
    }
    
    re
}

# Convert a line of text into a vector of tokens.
#
# parameters:
#   line:     the line of text (characters) to tokenize
# returns: a character vector populated with tokens.
tokenize.line <- function(line, token.re) {
    tokens <- stri_match_all(line, regex = token.re, extended = TRUE)
    tokens <- lapply(tokens[[1]], normalize.token)
    
    tokens
}

# Remove a final period from words that don't look like abbreviations
# (that have a vowel in them).
normalize.token <- function(s) {
    if (grepl(".*[aeiouAEIOU].*\\.$", s)) {
        toupper(substring(s, 1, nchar(s) - 1))
    } else {
        toupper(s)
    }
}

# Filter the specified input file, populating the specified data frame
# with word or n-gram counts. Use randomly-selected lines in the input
# file (with probability sample.fraction). All tokens are upper case.
process.file <- function(src, con, sample.rate = 1.0, words.only = TRUE) {
    token.re <- token.regex(words.only)
    
    while (length(lines <- readLines(con, n = 1000, warn = FALSE)) > 0) {
        
        # Tokenize randomly selected lines, add tokens to output
        for (i in 1:length(lines)) {
            rand <- rbinom(n = 1, size = 1, prob = sample.rate)
            
            if (rand == 1) {
                tokens <- tokenize.line(lines[[i]], token.re)
                
                if (length(tokens > 0)) {
                    cat(paste(tokens, sep = " "))
                    cat(paste("\n"))
                }
            }
        }
    }
}

# Filter raw input files (all .txt files in the given directory)
# into a single tokenized file.
tokenize.file <- function(source.dir, output.file, sample.rate = 1.0, words.only = TRUE) {

    file.remove(output.file)
    source.files <- list.files(source.dir, pattern = "\\.txt$")
    
    for (src in source.files) {
        in.path <- paste(source.dir, src, sep = "/")
        con <- file(in.path, "r")
        
        sink(file = output.file, append = TRUE, type = "output")
        process.file(src, con, sample.rate, words.only)
        sink(file = NULL)
        
        close(con)
    }
}
