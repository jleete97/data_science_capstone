
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

# Convert a line of text into a vector of tokens.
#
# parameters:
#   line:     the line of text (characters) to tokenize
# returns: a character vector populated with tokens.
tokenize <- function(line) {
    tokens <- stri_match_all(line, regex = token.re, extended = TRUE)
    tokens <- lapply(tokens[[1]], normalize.token)
    tokens <- paste(tokens, collapse = " ")
    
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

# Filter the specified input file, producing an output file of
# tokenized lines matching randomly-selected lines in the input
# file (with probability p). All tokens are upper case.
#
filter <- function(filename, p = 1.0, word.df, ngram.df) {
    # Generate output file "news.tokens.txt" from "en_US.news.txt"
    filename.parts <- strsplit(filename, split = "\\.")
    fn <- filename.parts[2]
    outfilename <- paste(filename.parts[[1]][2], ".tokens.txt", sep = "")
    
    # Set input, output
    con <- file(filename, "r")
    sink(outfilename)
    
    # Tokenize randomly selected lines, send to output
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        rand <- rbinom(n = 1, size = 1, prob = p)
        
        if (rand == 1) {
            tokens <- tokenize(line)
            token.line <- paste(tokens[[1]], collapse = " ")
            s <- paste(token.line[1], "\n", sep = "")
            cat(s)
        }
    }
    
    # Close input, output
    close(con)
    sink()
}
