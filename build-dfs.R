
# Setup
library(stringi)
library(plyr)
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

populate.word.df <- function(src, word, word.df) {
    # Add the word to the word data frame.
    
    i <- which(word.df$word == word)
    
    if (length(i) == 0) {
        # Word not previously found: add row
        old.len = nrow(word.df)
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
    
    word.df
}

populate.ngram.df <- function(src, tokens, i, n, df) {
    # Add the appropriate n-gram (tokens i and i-1) to the n-gram data frame.
    
    if (i >= n) {
        first <- i - n + 1
        word <- paste(tokens[first:i], collapse = " ")
        
        i <- which(df$word == word)
        
        if (length(i) == 0) {
            # Word not previously found: add row
            old.len = nrow(df)
            if (is.null(old.len)) {
                old.len = 0;
            }
            new.len <- old.len + 1
            current.width <- length(df)
            
            df[new.len, 1] <- word
            df[new.len, 2:current.width] <- integer(current.width - 1)
            df[new.len, current.width] <- 1
        } else {
            # Word previously found: increment
            df[i, src] <- df[i, src] + 1
        }
    }
    
    df
}

populate.df <- function(src, con, sample.fraction = 1.0, df, n = 1) {
    # Filter the specified input file, populating the specified data frame
    # with word or n-gram counts. Use randomly-selected lines in the input
    # file (with probability sample.fraction). All tokens are upper case.
    
    # Tokenize randomly selected lines, add tokens to data frames
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        rand <- rbinom(n = 1, size = 1, prob = sample.fraction)
        
        if (rand == 1) {
            tokens <- tokenize(line)
            
            for (i in 1:length(tokens)) {
                if (n == 1) {
                    df <- populate.word.df(src, tokens[i], df)
                } else {
                    populate.ngram.df(src, tokens, i, n, df)
                }
            }
        }
    }
    
    df
}

create.df <- function(first.col.name, srcs) {
    # Create a data frame with the given first column name,
    # plus a column for each source.
    
    all.colnames <- append(srcs, first.col.name, after = 0)
    df <- read.table(text = "", col.names = all.colnames, stringsAsFactors = FALSE)
    
    df
}

build.df <- function(srcs, sample.fraction = 1.0, dir = ".", locale = "en_US", df, n = 1) {
    # Populate the word or ngram data frames from files
    # for each of the sources specified.
    
    for (src in srcs) {
        filename <- paste(locale, src, "txt", sep = ".")
        path <- paste(dir, filename, sep = "/")
        print(paste("Reading", path))
        con <- file(path, "r")
        
        df <- populate.df(src, con, sample.fraction, df, n)
        
        close(con)
    }

    # Calculate total uses across all sources, order by total (descending)
    df$total <- rowSums(df[,2:length(df)])
    df <- df[order(-df$total), ]
    
    # Add cumulative total uses column
    df <- mutate(df, cumsum = cumsum(total))
    
    df
}

how.many.needed <- function(fraction, cumulative, round.up = TRUE) {
    # How many words do you need to capture some fraction of
    # all uses?
    total.tokens <- cumulative[length(cumulative)]
    most.tokens  <- total.tokens * fraction
    highest.needed.row <- min(which(cumulativ > most.tokens))
    
    highest.needed.row
}

run <- function(srcs = srcs, dir = ".", sample.fraction = 1.0, locale = "en_US") {
    # Initialize word and n-gram data frames
    word.df <- create.df("word", srcs)
    ngram.df <- create.df("word", srcs)
    
    # Build data frames
    word.df <- build.df(srcs, sample.fraction, dir, locale, word.df, 1)
#    ngram.df <- build.df(srcs, sample.fraction, dir, locale, ngram.df, 2)
    
    coverage.fractions = c(0.5, 0.9, 0.95, 0.99, 0.999)
    needed.words <- lapply(coverage.fractions, FUN = (how.many.needed), cumulative = word.df$cumsum)
    
    
    head(word.df)
    max.y.rowindex <- max(floor(length(word.df$word) / 100), 1)
    max.y <- word.df[max.y.rowindex, "total"]
    plot(word.df[,2], type = "l", ylim = c(0, max.y), col = 1)
    for (i in 3:length(word.df)) {
        lines(word.df[,i], col = i - 1)
    }
    
    total.tokens <- word.df[length(word.df$word), "cumsum"]
    most.tokens  <- total.tokens * 0.90
    highest.needed.row <- min(which(word.df$cumsum > most.tokens))
    print(paste("need:", highest.needed.row))
    word.df
    #    print(word.df)
    
#    barplot(table(word.df), names.arg = word.df$word)
#    plot(word.df$test ~ word.df$word)
    #    plot(ngram.df)
    
#    print(word.df)
#    print(ngram.df)
}

srcs <- c("test")

