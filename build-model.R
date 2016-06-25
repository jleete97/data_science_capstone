# Convert a "cleaned-up" (tm) Corpus into an n-gram text prediction model.
#
# Author: Jon Leete
# 

library(tm)
library(hash)

# Number of words to use in prediction model
gram.length = 3

# Segment a text vector. See SegmentDataFrame.
# 
SegmentTextVector <- function(text.vector, gram.length, min.count) {
    text.df <- as.data.frame(text.vector, stringsAsFactors = FALSE)
    names(text.df) <- c("terms")
    
    SegmentDataframe(text.df, gram.length, min.count)
}

# Split a dataframe with space-delimited, (n+1)-gram "terms" element into a
# dataframe with "firstn" and "last1" elements that correspond to the first
# n words and the most common last word.
# 
# text.df: The text dataframe, which must have a "terms" element.
# gram.length: The number of words to use in the final "firstn" element. The
#              input text.df$terms should have gram.length + 1 words in it.
# min.count: Drop all n-grams that occur fewer than this many times.
# 
SegmentDataframe <- function(text.df, gram.length, min.count) {
    require(dplyr)
    
    text.df <- text.df %>%
        rowwise() %>%
        mutate(firstn = firstn(terms, gram.length)) %>%
        mutate(last1 = last1(terms, gram.length)) %>%
        count(firstn, last1) %>%
        filter(n >= min.count) %>%
        group_by(firstn) %>%
        filter(n == max(n))
    
    text.df
}

# Build a predictive model, in the form of a hash, that maps n-grams to
# predicted words.
# 
# segmented.df: dataframe with elements "firstn," which has the n-gram
# in normalized form (lower case, single-space delimited), "last1" with the
# predicted word for the n-gram.
# 
BuildHashModel <- function(segmented.df) {
    require(hash)
    
    h <- hash()
    
    # segmented.df has elements firstn and last1.
    for (i in 1:length(segmented.df$firstn)) {
        h[[segmented.df$firstn[[i]]]] <- segmented.df$last1[[i]]
    }
    
    h
}

# Predict the next word after a sequence of words, with a hash model.
# 
# hash.model: a hash of noramlized n-grams (space-delimited and lower case)
# words: a text string (character vector)
# 
Predict <- function(hash.model, words) {
    require(hash)
    
    gram.size <- length(strsplit(ls(hash.model)[1], " ", fixed = TRUE)[[1]])
    input.gram <- NormalizeInput(words, gram.size)
    
    if (has.key(input.gram, hash.model)) {
        hash.model[input.gram]
    } else {
        ""
    }
}

# Normalize a text string to a string of words of the type to match a key in
# our hash model.
# 
# words: text string, with at least as many words as gram.size
# gram.size: number of words to keep (from the end of the string)
# 
NormalizeInput <- function(words, gram.size) {
    require(tm)
    
    words <- tolower(words)
    words <- removeWords(words, stopwords("en"))
    words <- removePunctuation(words)
    words <- gsub("^\\s+|\\s+$", "", words)
    words <- gsub("\\s+", " ", words)
    
    word.vec <- strsplit(words, "\\s+")[[1]]
    start.pos <- length(word.vec) - gram.size + 1
    word.vec <- word.vec[start.pos : length(word.vec)]
    
    model.key <- paste(word.vec, collapse = " ")
    
    model.key
}

firstn <- function(tri, gram.length) paste(strsplit(tri, " ", fixed = TRUE)[[1]][1:gram.length], collapse = " ")
last1  <- function(tri, gram.length) strsplit(tri, " ", fixed = TRUE)[[1]][gram.length + 1]
