##################################################
#
# Run in 3 steps:
#
# 1. build.tokens("<token-file-dir>")
#    - builds tokens from specified source(s)
#
# 2. model <- build.model()
#    - builds model from token files
#
# 3. predict.all(model, <sentences>)
#    - predicts subsequent words
#
##################################################

source("token-filter.R")
source("model-hash.R")

# Build a single tokenized data file from the specified inputs.
# source.dir  : the directory containing all the source files (*.txt)
# output.file : the combined, tokenized output file.
# sample.rate : the fraction of lines in the input file to tokenize and keep
# words.only  : recognize only words as tokens (not numbers, currency amounts)
# n           : the "n" in "n-gram" tokens
#
build.tokens <- function(source.dir, output.file = "tokens.txt", sample.rate = 1.0, words.only = TRUE) {
    tokenize.file(source.dir, output.file, sample.rate, words.only)
}

# Build a model (n-gram hash) from the specified file
# token.file : the name (path) of the token file
# n          : the number of grams to build the model with
build.model <- function(token.file = "tokens.txt", n = 3) {
    model <- hash()
    
    start.time <- Sys.time()
    con <- file(token.file, "r")
    add.file.to.model(con, model, n)
    close(con)
    file.end.time <- Sys.time()
    
    trim.model(model)
    end.time <- Sys.time()
    
    print(sprintf("Processing file: %1.3f sec, trimming: %1.3f sec.",
                  (file.end.time - start.time),
                  (end.time - file.end.time)))
    
    model
}

read.model <- function(model.file = "model.txt") {
    m <- read.model.file(model.file)
    
    m
}

# Predict the next word for each sentence in the vector sentences,
# given the model built above.
predict.all <- function(model, sentences) {
    
    for (s in sentences) {
        print(paste(s, ":"))
        
        tokens <- strsplit(toupper(s), " ")[[1]]
        next.word <- predict(model, tokens)
        
        print(paste("    ", next.word, "\n"))
    }
}
