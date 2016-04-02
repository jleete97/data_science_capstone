##################################################
#
# Run in 3 steps:
#
# 1. tokenize()
#    - builds tokens from specified source(s)
#
# 2. build.model()
#    - builds model from token files
#
# 3. predict.all(<sentences>)
#    - predicts subsequent words
#
##################################################

source("token-filter.R")
source("model.R")

tokenize <- function() {
    remove.token.files(token.dir)
#    filter(c("test", "test1", "test2"), sample.fraction = 1.0, n = 3)
#    filter(c("blogtest", "newstest", "twittertest"), sample.fraction = 1.0, n = 3)
    filter(c("blogs", "news", "twitter"), sample.fraction = 0.1)
}

add.to.model <- function(con, model.tree, depth = 3) {
    while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        tokens <- strsplit(line, " ")[[1]]
        build.tree(model.tree, tokens, depth)
    }
}

build.model <- function(token.path, depth = 3) {
    token.files <- list.files(path = token.path, pattern = "\\.txt$")
    
    model <- new.env()
    
    for (src in token.files) {
        src.path <- paste(token.path, src, sep = "/")
        
        con <- file(src.path, "r")
        add.to.model(con, model, depth = depth)
        close(con)
    }
    
    model
}

predict.all <- function(token.dir = "tokens", sentences = c("Take me to the", "I want a", "Help")) {
    token.path <- paste(base.dir, token.dir, sep = "/")
    
    model <- build.model(token.path)
    
    for (s in sentences) {
        print(paste(s, ":"))
        
        tokens <- strsplit(toupper(s), " ")[[1]]
        next.word <- predict(model, tokens)
        
        print(paste("    ", next.word, "\n"))
    }
}
