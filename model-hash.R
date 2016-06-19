library(stringi)
library(hash)
library(tm)


# Predict the next token, given a tree that incorporates our learned model
# and a series of tokens.
predict <- function(hash, words, n = length(words)) {
    word <- match(hash, words, n)
    
    word
}

# Trim the model down to the most common next token for each n-gram.
trim.model <- function(hash) {
    for (key in keys(hash)) {
        submap <- hash[[key]]
        
        if (is.hash(submap)) {
            hash[[key]] <- most.common(submap)
        }
    }
}

# Print the model (hash).
print.model <- function(h) {
    for (x in keys(h)) {
        
        s <- paste(x, "->")
        
        val <- h[[x]]
        
        if (is.hash(val)) {
            first <- TRUE
            s <- paste(s, " { ", sep = "")
            
            for (y in keys(val)) {
                if (!first) {
                    s <- paste(s, ", ", sep = "")
                }
                s <- paste(s, y, ": ", val[[y]], sep = "")
                first <- FALSE
            }
            
            s <- paste(s, "}")
        } else {
            s <- paste(s, val)
        }
        
        print(s)
    }
}

# Find the most common next token for the series of tokens.
match <- function(hash, words, n) {
    
    result <- ""
    
    for (num.tokens in seq(n, 1, by = -1)) {
        last.words <- words[(length(words) - num.tokens + 1) : length(words)]
        key <- paste(last.words, collapse = " ")
        
        if (has.key(key, hash)) {
            value <- hash[[key]]
            
            if (is.hash(value)) {
                # Haven't trimmed model yet
                result <- most.common(value)
            } else {
                # Model trimmed, most common value here
                result <- hash[[key]]
            }
            break
        }
    }
    
    result
}

# Find the key with the highest value in the map.
# (All values should be numeric.)
most.common <- function(hash) {
    
    result <- ""
    highest <- 0
    
    for (x in keys(hash)) {
        count <- hash[[x]]
        
        if (count > highest) {
            highest <- count
            result <- x
        }
    }
    
    result
}

# Part of the model learning phase: adds a sequence of tokens to
# the tree, to a maximum specified depth (len).
# hash  : the map we're building
# key   : n-gram of space-delimited tokens (e.g., "ON TO THE")
# value : the next token/word (e.g., "ROOF")
add.to.map <- function(hash, key, value) {
    
    # Add a new submap for the given key, if needed.
    submap <- hash[[key]]
    
    if (is.null(submap)) {
        submap <- hash()
        hash[[key]] <- submap
    }
    
    # Initialize or increment the counter for the word in the submap.
    count <- submap[[value]]
    
    if (!is.null(count)) {
        submap[[value]] <- count + 1
    } else {
        submap[[value]] <- 1
    }
}

# Build up a single-level-nested hash of n-grams and subsequent word frequencies.
# This function adds one line's worth of data to the hash.
add.line.to.map <- function(hash, token.vector, n = 3) {
    if (is.null(token.vector) || length(token.vector) == 0) {
        return
    }
    
    # Remove stopwords
    # This is a simplified handler: just removing stopwords
    # isn't the best prediction model, but it'll do for our
    # initial cases.
    trimmed.token.vector <- subset(token.vector, sapply(token.vector, not.stopword, simplify = "array"))
    if (length(trimmed.token.vector) == 0) {
        return
    }
    n <- min(n, length(token.vector))
    
    # Add first token with blank key
    tryCatch({
        add.to.map(hash, " ", trimmed.token.vector[[1]])
    }, warning = function(w) {
        print(paste("warning: unable to add ", token.vector, ", trimmed =", trimmed.token.vector, w))
        return
    }, error = function(e) {
        print(paste("error: unable to add ", token.vector, ", trimmed =", trimmed.token.vector, e))
        return
    })
    
    # Add remaining elements as n-grams, where 1 <= n <= (function parameter n)
    if (length(trimmed.token.vector) > 1) {
        for (token.index in 2:length(trimmed.token.vector)) {
            last.gram.index <- token.index - 1
            
            for (start.index in max(token.index - n, 1) : last.gram.index) {
                key <- paste(trimmed.token.vector[start.index : last.gram.index], collapse = " ")
                add.to.map(hash, key, trimmed.token.vector[token.index])
            }
        }
    }
}

stop.words <- toupper(stopwords())

not.stopword <- function(word) {
    !(word %in% stop.words)
}

# Add a tokenized file to the model (hash) we're building.
add.file.to.model <- function(con, hash, n = 3) {
    while (length(lines <- readLines(con, n = 1000, warn = FALSE)) > 0) {
        
        for (i in 1:length(lines)) {
            tokens <- strsplit(lines[[i]], " ")[[1]]
            add.line.to.map(hash, tokens, n)
        }
    }
}

# Read a (trimmed) model from a file.
read.model.file <- function(model.file) {
    model <- hash()
    start.time <- Sys.time()
    
    con <- file(model.file, "r")
    while (length(lines <- readLines(con, n = 1000, warn = FALSE)) > 0) {
        for (i in 1:length(lines)) {
            parts <- strsplit(lines[[i]], " -> ")[[1]]
            if (length(parts) == 2) {
                key <- parts[[1]]
                val <- parts[[2]]
                model[[key]] <- val
            }
        }
    }
    close(con)
    
    end.time <- Sys.time()
    print(sprintf("Building model from file: %1.3f sec.",
                  (end.time - start.time)))
    
    model
}