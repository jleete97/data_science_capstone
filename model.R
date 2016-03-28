library(stringi)

# Beginning of Line indicator; placed at head of token string
bol <- "XXXXXX"

################### API ###################

# Display a JSON printout of the environment.
printenv <- function(envt, depth = 0) {
    indent <- stri_dup("  ", depth)
    
    for (x in ls(envt)) {
        val <- get(x, envir = envt)
        
        if (is.numeric(val) || is.character(val)) {
            if (val != 0) {
                print(paste(indent, x, " : ", val, sep = ""))
            }
        } else {
            print(paste(indent, x, " : {", sep = ""))
            printenv(get(x, envir = envt), depth = depth + 1)
            print(paste(indent, "}", sep = ""))
        }
    }
}

# Build up a "tree" (nested Environments aka hashmaps) of tokens and frequencies.
# This tree is used in predicting next tokens, given a string of tokens.
# The top level keys are final tokens.
# They point to nested nodes (Environments, hashmaps, whatever) that map
# previous tokens to further nested maps, as deeply as the tree was set up.
# The leaf nodes map keys (tokens) to frequencies / counts.
# So, given a series of tokens, walk it backwards from the root node down.
# The most common leaf value (mapping a token to a number, not another node)
# represents your prediction for the next token, given the series.
build.tree <- function(root, token.vector, max.depth = 3) {
    token.vector <- append(token.vector, bol, 0) # put BOL token at front
    
    for (i in 2:length(token.vector)) { # skip BOL
        token <- token.vector[i]
        add.to.tree(root, token.vector, i - 1, max.depth, token)
    }
    
    root
}

# Predict the next token, given a tree that incorporates our learned model
# and a series of tokens.
predict <- function(root, words) {
    words <- append(words, bol, 0) # put BOL at front
    
    word <- match(root, words)
    
    word
}

################# INTERNAL ################

# Find the most common next token for the series of tokens.
match <- function(node, words) {
    
    last.word <- words[length(words)]
    print(paste("last: '", last.word, "', of all: ", words, sep = ""))
    
    if (exists(last.word, envir = node, inherits = FALSE)) {
        next.value <- get(last.word, envir = node)
        
        if (is.numeric(next.value)) {
            last.word
        } else {
            remaining.words <- words[1:(length(words) -1)]
            match(next.value, remaining.words)
        }
    } else {
        most.common.in.tree(node)
    }
}

# Find the most common token in the arbitrarily-nested tree.
# Sums the leaf nodes into a map (Environment).
most.common.in.tree <- function(node, map = NULL) {
    
    if (is.null(map)) {
        map <- new.env()
    }
    # Convert to map token : integer (count)
    add.up.token.occurrences(node, map)
    
    # Find most common
    commonest.token <- most.common.in.map(map)
    
    commonest.token
}

# Find element in flat map (un-nested Environment) that maps to highest
# numeric value. This function assumes that the map is of this form.
# (A little deviation will probably work -- it tries to ignore things
# that don't fit.)
most.common.in.map <- function(map) {
    # Find most common
    commonest.token <- NULL
    highest <- 0
    
    for (token in ls(map)) {
        if ((token != bol) && exists(token, envir = map, inherits = FALSE)) {
            count <- get(token, envir = map)
            
            if (is.numeric(count) && (count > highest)) {
                highest <- count
                commonest.token <- token
            }
        }
    }
    
    commonest.token
}

# Sum up occurrences of all tokens in this subtree
add.up.token.occurrences <- function(node, map) {
    for (x in ls(node)) {
        
        print(paste("  key: ", x))
        val <- get(x, envir = node)
        
        if (is.numeric(val)) {
            print(paste("adding", val))
            if (!exists(x, envir = map, inherits = FALSE)) {
                assign(x, val, envir = map)
            } else {
                count <- get(x, envir = map)
                print(paste("    to", count))
                count <- count + val
                assign(x, count, envir = map)
            }
        } else {
            print("---- recursing...")
            add.up.token.occurrences(val, map)
        }
    }
}

# Part of the model learning phase: adds a sequence of tokens to
# the tree, to a maximum specified depth (len).
# 
add.to.tree <- function(pod, token.vector, index, len, token) {
    
    if (index > 0 && len > 0) {
        #todo allow stopwords only as immediate prececessors
        print(paste("index = ", index, ", len = ", len, sep=""))
        child <- ensure.child(pod, token.vector[index])
        add.to.tree(child, token.vector, index - 1, len - 1, token)
    }
    else {
        count <- 0
        if (exists(token, envir = pod, inherits = FALSE) && is.numeric(get(token, envir = pod))) {
            count <- get(token, envir = pod)
        }
        count <- count + 1
        assign(token, count, envir = pod)
    }
}

# Ensure that the specified node (Environment) has a child node
# with the specified name. If not, a new Environment is created.
ensure.child <- function(pod, name) {
    print(paste("checking for ", name, sep = ""))
    if (!exists(name, envir = pod, inherits = FALSE)) {
        print(paste("creating", name))
        # Create, add new child
        child <- new.env()
        assign(name, child, envir = pod)
    }
    
    get(name, envir = pod)
}
