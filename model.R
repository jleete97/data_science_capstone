library(stringi)

bol <- "XXXXXX"
cnt <- "_count"

################### API ###################

printenv <- function(envt, depth = 0) {
    indent <- stri_dup("  ", depth)
    
    for (x in ls(envt)) {
        val <- get(x, envir = envt)
        
        if (is.numeric(val) || is.character(val)) {
            if (! (x == cnt && val == 0)) {
                print(paste(indent, x, " : ", val, sep = ""))
            }
        } else {
            print(paste(indent, x, " : {", sep = ""))
            printenv(get(x, envir = envt), depth = depth + 1)
            print(paste(indent, "}", sep = ""))
        }
    }
}

build.tree <- function(root, token.vector, max.depth = 3) {
    token.vector <- append(token.vector, bol, 0) # put BOL token at front
    
    for (i in 2:length(token.vector)) { # skip BOL
        token <- token.vector[i]
        add.to.tree(root, token.vector, i - 1, max.depth, token)
    }
    
    root
}

predict <- function(root, words) {
    words <- append(words, bol, 0) # put BOL at front
    
    word <- match(root, words)
    
    word
}

match <- function(node, words) {
    
    last.word <- words[length(words)]
    
    if (exists(last.word, envir = node, inherits = FALSE)) {
        next.value <- get(last.word, envir = node)
        
        if (is.numeric(next.value)) {
            last.word
        } else {
            remaining.words <- words[1:(length(words) -1)]
            match(next.value, remaining.words)
        }
    } else {
        most.common(node)
    }
}

################# INTERNAL ################

most.common <- function(node, map = NULL) {
    
    if (is.null(map)) {
        map <- new.env()
    }
    # Convert to map token : integer (count)
    add.up.token.occurrences(node, map)
    
    # Find most common
    commonest.token <- NULL
    highest <- 0
    
    for (token in ls(node)) {
        if (token == cnt || x == bol) next
        
        count <- get(token, envir = map)
        if (count > highest) {
            highest <- count
            commonest.token <- token
        }
    }
    
    commonest.token
}

# Sum up occurrences of all tokens in this subtree
add.up.token.occurrences <- function(node, map) {
    for (x in ls(node)) {
        if (x == cnt || x == bol) next
        
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

ensure.child <- function(pod, name) {
    print(paste("checking for ", name, sep=""))
    if (!exists(name, envir = pod, inherits = FALSE)) {
        print(paste("creating", name))
        # Create, add new child
        child <- new.env()
        assign(cnt, 0, envir = pod)
        assign(name, child, envir = pod)
    }
    
    get(name, envir = pod)
}

add.up <- function(envt) {
    total <- 0
    
    for (x in ls(envt)) {
        if (is.token.child(x)) {
            total <- total + add.up(get(x, envir = envt))
        }
    }
    
    if (total == 0) {
        total <- get(cnd, envir = envt)
    }
    
    total
}

is.token.child <- function(name) {
    !is.null(name) && (name != bol) && (name != cnt)
}
