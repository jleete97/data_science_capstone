library(tm)
library(wordcloud)
library(NLP)
library(openNLP)

cleanCorpus <- function(c) {
    start <- Sys.time()
    c <- tm_map(c, content_transformer(tolower))
    print(paste(c("tolower:", (Sys.time() - start))))
    c <- tm_map(c, removeNumbers)
    print(paste(c("+ removeNumbers:", (Sys.time() - start))))
    c <- tm_map(c, removeWords, stopwords("english"))
    print(paste(c("+ remove stopwords:", (Sys.time() - start))))
    c <- tm_map(c, removePunctuation)  # reconsider if separating sentences
    print(paste(c("removePunctuation:", (Sys.time() - start))))
    c <- tm_map(c, stripWhitespace)
    print(paste(c("stripWhitespace:", (Sys.time() - start))))
    
    c
}

wordCloud <- function(c, max.words=100) {
    wordcloud(c,
              scale=c(5,0.5),
              max.words=100,
              random.order=FALSE,
              rot.per=0.35,
              use.r.layout=FALSE,
              colors=brewer.pal(8, "Dark2"))
    
}

buildCorpus <- function(dir_name="short") {
    start <- Sys.time()
    base_dir <- "~/r/capstone/data"
    dir      <- paste(c(base_dir, dir_name), collapse="/")
    print(dir)
    
    src       <- DirSource(directory=dir, encoding="UTF-8", mode="text")
    corpus    <- VCorpus(x=src)
    
    print(paste(c("Building corpus:", (Sys.time() - start))))
    
    corpus
}

doAll <- function(dir_name="short") {
    corpus <- buildCorpus(dir_name)
    corpus <- cleanCorpus(corpus)
    
    #wordCloud(corpus)
    
    corpus
}

# Split text into sentences with OpenNLP sentence detection
splitTextIntoSentences <- function(text) {
    # With appreciation for Tony Breyal,
    # http://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences
    require(tm)
    require(NLP)
    require(openNLP)
    
    text <- as.String(text)
    sentence.boundaries <- annotate(text, Maxent_Sent_Token_Annotator(language = "en"))
    sentences <- text[sentence.boundaries]
    
    sentences
}

modifyCorpus <- function(corpus, FUN, ...) {
    # With appreciation for Tony Breyal,
    # http://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences
    require(tm)
    require(NLP)
    require(openNLP)
    
    text <- lapply(corpus, content)
    
    text.docs <- lapply(text, FUN, ...)
    text.docs <- as.vector(unlist(text.docs))
    
    split.corpus <- Corpus(VectorSource(text.docs))
    
    split.corpus
}


splitCorpusIntoSentences <- function(corpus) {
    modifyCorpus(corpus, splitTextIntoSentences)
}