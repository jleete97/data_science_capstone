# Build tm Corpus from text files, analyse
#
# Author: Jon Leete
# 

library(tm)
library(NLP)
library(openNLP)
library(wordcloud)

# Build a clean tm Corpus from text inputs
# 
BuildCleanCorpus <- function(dir.name = "short") {
    
    corpus <- ReadCorpus(dir.name)
    corpus <- SplitCorpusIntoSentences(corpus)
    corpus <- CleanCorpus(corpus)
    
    corpus
}

# Build a tm Corpus from all text files in the specified subdirectory of
# a standard base directory.
# 
ReadCorpus <- function(dir.name) {
    require(tm)
    
    base.dir <- "~/r/capstone/data"
    dir      <- paste(c(base.dir, dir.name), collapse = "/")
    print(paste(c("Building corpus in ", dir, "at", Sys.time())))
    
    src       <- DirSource(directory = dir, encoding = "UTF-8", mode = "text")
    corpus    <- VCorpus(x = src)
    
    corpus
}

# Split text into sentences with OpenNLP sentence detection
# 
SplitTextIntoSentences <- function(text) {
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

# Apply a modification function to a Corpus
# 
ModifyCorpus <- function(corpus, FUN, ...) {
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

# Split a Corpus into one document per sentence
# 
SplitCorpusIntoSentences <- function(corpus) {
    print(paste(c("Splitting corpus into sentences at", Sys.time())))
    ModifyCorpus(corpus, SplitTextIntoSentences)
}

# Clean a Corpus: lower-case, remove numbers, stopwords and punctuation, strip
# whitespace.
# 
CleanCorpus <- function(c) {
    
    print(paste(c("Cleaning corpus at", Sys.time())))
    c <- tm_map(c, content_transformer(tolower))
    print(paste(c("tolower done:", Sys.time())))
    c <- tm_map(c, removeNumbers)
    print(paste(c("removeNumbers done:", Sys.time())))
    c <- tm_map(c, removeWords, stopwords("english"))
    print(paste(c("stopwords done:", Sys.time())))
    c <- tm_map(c, removePunctuation)  # reconsider if separating sentences
    print(paste(c("removePunctuation done:", Sys.time())))
    c <- tm_map(c, stripWhitespace)
    print(paste(c("stripWhitespace done:", Sys.time())))
    
    c
}

# Build a 100-word word cloud from a Corpus.
# 
WordCloud100 <- function(c, max.words=100) {
    require(wordcloud)
    
    wordcloud(c,
              scale=c(5,0.5),
              max.words=100,
              random.order=FALSE,
              rot.per=0.35,
              use.r.layout=FALSE,
              colors=brewer.pal(8, "Dark2"))
    
}

