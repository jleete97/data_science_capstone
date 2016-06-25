# Build a cleaned-up (tm) Corpus from text files.
#
# Author: Jon Leete
# 

library(tm)
library(NLP)
library(openNLP)
library(wordcloud)
library(chron)
library(SnowballC)
library(dplyr)
library(RColorBrewer)
library(hash)

# Default base directory for scanning text input.
base.dir <- "~/r/capstone/data"

# Build a clean tm Corpus from text inputs.
#
# dir.name: the name of the directory, relative to the base directory, to
#           scan for text files
# base.dir: the base directory
#
BuildCleanCorpus <- function(dir.name = "short", base.dir = base.dir) {
    require(tm)
    
    corpus <- ReadCorpus(dir.name, base.dir)
#    corpus <- SplitCorpusIntoSentences(corpus)
    corpus <- CleanCorpus(corpus)
    
    corpus
}

# Build a tm Corpus from all text files in the specified subdirectory of
# a standard base directory.
# 
# dir.name: the name of the directory, relative to the base directory, to
#           scan for text files
# base.dir: the base directory
#
ReadCorpus <- function(dir.name, base.dir = base.dir) {
    require(tm)
    
    dir      <- paste(c(base.dir, dir.name), collapse = "/")
    print(paste(c("Building corpus in ", dir, "at", Sys.time()), collapse = " "))
    
    src       <- DirSource(directory = dir, encoding = "UTF-8", mode = "text")
    corpus    <- VCorpus(x = src)
    
    corpus
}

# Clean a Corpus: lower-case, remove numbers, stopwords and punctuation, strip
# whitespace.
# 
# c: the Corpus to clean.
# 
CleanCorpus <- function(c) {
    require(tm)
    
    print(paste(c("Cleaning corpus at", Sys.time()), collapse = " "))
    c <- tm_map(c, content_transformer(tolower))
    print(paste(c("tolower done:", Sys.time()), collapse = " "))
    c <- tm_map(c, removeNumbers)
    print(paste(c("removeNumbers done:", Sys.time()), collapse = " "))
    c <- tm_map(c, removeWords, stopwords("english"))
    print(paste(c("stopwords done:", Sys.time()), collapse = " "))
    c <- tm_map(c, removePunctuation)  # reconsider if separating sentences
    print(paste(c("removePunctuation done:", Sys.time()), collapse = " "))
    #    c <- tm_map(c, stemDocument)
    #    print(paste(c("stemDocument done:", Sys.time()), collapse = " "))
    c <- tm_map(c, stripWhitespace)
    print(paste(c("stripWhitespace done:", Sys.time()), collapse = " "))
    
    c
}

# Split a Corpus into one document per sentence.
# 
# corpus: The Corpus to split into TextDocuments of one sentence each.
# 
SplitCorpusIntoSentences <- function(corpus) {
    print(paste(c("Splitting corpus into sentences at", Sys.time()), collapse = " "))
    ModifyCorpus(corpus, SplitTextIntoSentences)
}

# Apply a modification function to a Corpus. Returns the modified Corpus.
# 
# corpus: the Corpus to modify, with FUN.
# FUN: the function to use on the corpus.
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
    
    modified.corpus <- Corpus(VectorSource(text.docs))
    
    modified.corpus
}

# Split a TextDocument into sentences with OpenNLP sentence detection.
# 
# text: the TextDocument to split
# 
SplitTextIntoSentences <- function(text) {
    # With appreciation for Tony Breyal,
    # http://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences
    require(tm)
    require(NLP)
    require(openNLP)
    require(SnowballC)
    
    text <- as.String(text)
    sentence.boundaries <- annotate(text, Maxent_Sent_Token_Annotator(language = "en"))
    sentences <- text[sentence.boundaries]
    
    sentences
}

# Build a 100-word word cloud from a Corpus.
# 
WordCloud100 <- function(c, max.words=100) {
    require(wordcloud)
    require(RColorBrewer)
    
    wordcloud(c,
              scale=c(5,0.5),
              max.words=100,
              random.order=FALSE,
              rot.per=0.35,
              use.r.layout=FALSE,
              colors=brewer.pal(8, "Dark2"))
    
}
