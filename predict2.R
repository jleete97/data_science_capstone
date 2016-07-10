################################################################################
# 
# Predict next word based on a text model.
# 
# Plan:
# 
# - Read text sources from all files in a specified directory, into a (tm)
#   Corpus.
# - Clean up the Corpus, 

source('build-corpus.R')
source('build-model.R')

library(tm)
library(RWeka)

# Default directory.
if (!exists("data.dir")) { data.dir <- "full" }
# Default n-gram length.
if (!exists("gram.size")) { gram.size <- 3 }
gram.length <- gram.size

corpus <- BuildCleanCorpus(data.dir, "~/r/capstone/data", split = TRUE)
model <- BuildHashModel(corpus)

Predict(model, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
Predict(model, "You're the reason why I smile everyday. Can you follow me please? It would mean the")
Predict(model, "Hey sunshine, can you follow me and make me the")
Predict(model, "Very early observations on the Bills game: Offense still struggling but the")
Predict(model, "Go on a romantic date at the")
Predict(model, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
Predict(model, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
Predict(model, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
Predict(model, "Be grateful for the good times and keep the faith during the")
Predict(model, "If this isn't the cutest thing you've ever seen, then you must be")
