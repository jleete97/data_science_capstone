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
library(dplyr)

corpus <- BuildCleanCorpus(dir = "short", base.dir = "~/r/capstone/data", split = TRUE)
model <- BuildMultiGramModel(corpus, min.count = 1)

PredictMultiGram(model, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
PredictMultiGram(model, "You're the reason why I smile everyday. Can you follow me please? It would mean the")
PredictMultiGram(model, "Hey sunshine, can you follow me and make me the")
PredictMultiGram(model, "Very early observations on the Bills game: Offense still struggling but the")
PredictMultiGram(model, "Go on a romantic date at the")
PredictMultiGram(model, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
PredictMultiGram(model, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
PredictMultiGram(model, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
PredictMultiGram(model, "Be grateful for the good times and keep the faith during the")
PredictMultiGram(model, "If this isn't the cutest thing you've ever seen, then you must be")
