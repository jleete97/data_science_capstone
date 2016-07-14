################################################################################
# 
# Predict next word based on a text model.
# 
# Plan:
# 
# - Read text sources from all files in a specified directory, into a (tm)
#   Corpus.
# - Clean up the Corpus, 

source('build-corpus2.R')
source('build-model2.R')

# Bump up heap space for RWeka
options(java.parameters = "-Xmx4g")

library(tm)
library(RWeka)
library(dplyr)

corpus <- BuildCleanCorpus(dir = "partial", base.dir = "~/r/capstone/data", split = TRUE)
model <- BuildMultiGramModel(corpus, min.count = 1)

PredictMultiGramDfs(model, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
PredictMultiGramDfs(model, "You're the reason why I smile everyday. Can you follow me please? It would mean the")
PredictMultiGramDfs(model, "Hey sunshine, can you follow me and make me the")
PredictMultiGramDfs(model, "Very early observations on the Bills game: Offense still struggling but the")
PredictMultiGramDfs(model, "Go on a romantic date at the")
PredictMultiGramDfs(model, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
PredictMultiGramDfs(model, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
PredictMultiGramDfs(model, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
PredictMultiGramDfs(model, "Be grateful for the good times and keep the faith during the")
PredictMultiGramDfs(model, "If this isn't the cutest thing you've ever seen, then you must be")
