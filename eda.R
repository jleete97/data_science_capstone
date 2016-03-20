
# Setup
library(tm)
set.seed(314159)

freqs <- function(filename) {
    data <- read.csv(filename, sep = " ", header = FALSE, quote = "", comment.char = "", stringsAsFactors = FALSE)
    
    data
}
