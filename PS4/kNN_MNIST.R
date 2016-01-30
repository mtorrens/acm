################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #4 (Part 3)
# Date   : 2016.01.30
################################################################################
# source('~/Desktop/bgse/courses/term2/acm/problemSets/PS4/kNN_MNIST.R')
################################################################################

# Package dependencies
library(class)

# Directory of the code
PATH <- '~/Desktop/bgse/courses/term2/acm/problemSets/PS4/'

# Display digits function
source(paste(PATH, '../../datasets/MNIST/displayDigit.R', sep = ''))

# MNIST dataset
file1 <- paste(PATH, '../../datasets/MNIST/MNIST_test.csv', sep = '')
file2 <- paste(PATH, '../../datasets/MNIST/MNIST_training.csv', sep = '')
mnistt <- read.csv(file = file1)
mniste <- read.csv(file = file2)

# Train K-NN function
cl <- mniste[, 1]
train <- mniste[, 2:ncol(mniste)]
test <- mnistt

# Test different values
if (FALSE) {
  # For different values of k
  preds1 <- knn(cl = cl, train = train, test = train, k = 1)
  preds3 <- knn(cl = cl, train = train, test = train, k = 3)
  preds5 <- knn(cl = cl, train = train, test = train, k = 5)
  preds7 <- knn(cl = cl, train = train, test = train, k = 7)
  preds9 <- knn(cl = cl, train = train, test = train, k = 7)

  # Checking results
  table(preds1, cl)
  table(preds3, cl)
  table(preds5, cl)
  table(preds7, cl)
  table(preds9, cl)
  mean(preds1 == cl)
  mean(preds3 == cl)
  mean(preds5 == cl)
  mean(preds7 == cl)
  mean(preds9 == cl)
}

# With these results makes little sense playing with non-Euclidean distances
# Three neighbors provides already excellent results on training sample
preds <- knn(cl = cl, train = train, test = test, k = 3)
#pred.mnist <- cbind(preds, mnistt)
#colnames(pred.mnist) <- c('pred_lab', colnames(mnistt))
#displayDigit(as.numeric(mnistt[1, ]), label = NA, predictedLabel = preds[1])
#displayDigitSeq(mnistt, rep(NA, nrow(mnistt)), preds)
preds <- as.data.frame(preds)
colnames(preds) <- 'pred_lab'

# Save predictions
file <- paste(PATH, 'MNIST_predictions.csv', sep = '')
write.csv(preds, file = file, row.names = FALSE)
cat('Written file:', file, '\n')
# END OF SCRIPT
