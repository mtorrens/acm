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

# Newly minted kNN
source(paste(PATH, 'kNN.R', sep = ''))

# MNIST dataset
file1 <- paste(PATH, '../../datasets/MNIST/MNIST_test.csv', sep = '')
file2 <- paste(PATH, '../../datasets/MNIST/MNIST_training.csv', sep = '')
mnistt <- read.csv(file = file1)
mniste <- read.csv(file = file2)

# Train K-NN function
cl <- mniste[, 1]
train <- mniste[, 2:ncol(mniste)]
test <- mnistt

################################################################################
test <- FALSE  # if TRUE runs all time-consuming kNNs
################################################################################

# Test different values
if (test == TRUE) {
  # For different values of k
  preds1 <- knn(cl = cl, train = train, test = train, k = 1)
  preds3 <- knn(cl = cl, train = train, test = train, k = 3)
  preds5 <- knn(cl = cl, train = train, test = train, k = 5)
  preds7 <- knn(cl = cl, train = train, test = train, k = 7)
  preds9 <- knn(cl = cl, train = train, test = train, k = 9)

  # For different values of p
  predsm1 <- kNN(features = train, labels = cl, k = 3, p = 1)
  predsm2 <- kNN(features = train, labels = cl, k = 3, p = 2)
  predsm3 <- kNN(features = train, labels = cl, k = 3, p = 3)
  predsm4 <- kNN(features = train, labels = cl, k = 3, p = 4)
  predsm5 <- kNN(features = train, labels = cl, k = 3, p = 5)
  predsmI <- kNN(features = train, labels = cl, k = 3, p = Inf)

  # Checking results
  table(preds1, cl)
  table(preds3, cl)
  table(preds5, cl)
  table(preds7, cl)
  table(preds9, cl)
  table(predsm1[[1]], cl)
  table(predsm2[[1]], cl)
  table(predsm3[[1]], cl)
  table(predsm4[[1]], cl)
  table(predsm5[[1]], cl)
  table(predsmI[[1]], cl)
  mean(preds1 == cl)
  mean(preds3 == cl)
  mean(preds5 == cl)
  mean(preds7 == cl)
  mean(preds9 == cl)
  mean(predsm1[[1]] == cl)
  mean(predsm2[[1]] == cl)
  mean(predsm3[[1]] == cl)
  mean(predsm4[[1]] == cl)
  mean(predsm5[[1]] == cl)
  mean(predsmI[[1]] == cl)
}

# 3-NN with Euclidean provides already good results in-sample
# Type of distance really makes little difference within reasonable range of "p"
# Try a bit of CV
set.seed(666)
s <- sample(1:nrow(train), floor(0.8 * nrow(train)))
n <- (1:nrow(train))[which(! (1:nrow(train) %in% s))]
preds3 <- knn(cl = cl[s], train = train[s, ], test = train[n, ], k = 3)
preds5 <- knn(cl = cl[s], train = train[s, ], test = train[n, ], k = 5)

# Out of sample performs good as well
table(cl[n], preds3)
table(cl[n], preds5)
mean(cl[n] == preds3)
mean(cl[n] == preds5)

# We choose 3-NN with Euclidean distance as it is good enough
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
