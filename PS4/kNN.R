################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #4 (Part 1)
# Date   : 2016.01.30
################################################################################
# source('~/Desktop/bgse/courses/term2/acm/problemSets/PS4/kNN.R')
################################################################################

################################################################################
kNN <- function(features, labels, k = 3, p = 2, action = 'train') {
# Miquel Torrens (c) 2016.01.30
# features : (matrix) object with feature values
# labels   : (character) vector with true labels
# k        : (integer) Number of nearest neighbors to consider
# p        : (double) (1)   Manhattan,
#                     (2)   Euclidean,
#                     ...
#                     (Inf) Chebyshev
# action   : (character)
################################################################################
  # Class checkings
  if (class(features) != 'matrix') {
    features <- try(as.data.frame(features))
    if (class(features) == 'try-error') {
      stop('object "features" not a "matrix" object and uncoercible.')
    }
  }

  if (class(labels) != 'character') {
    labels <- try(as.character(labels))
    if (class(labels) == 'try-error') {
      stop('object "labels" not a "character" object and uncoercible.')
    }
  }
  labels <- as.factor(labels)  # Factors are more efficient

  if (class(k) != 'integer') {
    k <- try(as.integer(k))
    if (class(k) == 'try-error') {
      stop('object "k" not an "integer" object and uncoercible.')
    }
  }

  if (p != Inf && class(p) != 'integer') {
    p <- try(as.integer(p))
    if (class(p) == 'try-error') {
      stop('object "p" not an "integer" object and uncoercible.')
    }
  }

  if (! action %in% c('train', 'test')) {
    stop('action must be "train" or "test"')
  }

  # Additional checks
  if (nrow(features) != length(labels)) {
    stop('feature matrix and labels must have identical length.')
  }

  # Check that k is odd (if two classes)
  if (length(levels(labels)) == 2 && (k %% 2) == 0) {
    stop('parameter "k" must be an odd integer when there are two categories.')
  }

  # Control for NAs in labels
  no.nas <- which(! is.na(labels))
  if (action == 'train') {
    features <- features[no.nas, ]
    labels <- labels[no.nas]
  }  
  
  # Calculate distances in the features
  nobs <- nrow(features)
  if (p != Inf) {
    dists <- as.matrix(dist(features, method = 'minkowski', p = p))
  } else {
    dists <- as.matrix(dist(features, method = 'maximum'))
  }
  #all(diag(dists) == 0)

  # Neighbor lists
  nb <- apply(dists, 1, order)
  
  # Predict labels
  preds <- rep(NA, length = nobs)
  prob <- rep(NA, length = nobs)
  aux <- lapply(1:nobs, function(x) {
    if (action == 'train') {
      counts <- table(labels[nb[2:(k + 1), x]])
    } else {
      # Kick out those without true labels
      chosen <- nb[which(nb[, x] %in% no.nas), x][2:(k + 1)]
      counts <- table(labels[chosen])
    }    
    winner <- which.max(counts)
    preds[x] <<- names(counts)[winner]
    prob[x] <<- counts[winner] / k  # Proportion of neighbors
  })
  
  # End
  return(list(predLabels = as.factor(preds), prob = prob))
}
# END OF SCRIPT
