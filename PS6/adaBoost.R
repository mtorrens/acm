################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #5 (Part 1)
# Date   : 2016.02.21
################################################################################
# source('~/Desktop/bgse/courses/term2/acm/problemSets/PS5/cTree.R')
################################################################################

################################################################################
adaBoost <- function(formula, data, test = NULL, depth, noTrees, ...) {
# formula   (formula): formula for the regression tree
# data   (data.frame): data frame with the features
# test   (data.frame): data frame with the test set
# depth     (numeric): depth of the tree
# noTrees   (numeric): number of trees adaboost should run
# ...                : other arguments for "rpart"
################################################################################
  ##############################################################################
  # Asserting arguments
  # Required packages
  if (! require(rpart)) {
    stop('required package not install: "rpart".')
  }

  # Check formula
  if (class(formula) != 'formula') {
    stop('"formula" argument is not of class formula.')
  }  

  # Check data
  if (! class(data) %in% c('matrix', 'data.frame')) {
    stop('"data" argument is not of class matrix or data.frame.')
  }  

  # Check test
  if (! is.null(test)) {
    if (! class(test) %in% c('matrix', 'data.frame')) {
      stop('"test" argument is not of class matrix or data.frame.')
    }      
  }

  # Check depth
  if (! is.numeric(depth) || length(depth) != 1) {
    stop('"depth" argument is not numeric.')
  } else {
    depth <- as.integer(depth)
  }

  # Check number of trees
  if (! is.numeric(noTrees) || length(noTrees) != 1) {
    stop('"depth" argument is not numeric.')
  } else {
    noTrees <- as.integer(noTrees)
  }
  ##############################################################################

  ##############################################################################
  # Train
  # Select the variables from the data.frame
  vars <- all.vars(formula)
  if (vars[2] == '.') {
    vars <- c(vars[1], colnames(data)[colnames(data) != vars[1]])
  }
  ys <- vars[1]
  xs <- vars[2:length(vars)]
  data <- data[, vars]
  nf <- as.formula(paste(ys, paste(xs, collapse = ' + '), sep = ' ~ '))

  # Factorize the predicted variable
  data[, 1] <- as.factor(as.numeric(data[, 1]))
  data[, 1] <- as.factor(ifelse(data[, 1] == 1, 1, -1))

  # Initialize objects to store results
  n <- nrow(data) # Number of observations
  w <- rep(1 / n, n)  # Initial weights
  M <- noTrees
  m <- 1
  alphas <- rep(NA, length = M)
  pm <- as.data.frame(matrix(ncol = M, nrow = n))

  trees <- vector(mode = 'list', length = M)

  # Loop to perform adaboost
  repeat {
    # Calculate classifier
    tree <- rpart(formula = nf, data = data, weights = w,
                  control = rpart.control(maxdepth = depth))#, ...)
    preds <- predict(tree, data, type = 'class')

    # Collect predictions
    indicators <- as.numeric(data[, 1] != preds)
    errors <- sum(w[which(indicators == 1)]) / sum(w)
    alpha <- log((1 - errors) / errors)

    # Update new results
    w <- w * exp(alpha * indicators)
    alphas[m] <- alpha
    pm[, m] <- as.numeric(as.character(preds))
    trees[[m]] <- tree

    # Next iteration
    m <- m + 1
    if (m == M + 1) {
      break
    }
  }
  ##############################################################################

  ##############################################################################
  # Test
  if (! is.null(test)) {
    n <- nrow(test) # Number of observations
    tm <- as.data.frame(matrix(ncol = M, nrow = nrow(test)))
    for (m in 1:M) {
      preds <- predict(trees[[m]], test, type = 'class')
      tm[, m] <- as.numeric(as.character(preds))
    }
    # Final prediction of adaboost algorithm
    out <- sign(as.matrix(tm) %*% alphas)
  } else {
    # Final result of adaboost algorithm
    out <- sign(as.matrix(pm) %*% alphas)
  }
  ##############################################################################

  # End
  return(list(predLabels = out))
}
# END OF SCRIPT
