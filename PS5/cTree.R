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
# :: Internal function of cTree
div.set <- function(data, col.idx, val) {
# Splitting the set according to some rule
# (Internal function of cTree)
################################################################################
  set1 <- data[which(data[, col.idx] <= val), ]
  set2 <- data[which(data[, col.idx] > val), ]
  return(list(set1 = set1, set2 = set2))
}

################################################################################
# :: Internal function of cTree
counts <- function(data, col.idx) {
# Counting labels
################################################################################
  tt <- table(data[, col.idx]) / nrow(data)
  return(list(vals = names(tt), props = unname(tt)))
}

################################################################################
# :: Internal function of cTree
cost <- function(data, col.idx, fun) {
# Function to calculate the cost depending on loss function
################################################################################
  res <- counts(data, col.idx)
  props <- res$props
  if (fun == 'ME') {
    err <- 1 - max(props)
  } else if (fun == 'Gini') {
    err <- sum(props * (1 - props))
  } else if (fun == 'Entropy') {
    err <- -sum(props * log(props))
  }
  return(err)
}

################################################################################
train.tree <- function(formula, data, depth, minPoints, costFnc,
                       parent = 0, set = 0) {
# formula   (formula): formula for the regression tree
# data   (data.frame): data frame with the features
# depth     (numeric): depth of the tree
# minPoints (numeric): minimum number of points in a region
# costFnc (character): cost function used to grow the tree
# parent    (numeric): parent node (DO NOT CHANGE: used in recursion)
# set       (numeric): index of set in split (DO NOT CHANGE: recursion)
################################################################################
  ##############################################################################
  # Stop recursion
  if (nrow(data) <= minPoints || depth <= 1) {
    # Record an empty node
    members <- paste(rownames(data), collapse = ',')
    chosen <- which(sapply(bcrit, `[`, 4) == parent)
    best.crit <- data.frame(col = NA,
                            cut.point = NA,
                            parent = parent,
                            node = NA,
                            depth = depth,
                            members = members)
    best.crit[, 'node'] <- length(bcrit) + 1
    bcrit[[length(bcrit) + 1]] <<- best.crit

    # End
    return(NULL)
  }

  # Starting node
  if (is.null(parent)) { parent <- 0 }
  ##############################################################################  

  ##############################################################################
  # Define sets
  vars <- all.vars(formula)
  data <- data[, c(which(colnames(data) != vars[1]),
                   which(colnames(data) == vars[1]))]
  y.idx <- which(colnames(data) == vars[1])
  if (costFnc == 'Entropy') {
    data[, y.idx] <- as.character(data[, y.idx])
  }

  # Starting score
  score <- cost(data = data, col.idx = y.idx, fun = costFnc)

  # Tracking variables
  best.gain <- 0L
  best.crit <- NA
  best.sets <- NA

  # Number of features and observations
  nfeats <- (1:ncol(data))[1:ncol(data) != y.idx]
  nobs <- nrow(data)
  members <- paste(rownames(data), collapse = ',')
  ##############################################################################

  ##############################################################################
  # Loop to find the best observation and feature
  for (j in nfeats) {
    # Order by the column
    new.data <- data[order(data[, j]), ]
    
    # Loop over the rows to search the best split
    for (i in minPoints:(nobs - minPoints)) {
      cut.point <- mean(new.data[i, j], new.data[i + 1, j])
      half.data <- div.set(new.data, j, cut.point)
      set1 <- half.data[[1]]
      set2 <- half.data[[2]]

      # Gain
      rem.obs <- nrow(set1) / nobs
      gain <- score - rem.obs * cost(set1, y.idx, costFnc) +
              (1 - rem.obs) * cost(set2, y.idx, costFnc) 

      # Check if we made progress
      if (gain >= best.gain &&
          nrow(set1) >= minPoints &&
          nrow(set2) >= minPoints) {
        best.gain <- gain
        best.crit <- data.frame(col = j,
                                cut.point = cut.point,
                                parent = parent,
                                node = 1,
                                depth = depth,
                                members = members)
        best.sets <- half.data
      }
    }
  }

  # If we did not make any progress
  if (is.na(best.crit[1])) {
    best.crit <- data.frame(col = NA,
                            cut.point = NA,
                            parent = parent,
                            node = 1,
                            depth = depth,
                            members = members)
  }
  ##############################################################################

  ##############################################################################
  # Accumulate results
  if (! 'bcrit' %in% ls(envir = .GlobalEnv)) {
    bcrit <<- vector(mode = 'list', length = 1)
    bcrit[[1]] <<- best.crit
  } else {
    chosen <- which(sapply(bcrit, `[`, 4) == parent)
    best.crit[, 'node'] <- length(bcrit) + 1
    bcrit[[length(bcrit) + 1]] <<- best.crit
  }

  # End
  if (is.na(best.crit[1])) {
    return(NULL)
  }
  ##############################################################################

  ##############################################################################
  # Recursion
  ndepth <- depth - 1
  ns1 <- train.tree(formula, data = best.sets[[1]], ndepth, minPoints, costFnc,
                    parent = best.crit[, 'node'], set = 1)
  ns2 <- train.tree(formula, data = best.sets[[2]], ndepth, minPoints, costFnc,
                    parent = best.crit[, 'node'], set = 2)
  ##############################################################################
}

################################################################################
cTree <- function(formula, data, test = NULL, depth, minPoints = 1,
                  costFnc = 'ME') {
# formula   (formula): formula for the regression tree
# data   (data.frame): data frame with the features
# test   (data.frame): data frame with the test set
# depth     (numeric): depth of the tree
# minPoints (numeric): minimum number of points in a region
# costFnc (character): cost function used to grow the tree
################################################################################
  ##############################################################################
  # Assertions
  # Packages
  if (! require(data.table)) {
    stop('required package not installed: "data.table".')
  }

  # Check depth
  if (! is.numeric(depth)) {
    stop('"depth" argument is not numeric.')
  } else {
    depth <- as.integer(depth)
  }

  # Check formula
  if (class(formula) != 'formula') {
    stop('"formula" argument is not of class formula.')
  }

  # Assert value of cost function parameter
  if (! costFnc %in% c('ME', 'Gini', 'Entropy')) {
    stop('unknown cost function. Choose among: "ME", "Gini", "Entropy".')
  }
  ##############################################################################

  ##############################################################################
  # Select the relevant features
  vars <- all.vars(formula)
  data <- data[, c(which(colnames(data) != vars[1]),
                   which(colnames(data) == vars[1]))]
  y.idx <- which(colnames(data) == vars[1])
  rownames(data) <- NULL
  ##############################################################################

  ##############################################################################
  # We work on global environment to make sure the recursion works well
  if ('bcrit' %in% ls(envir = .GlobalEnv)) {
    rm(bcrit, envir = .GlobalEnv)
  }

  # Grow the tree
  train.tree(formula, data, depth, minPoints, costFnc)

  # Get the tree from global environment
  tree <- as.data.frame(rbindlist(get('bcrit', envir = .GlobalEnv)))
  rm(bcrit, envir = .GlobalEnv)

  # Change depth scale (just for beauty)
  aux <- cbind(1:max(depth), max(depth):1)
  tree[, 'depth'] <- aux[match(tree[, 'depth'], aux[, 1]), 2]
  ##############################################################################

  ##############################################################################
  # Assign labels to the training data
  train.labs <- tree[which(is.na(tree[, 'col'])), c('node', 'members')]
  train.labs[, 'members'] <- as.character(train.labs[, 'members'])
  train.labs[, 'pred_label'] <- NA
  train.labs[, 'prob'] <- NA

  # Calculate values for each node
  for (row in 1:nrow(train.labs)) {
    who <- as.numeric(strsplit(train.labs[row, 2], ',')[[1]])
    props <- table(data[who, y.idx])
    label <- names(which.max(props))
    prob <- props[which.max(props)] / sum(props)
    train.labs[row, 'pred_label'] <- label
    train.labs[row, 'prob'] <- prob
  }
  ##############################################################################

  ##############################################################################
  # Assign labels to the test data
  if (is.null(test)) {  # We want the predicted labels on training set
    preds <- c()
    for (row in 1:nrow(train.labs)) {
      who <- as.numeric(strsplit(train.labs[row, 2], ',')[[1]])
      part <- cbind.data.frame(who,
                               train.labs[row, 'pred_label'],
                               train.labs[row, 'prob'])
      preds <- rbind.data.frame(preds, part)
    }
    preds <- preds[order(preds[, 1]), ]
    labs <- as.character(preds[, 2])
    prob <- preds[, 3]
  } else {
    # Set shape
    test <- test[, which(colnames(test) != vars[1])]

    # We want to predict the test set
    nodes <- rep(0, nrow(test))
    for (row in 1:nrow(test)) {
      parent <- 1
      col <- tree[which(tree[, 'parent'] == 0), 'col']
      val <- tree[which(tree[, 'parent'] == 0), 'cut.point']
      direc <- test[row, col] <= val
      while (TRUE) {
        # See if the node has children
        children <- tree[which(tree[, 'parent'] == parent), 'node']
        
        # If it does not it means we hit the bottom of the tree
        if (length(children) == 0) {
          nodes[row] <- node
          break
        }

        # Decide to which node we jump next
        node <- ifelse(direc, children[1], children[2])
        col <- tree[which(tree[, 'node'] == node), 'col']
        val <- tree[which(tree[, 'node'] == node), 'cut.point']
        direc <- data[row, col] <= val
        
        # Update the new parent and restart loop to go deeper
        parent <- node      
      }
    }

    # Match the trained results
    m <- match(nodes, train.labs[, 'node'])
    labs <- as.character(train.labs[m, 'pred_label'])
    prob <- train.labs[m, 'prob']
  }
  ##############################################################################

  # End
  return(list(predLabels = as.factor(labs), prob = prob, tree = tree))
}
# END OF SCRIPT
