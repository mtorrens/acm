################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #1 (Part 1)
# Date   : 2016.01.11
################################################################################
# source('~/Desktop/bgse/projects/github/acm/PS1/genData.R')
################################################################################

# Set working directory
if (FALSE) {
  setwd('~/Desktop/bgse/projects/github/acm/PS1/')
}

################################################################################
# Inspired by function "mlbench.circle" from package "mlbench"
# URL: https://cran.r-project.org/web/packages/mlbench/mlbench.pdf
gen.data <- function(n, features = 2, seed = NA, mus = NULL, sigma = NULL,
                     store = TRUE, pic = TRUE) {
# (c) Miquel Torrens, 2016.01.11
# n : number of observation for the dataset
# features : number of features generated
# seed : preferred seed
# mus : means of the features
# sigma : variance-covariance matrix of the features specified
# store : TRUE if we want to save the dataset (in current working directory)
# pic : TRUE if we want to save the plot (in current working directory)
################################################################################
  # Libraries
  if (! require(mvtnorm)) { stop('required package not installed: mvtnorm') }
  if (! require(ggplot2)) { stop('required package not installed: ggplot2') }

  # For simplicity we restrict to 2 or 3 dimensions (this can be relaxed)
  if (! as.numeric(features) %in% 2:3) {
    stop('argument "features" must be 2 or 3.')
  }

  # Default values
  if (! is.na(seed)) { set.seed(seed) }
  if (is.null(sigma)) { sigma <- diag(features) }
  if (is.null(mus)) { mus <- rep(0, features) }

  # Simulate points from a bivariate normal
  phi <- rmvnorm(n, mean = mus, sigma = sigma)

  # Decide which belong to each cluster
  rad <- (2 ** (features - 1) * gamma(1 + features / 2) /
         (pi ** (features / 2))) ** (1 / features)
  ones <- apply(phi, 1, function(x) { jitter(sum((x - mus) ** 2)) }) > rad ** 2
  #ones <- apply(phi, 1, function(x) { sum((x - mus) ** 2) }) > rad ** 2    
  category <- rep(0, length = n)
  category[ones] <- 1

  # Build the final data frame
  new.phi <- cbind.data.frame(as.character(category), phi)
  new.phi[, 1] <- as.factor(new.phi[, 1])
  colnames(new.phi) <- c('category', paste('feature', 1:features, sep = ''))

  # Save the data in a .csv file
  if (store == TRUE) {
    write.csv(new.phi, file = 'dataset.csv', row.names = FALSE)
    cat('Saved file:', paste(getwd(), '/dataset.csv', sep = ''), '\n')
  }

  # Plot
  if (pic == TRUE) {
    unlink('dataPlot.pdf')
    pdf('dataPlot.pdf')
    plot1 <- ggplot(data = new.phi, aes(x = feature1, y = feature2,
                    colour = category, fill = category)) +
                    geom_point() +
                    xlab('feature1') +
                    ylab('feature2') +
                    theme_bw()
    print(plot1)
    dev.off()
    cat('Saved plot:', paste(getwd(), '/dataPlot.pdf', sep = ''), '\n')
  }

  # End
  return(new.phi)
}
# END OF SCRIPT
