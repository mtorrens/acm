################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #1 (Part 2)
# Date   : 2016.01.13
################################################################################
# source('~/Desktop/bgse/projects/github/acm/PS1/loanData3C.R')
################################################################################

# Set working directory
if (FALSE) {
  setwd('~/Desktop/bgse/projects/github/acm/PS1/')
}

# Libraries
library(mvtnorm)
library(ggplot2)

# Wrapper functions
################################################################################
sigma.xy <- function(rho, sd.x, sd.y) {
################################################################################
  cov.term <- rho * sd.x * sd.y
  vcm <- matrix(c(sd.x ** 2, cov.term, cov.term, sd.y ** 2), 2, 2, byrow = TRUE)
  return(vcm)
}

################################################################################
gen.bvn <- function(n = 1, seed = NA, mu.xy = c(0, 1), sigma.xy = diag(2)) {
################################################################################
  require(mvtnorm)
  if (! is.na(seed)) { set.seed(seed) }
  rdraws <- rmvnorm(n, mean = mu.xy, sigma = sigma.xy)
  return(rdraws)
}

################################################################################
loan.data <- function(n.app, n.den, n.und, mu.app, mu.den, mu.und, sd.app,
                      sd.den, sd.und, rho.app, rho.den, rho.und, seed = 1111,
                      store = FALSE, pic = TRUE) {
################################################################################
  sigma.app <- sigma.xy(rho = rho.app, sd.x = sd.app[1], sd.y = sd.app[2])
  sigma.den <- sigma.xy(rho = rho.den, sd.x = sd.den[1], sd.y = sd.den[2])
  sigma.und <- sigma.xy(rho = rho.und, sd.x = sd.und[1], sd.y = sd.und[2])
  app <- gen.bvn(n = n.app, mu.app, sigma.app, seed = seed)
  den <- gen.bvn(n = n.den, mu.den, sigma.den, seed = seed + 1)
  und <- gen.bvn(n = n.und, mu.und, sigma.und, seed = seed + 2)
  loan.df <- as.data.frame(rbind(app, den, und))
  deny <- c(rep('approved', n.app),
            rep('denied', n.den),
            rep('undecided', n.und))
  target <- c(rep(0, n.app), rep(1, n.den), rep(2, n.und))
  loan.df <- data.frame(loan.df, deny, target)
  colnames(loan.df) <- c('pi_ratio', 'solvency', 'deny', 'target')

  if (store == TRUE) {
    write.csv(loan.df, file = 'predictions.csv', row.names = FALSE)    
  }

  return(loan.df)
}

################################################################################
# Function that carries out the exercise
loanData3C <- function(store = TRUE, pic = TRUE, dashed = TRUE) {
################################################################################
  # Build the data frame
  loan.df <- loan.data(n.app = 50, n.den = 50, n.und = 50, mu.app = c(4, 150),
                       mu.den = c(10, 100), mu.und = c(9, 200),
                       sd.app = c(1, 20), sd.den = c(2, 30), sd.und = c(1, 10),
                       rho.app = -0.1, rho.den = 0.6, rho.und = 0.5,
                       seed = 1221)

  # Rebuild the data frame for prediction
  n.app <- 50
  n.den <- 50
  n.und <- 50
  loan.df <- cbind(loan.df,
                   target1 = c(rep(0, n.app), rep(1, n.den), rep(0, n.und)),
                   target2 = c(rep(1, n.app), rep(0, n.den), rep(0, n.und)),
                   target3 = c(rep(0, n.app), rep(0, n.den), rep(1, n.und)))

  cols <- c('pi_ratio', 'solvency')
  X <- as.matrix(cbind(ind = rep(1, nrow(loan.df)), loan.df[, cols]))
  Y <- cbind(target1 = c(rep(0, n.app), rep(1, n.den), rep(0, n.und)),
             target2 = c(rep(1, n.app), rep(0, n.den), rep(0, n.und)),
             target3 = c(rep(0, n.app), rep(0, n.den), rep(1, n.und)))

  # Parameters
  w <- solve(t(X) %*% X) %*% t(X) %*% Y

  # Predictions
  preds <- X %*% w
  #head(preds)
  denied <- (preds == apply(preds, 1, max))[, 1]
  approv <- (preds == apply(preds, 1, max))[, 2]
  undeci <- (preds == apply(preds, 1, max))[, 3]

  # Prediction and conf.matrix
  pred.labs <- ifelse(denied, 'denied', ifelse(approv, 'approved', 'undecided'))
  conf.matrix <- table(loan.df[, 'deny'], pred.labs)
  conf.matrixP <- prop.table(conf.matrix, 1)

  # Intercepts and slopes of the discriminant functions
  int1 <- (w[1, 2] - w[1, 1]) / (w[2, 1] - w[2, 2])
  slo1 <- (w[3, 2] - w[3, 1]) / (w[2, 1] - w[2, 2])

  int2 <- (w[1, 3] - w[1, 1]) / (w[2, 1] - w[2, 3])
  slo2 <- (w[3, 3] - w[3, 1]) / (w[2, 1] - w[2, 3])

  int3 <- (w[1, 2] - w[1, 3]) / (w[2, 3] - w[2, 2])
  slo3 <- (w[3, 2] - w[3, 3]) / (w[2, 3] - w[2, 2])

  # Plot them
  if (dashed == FALSE) {
    plot1 <- ggplot(data = loan.df,
                    aes(x = solvency,
                        y = pi_ratio,
                        colour = deny,
                        fill = deny)) +
                    theme_bw() +
                    geom_point() +
                    xlab('Solvency') +
                    ylab('PI Ratio') +
                    #theme(text = element_text(family = 'Arial')) + 
                    geom_abline(intercept = int1, slope = slo1) + 
                    geom_abline(intercept = int2, slope = slo2) +
                    geom_abline(intercept = int3, slope = slo3)
  } else {
    # Where the three equations intersect
    meet.x <- (int2 - int1) / (slo1 - slo2)
    meet.y <- int1 + slo1 * meet.x

    x1 <- 0.5 * min(loan.df[, 'solvency'])
    x2 <- 1.2 * max(loan.df[, 'solvency'])

    y11 <- int1 + slo1 * x1
    y12 <- int1 + slo1 * x2
    y21 <- int2 + slo2 * x1
    #y21 <- int2 + slo2 * 142  # Special case
    y22 <- int2 + slo2 * x2
    #y22 <- int2 + slo2 * 167  # Special case
    y31 <- int3 + slo3 * x1
    y32 <- int3 + slo3 * x2

    # Plot
    plot1 <- ggplot(data = loan.df,
                    aes(x = solvency, y = pi_ratio, colour = deny, fill = deny)) +
                    theme_bw() +
                    geom_point() +
                    xlab('Solvency') +
                    ylab('PI Ratio') +
                    geom_segment(x = x1, y = y11, xend = meet.x, yend = meet.y, col = 'black') +
                    geom_segment(x = x2, y = y12, xend = meet.x, yend = meet.y, lty = 3, col = 'black') +
                    geom_segment(x = x1, y = y21, xend = meet.x, yend = meet.y, lty = 3, col = 'black') +
                    #geom_segment(x = 142, y = y21, xend = meet.x, yend = meet.y, lty = 3, col = 'black') +
                    geom_segment(x = x2, y = y22, xend = meet.x, yend = meet.y, col = 'black') +
                    #geom_segment(x = 167, y = y22, xend = meet.x, yend = meet.y, col = 'black') +
                    geom_segment(x = x1, y = y31, xend = meet.x, yend = meet.y, lty = 3, col = 'black') +
                    geom_segment(x = x2, y = y32, xend = meet.x, yend = meet.y, col = 'black')    
  }

  # Save the plot
  if (pic == TRUE) {
    pdf('discFunction3C.pdf')
    print(plot1)
    dev.off()
    cat('Saved plot:', paste(getwd(), '/discFunction3C.pdf', sep = ''), '\n')
  }

  # Final result
  df <- loan.df
  for (i in 1:3) {
    df[, paste('target', i, sep = '')] <- NULL
  }
  df <- cbind(df, preds, pred.labs)
  colnames(df) <- c('pi_ratio', 'solvency', 'denied', 'target', 'pred_class1',
                    'pred_class2', 'pred_class3', 'decision')

  # Save the data in a .csv file
  if (store == TRUE) {
    write.csv(df, file = 'predictions.csv', row.names = FALSE)
    cat('Saved file:', paste(getwd(), '/predictions.csv', sep = ''), '\n')
  }

  # End
  predictions <<- df
  return(df)
}

# Run it
loanData3C()
# END OF SCRIPT
