################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #4 (Part 2)
# Date   : 2016.01.30
################################################################################
# source('~/Desktop/bgse/courses/term2/acm/problemSets/PS4/kNN_genData.R')
################################################################################

# Library requirements
library(mvtnorm)
library(MASS)
library(class)
library(ggplot2)
library(RColorBrewer)

# Directory of the code
PATH <- '~/Desktop/bgse/courses/term2/acm/problemSets/PS4/'

# Newly minted kNN
source(paste(PATH, 'kNN.R', sep = ''))

# Generating data functions
source(paste(PATH, 'genData.R', sep = ''))

# Set working directory
setwd(PATH)

# Generate some data
training <- genSun(seed = 666, saveData = FALSE, savePlot = FALSE)

# Apply k-NN function
res <- kNN(features = training[, 1:2], labels = training[, 3], k = 3, p = 2)

# Predictions data set
preds <- cbind.data.frame(training, res[['predLabels']])
colnames(preds) <- c(colnames(training), 'preds')

# Save results
file <- paste(PATH, 'predictions.csv', sep = '')
write.csv(preds, file = file, row.names = FALSE)
cat('Written file:', file, '\n')

# Preparing the plot
x <- training[, 1:2]
y <- as.numeric(as.character(training[, 3]))
colors <- brewer.pal(8, 'Dark2')[c(3, 2)]
ecolors <- colors[y + 1]

# Defining the grid
gs <- 75
xlim <- range(x[, 1])
tmpx <- seq(xlim[1], xlim[2], len = gs)
ylim <- range(x[, 2])
tmpy <- seq(ylim[1], ylim[2], len=gs)
newx <- expand.grid(tmpx, tmpy)
colnames(newx) <- colnames(x)
extx <- rbind(x, newx)
exty <- c(y, rep(NA, nrow(newx)))
yhat <- kNN(extx, exty, k = 3, p = 2, action = 'test')
#yhat <- knn(x, newx, y, k = 3)
yhat <- yhat[['predLabels']]
#yhat <- yhat[['probs']]
yhat <- yhat[(nrow(x) + 1):nrow(rbind(x, newx))]
colshat <- colors[as.numeric(yhat)]

# Plotting
plot.file <- paste(PATH, 'plot.pdf', sep = '')
cairo_pdf(plot.file)
plot(x, xlab = 'X_1', ylab = 'X_2', xlim = xlim, ylim = ylim, type = 'n')
points(newx, col = colshat, pch = '.')
contour(tmpx, tmpy, matrix(as.numeric(yhat), gs, gs),
        levels = c(1, 2), add = TRUE, drawlabels = FALSE)
points(x, col = ecolors)
title('3-NN Decision boundaries')
dev.off(); cat('Saved plot:', plot.file, '\n')
# END OF SCRIPT
