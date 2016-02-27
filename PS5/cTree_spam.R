################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #5 (Part 2)
# Date   : 2016.02.27
################################################################################
# source('~/Desktop/bgse/courses/term2/acm/problemSets/PS5/cTree_spam.R')
################################################################################

# Path
PATH <- '~/Desktop/bgse/courses/term2/acm/problemSets/PS5/'

# Package dependencies
library(rpart)
#library(tree)
library(data.table)
library(compiler)
library(parallel)
library(doMC)

# Source the function
source(paste(PATH, 'cTree.R', sep = ''))

# Optimize a bit
train.tree <- cmpfun(train.tree)
cTree <- cmpfun(cTree)
div.set <- cmpfun(div.set)
counts <- cmpfun(counts)
cost <- cmpfun(cost)

# Load Spam dataset
file <- paste(PATH, '../../datasets/Spam/spambase.data', sep = '')
spam <- read.table(file, sep = ',', header = FALSE)

# Variable names
names(spam) <- c('v_make', 'v_address', 'v_all', 'v_3d', 'v_our', 'v_over', 
                 'v_remove', 'v_internet', 'v_order', 'v_mail', 'v_receive',
                 'v_will',  'v_people', 'v_report', 'v_addresses', 'v_free',
                 'v_business', 'v_email',  'v_you', 'v_credit', 'v_your',
                 'v_font', 'v_000', 'v_money', 'v_hp', 'v_hpl', 'v_george',
                 'v_650', 'v_lab', 'v_labs', 'v_telnet', 'v_857',  'v_data',
                 'v_415', 'v_85', 'v_technology', 'v_1999', 'v_parts',  'v_pm',
                 'v_direct', 'v_cs', 'v_meeting', 'v_original', 'v_project', 
                 'v_re', 'v_edu', 'v_table', 'v_conference', 'v_semicolon',
                 'v_par', 'v_bracket', 'v_excl', 'v_dollar', 'v_hash',
                 'v_average', 'v_longest', 'v_total', 'y')

# spam.tree <- tree(factor(y) ~ ., data = spam)
# summary(spamTree)

# Randomly divide the dataset
set.seed(666)
train <- sample(1:nrow(spam), floor(0.7 * nrow(spam)))
spam.train <- spam[train, ]
spam.test <- spam[-train, ]

if (FALSE) {
  # Evaluate the model on the test data
  #spam.tree <- tree(factor(y) ~ ., data = spam, subset = train)
  spam.tree <- rpart(factor(y) ~ ., data = spam[train, ], method = 'class',
                      control = rpart.control(maxdepth = 1))

  # Test error
  spam.pred <- predict(spam.tree, spam.test, type = 'class')
  conf.matrix <- table(spam.pred, spam.test[, 'y'])
  etree <- 1 - sum(diag(conf.matrix)) / sum(conf.matrix)

  # Our tree
  spam.ctree <- cTree(factor(y) ~ ., data = spam.train, test = spam.test,
                      minPoints = 10, depth = 2)
}

# Try different depths
depths <- c(seq(1, 30, 1))
registerDoMC(cores = detectCores() - 1) 
package.trees <- foreach(d = depths) %dopar% {
  cat('Training rpart: maxdepth =', d, '\n')
  ptree <- rpart(factor(y) ~ ., data = spam[train, ], method = 'class',
                 control = rpart.control(maxdepth = d,
                                         minsplit = 3,
                                         minbucket = 3))

  preds <- predict(ptree, spam.test, type = 'class')
  conf.matrix <- table(preds, spam.test[, 'y'])
  etree <- 1 - sum(diag(conf.matrix)) / sum(conf.matrix)
  return(data.frame(d = d, etree = etree))
}
package.trees <- as.data.frame(rbindlist(package.trees))

# Same for our function
registerDoMC(cores = detectCores() - 1) 
our.trees <- foreach(d = depths) %dopar% {
  cat('Training cTree: maxdepth =', d, '\n')
  ptree <- cTree(factor(y) ~ ., data = spam.train, test = spam.test,
                 minPoints = 3, depth = d, costFnc = 'Gini')

  preds <- ptree[[1]]
  conf.matrix <- table(preds, spam.test[, 'y'])
  etree <- 1 - sum(diag(conf.matrix)) / sum(conf.matrix)
  cat('Depth:', d, 'Error:', etree, '\n')
  return(data.frame(d = d, etree = etree))
}
our.trees <- as.data.frame(rbindlist(our.trees))

# Final plot
cairo_pdf(paste(PATH, 'cTree.pdf', sep = ''))
plot(package.trees[, 2], type = 'l', ylim = c(0.1, 0.25),
     col = 'darkgreen', lty = 2, lwd = 2)
lines(our.trees[, 2], col = 'darkred', lty = 2, lwd = 2)
legend('bottomright', c('rpart', 'cTree'),
       col = c('darkgreen', 'darkred'), lty = 1)
dev.off()
# END OF SCRIPT
