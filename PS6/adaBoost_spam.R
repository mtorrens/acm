################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #6 (Part 2)
# Date   : 2016.03.04
################################################################################
# source('~/Desktop/bgse/courses/term2/acm/problemSets/PS6/adaBoost_spam.R')
################################################################################

# Path
PATH <- '~/Desktop/bgse/courses/term2/acm/problemSets/PS6/'

# Package dependencies
library(gbm)
library(rpart)

# New adaBoost algorithm
source(paste(PATH, 'adaBoost.R', sep = ''))

# Load Spam dataset
file <- paste(PATH, '../../datasets/Spam/spambase.data', sep = '')
spam <- read.table(file, sep = ',', header = FALSE)
spam <- spam[sample(1:nrow(spam), nrow(spam)), ]  # Randomize sample order

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

# In and out of sample observations (80%-20%)
set.seed(666)
is <- sort(sample(1:nrow(spam), floor(0.8 * nrow(spam))))
os <- (1:nrow(spam))[! (1:nrow(spam)) %in% is]

# Define the two datasets
data <- spam[is, ]
test <- spam[os, ]

# Parameters for the model
form <- as.formula(y ~ .)
depth <- 10
niter <- 10

# See in sample performance
labs <- adaBoost(form, data = data, depth = depth, noTrees = niter)
tt <- table(data[, 'y'], labs[[1]])
acc <- sum(diag(tt)) / sum(tt)
cat('In-sample performance: ', 100 * round(acc, 3), '%\n', sep = '')

# See out sample performance
labs2 <- adaBoost(form, data, test, depth = depth, noTrees = niter)
tt2 <- table(test[, 'y'], labs2[[1]])
acc2 <- sum(diag(tt2)) / sum(tt2)
cat('Out-sample performance: ', 100 * round(acc2, 3), '%\n', sep = '')

# Package adaboost
boost <- gbm(formula = form,
             distribution = 'adaboost',
             data = rbind(data, test),
             n.trees = niter,
             interaction.depth = depth,
             shrinkage = 1,
             bag.fraction = 1,
             train.fraction = (4 / 5))

# Results from the package
acc3 <- mean(sign(predict(boost, test)) == ifelse(test[, 'y'] == '1', 1, -1))
cat('Out-sample performance: ', 100 * round(acc3, 3), '%\n', sep = '')

# See for different number of iterations
results <- as.data.frame(matrix(ncol = 3, nrow = 30))
results[, 1] <- 1:30
colnames(results) <- c('Iterations', 'adaBoost', 'gboost')
for (m in 1:30) {
  cat('Computing results for niter = ', m, '...\n', sep = '')

  # My super function
  labs <- adaBoost(form, data, test, depth = depth, noTrees = m)
  tt <- table(test[, 'y'], labs[[1]])
  acc <- sum(diag(tt)) / sum(tt)

  # Package function
  boost <- gbm(formula = form,
               distribution = 'adaboost',
               data = rbind(data, test),
               n.trees = m,
               interaction.depth = depth,
               shrinkage = 1,
               bag.fraction = 1,
               train.fraction = (4 / 5))

  # Add upp the results
  acc2 <- mean(sign(predict(boost, test)) == ifelse(test[, 'y'] == '1', 1, -1))
  results[m, 2] <- acc
  results[m, 3] <- acc2
  cat('Done!\n')
}

# Final plot
cairo_pdf(paste(PATH, 'adaBoost.pdf', sep = ''))
plot(results[, 2], type = 'l',  xlab ='Tree depth',
     ylab = 'Success rate', main = 'AdaBoost performance',
     col = 'darkblue', lwd = 2)
lines(results[, 3], col = 'darkred', lwd = 2)
legend('bottomright', c('adaBoost', 'gboost'),
       col = c('darkblue', 'darkred'), lty = 1, lwd = 2)
dev.off()
cat('Saved plot:', paste(PATH, 'adaBoost.pdf', sep = ''), '\n')
# END OF SCRIPT
