options(stringsAsFactors = FALSE)

cwd <- '.'
setwd(cwd)

library(dplyr)

train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

load('intermed-rdatas/model1.RData')

test_covars <- test %>% select(starts_with('X'))
cat_test <- test_covars[sapply(test_covars, function(x) class(x) == 'character')]
num_test <- test_covars[sapply(test_covars, function(x) class(x) != 'character')]

test_cat_mm <- as.data.frame(model.matrix(~ . - 1, data = cat_test))
# ... Whoops.  To do - remove X0, X2, X5