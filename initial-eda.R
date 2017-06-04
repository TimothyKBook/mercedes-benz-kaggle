options(stringsAsFactors = FALSE)

cwd <- '.'
setwd(cwd)

library(dplyr)
library(ggplot2)

train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

# No missing data.
any(sapply(train, function(x) any(is.na(x))))
any(sapply(test, function(x) any(is.na(x))))

# Strip covariates by type
covars <- train %>% select(starts_with('X'))
cat_vars <- covars[sapply(covars, function(x) class(x) == 'character')]
num_vars <- covars[sapply(covars, function(x) class(x) != 'character')]

# All quantitative variables are at most binary.  Will probably need to remove 
# the ones that are unary (or almost unary).
summary(sapply(num_vars, function(x) length(unique(x))))

# Categorical variables look ok.  Debating turning them into indicators and
# running a grouped lasso.
sapply(cat_vars, function(x) length(unique(x)))

# No duplicates
length(unique(train$ID)) == nrow(train)
length(unique(test$ID)) == nrow(test)

# Visualize response.  Very strange shape.  Trimodal?  Potential idea for new 
# dummy varite: separate modes into clusters.
ggplot(train) + 
  theme_bw() +
  geom_density(aes(x = y), fill = 'black', alpha = 1/2) 

# If trying an elastic net method, will likely want to log response.
ggplot(train) + 
  theme_bw() +
  geom_density(aes(x = log(y)), fill = 'black', alpha = 1/2) 

# EDA conclusion: All covariates are either categorical or binary.  This screams
# tree-methods, but have curse of dimensionality to contend with as well.  IMO,
# a grouped lasso seems like a good idea here.  Potentially with log-response.

# TODO: Mock this up as Jupyter Notebook