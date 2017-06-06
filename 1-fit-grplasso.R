options(stringsAsFactors = FALSE)

cwd <- '.'
setwd(cwd)

library(dplyr)
library(grplasso)
library(glmnet)

train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

# Strip covariates by type
covars <- train %>% select(starts_with('X'))
cat_vars <- covars[sapply(covars, function(x) class(x) == 'character')]
num_vars <- covars[sapply(covars, function(x) class(x) != 'character')]

### Prepare categorical variables ==============================================
# Produce design matrix of categorical variables
cat_mm <- as.data.frame(model.matrix(~ . - 1, data = cat_vars))

# Create grouped lasso indices
cat_names <- colnames(cat_vars)
for (cn in cat_names) {
  assign(paste0(cn, '_ind'), which(grepl(cn, colnames(cat_mm))))
}

# Create integer indices for grplasso call
p_cat <- ncol(cat_mm)
grp_ind <- rep(0, p_cat)
for (i in 1:length(cat_names)) {
  cn <- cat_names[i]
  grp_ind[get(paste0(cn, '_ind'))] <- i
}

### Prepare quantitative variables =============================================
# Remove variables that have too-high purity.  ie, if they are too similar to
# being all-zeros or all-ones.
cut_tol <- 0.01
bin_purity <- unname(sapply(num_vars, mean))
num_keep <- (bin_purity < 1 - cut_tol) & (bin_purity > cut_tol)
table(num_keep)

num_vars <- num_vars[num_keep]

### Combine variables, get params input-ready ==================================
y <- train$y
x <- cbind(cat_mm, num_vars)

df <- data.frame(y, x)
df_log <- data.frame(logy, x)

grp_ind <- c(grp_ind, rep(NA, ncol(num_vars)))

### Fit m1 =====================================================================
m1 <- grplasso(formula = y ~ ., data = df, lambda = 100, model = LinReg())
yhat <- fitted(m1)

# Rsq of 61.4%, wow!
sse <- sum((y - yhat)^2)
ssto <- sum((y - mean(y))^2)
1 - sse / ssto

### Fit m2 =====================================================================
m2 <- cv.glmnet(as.matrix(num_vars), y)

