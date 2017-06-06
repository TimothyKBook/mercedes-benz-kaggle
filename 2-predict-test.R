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

# TODO: Get categorical variables ==============================================

for_m1 <- (test$X0 %in% train$X0) &
          (test$X2 %in% train$X2) &
          (test$X5 %in% train$X5)

cat_test_m1 <- cat_test[for_m1,]

cat_test_m2 <- cat_test[!for_m1,]

test_cat_mm1 <- as.data.frame(model.matrix(~ . - 1, data = cat_test_m1))
missing_cols <- setdiff(names(cat_mm), names(test_cat_mm))
for (col in missing_cols) {
  test_cat_mm1[col] <- 0
}

# Numeric variables ============================================================
num_test <- num_test[num_keep]
num_test1 <- num_test[for_m1,]
num_test2 <- num_test[!for_m1,]

# Fit m1 =======================================================================
df_test1 <- data.frame(cbind(test_cat_mm, num_test1))

test_pred1 <- predict(m1, df_test1)
test_pred2 <- predict(m2, as.matrix(num_test2), s = 'lambda.min')

test_out1 <- cbind(test$ID[for_m1], test_pred1)
test_out2 <- cbind(test$ID[!for_m1], test_pred2)

test_out <- rbind(test_out1, test_out2)
colnames(test_out) <- c('ID', 'y')

write.csv(test_out, 'tune-100.csv', row.names = FALSE)
