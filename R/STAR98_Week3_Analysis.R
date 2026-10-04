# ============================================================
# Week 3: Statistical Analysis and Predictive Modeling using R
# Dataset: STAR98 California school districts
# Model: Multiple Linear Regression
# ============================================================

# Install once if required:
# install.packages(c("ggplot2", "dplyr"))

library(ggplot2)
library(dplyr)

# 1. Load data -------------------------------------------------
star98 <- read.csv("STAR98_California_Schools.csv")

str(star98)
summary(star98)
dim(star98)
colSums(is.na(star98))

# 2. Variables -------------------------------------------------
target <- "PercentAbove"

predictors <- c(
  "LOWINC", "PERASIAN", "PERBLACK", "PERHISP", "PERMINTE",
  "AVYRSEXP", "AVSALK", "PERSPENK", "PTRATIO", "PCTAF",
  "PCTCHRT", "PCTYRRND"
)

# 3. Hypothesis testing ----------------------------------------
# H0: no linear association between PCTAF and PercentAbove
# H1: a linear association exists
cor.test(star98$PCTAF, star98$PercentAbove, method = "pearson")

# Correlation matrix
round(cor(star98[, c(target, predictors)]), 3)

# Two-group hypothesis test
pctaf_median <- median(star98$PCTAF)
star98$PCTAF_Group <- ifelse(
  star98$PCTAF < pctaf_median,
  "Below median PCTAF",
  "At/above median PCTAF"
)

t.test(PercentAbove ~ PCTAF_Group, data = star98)

# 4. Train/test split ------------------------------------------
set.seed(42)
train_index <- sample(
  seq_len(nrow(star98)),
  size = floor(0.80 * nrow(star98))
)

train <- star98[train_index, ]
test <- star98[-train_index, ]

# 5. Multiple linear regression --------------------------------
model <- lm(
  PercentAbove ~ LOWINC + PERASIAN + PERBLACK + PERHISP + PERMINTE +
    AVYRSEXP + AVSALK + PERSPENK + PTRATIO + PCTAF + PCTCHRT + PCTYRRND,
  data = train
)

summary(model)

# 6. Test-set performance --------------------------------------
test$Predicted <- predict(model, newdata = test)
test$Residual <- test$PercentAbove - test$Predicted

MAE <- mean(abs(test$Residual))
RMSE <- sqrt(mean(test$Residual^2))
R2 <- 1 - sum(test$Residual^2) /
  sum((test$PercentAbove - mean(test$PercentAbove))^2)

cat("Test MAE:", MAE, "\n")
cat("Test RMSE:", RMSE, "\n")
cat("Test R-squared:", R2, "\n")

# 7. Diagnostics -----------------------------------------------
par(mfrow = c(2, 2))
plot(model)

# Shapiro-Wilk test of test residuals
shapiro.test(test$Residual)

# 8. Manual VIF calculation ------------------------------------
vif_manual <- function(data, variables) {
  output <- numeric(length(variables))
  names(output) <- variables

  for (v in variables) {
    others <- setdiff(variables, v)
    f <- as.formula(paste(v, "~", paste(others, collapse = " + ")))
    aux <- lm(f, data = data)
    output[v] <- 1 / (1 - summary(aux)$r.squared)
  }
  output
}

vif_manual(train, predictors)

# 9. 10-fold cross-validation ----------------------------------
set.seed(42)
K <- 10
fold_id <- sample(rep(1:K, length.out = nrow(train)))

cv_results <- data.frame(
  Fold = integer(),
  RMSE = numeric(),
  MAE = numeric(),
  R2 = numeric()
)

for (k in 1:K) {
  cv_train <- train[fold_id != k, ]
  cv_valid <- train[fold_id == k, ]

  cv_model <- lm(
    PercentAbove ~ LOWINC + PERASIAN + PERBLACK + PERHISP + PERMINTE +
      AVYRSEXP + AVSALK + PERSPENK + PTRATIO + PCTAF + PCTCHRT + PCTYRRND,
    data = cv_train
  )

  pred <- predict(cv_model, newdata = cv_valid)
  actual <- cv_valid$PercentAbove

  rmse <- sqrt(mean((actual - pred)^2))
  mae <- mean(abs(actual - pred))
  r2 <- 1 - sum((actual - pred)^2) /
    sum((actual - mean(actual))^2)

  cv_results <- rbind(
    cv_results,
    data.frame(Fold = k, RMSE = rmse, MAE = mae, R2 = r2)
  )
}

print(cv_results)

cat("Mean CV RMSE:", mean(cv_results$RMSE), "\n")
cat("Mean CV MAE:", mean(cv_results$MAE), "\n")
cat("Mean CV R2:", mean(cv_results$R2), "\n")

# 10. Visualizations -------------------------------------------
ggplot(star98, aes(x = PCTAF, y = PercentAbove)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "College-Prep Participation vs Students Above National Median",
    x = "% taking UC/CSU preparation courses",
    y = "% students above national math median"
  )

ggplot(test, aes(x = Predicted, y = Residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Predicted %",
    y = "Residual"
  )

ggplot(test, aes(x = PercentAbove, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Percentage Above Median",
    x = "Actual %",
    y = "Predicted %"
  )

# End of Week 3 script
