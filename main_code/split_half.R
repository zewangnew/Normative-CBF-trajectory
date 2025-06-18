

library(gamlss)
library(caret)     # For stratified data splitting
library(moments)   # For skewness and kurtosis
library(nortest)   # For alternative normality testing

set.seed(123)

n_iterations <- 1000

# Initialize storage vectors
R_squared_values <- numeric(n_iterations)
skewness_values <- numeric(n_iterations)
kurtosis_values <- numeric(n_iterations)
shapiro_W_values <- numeric(n_iterations)
shapiro_p_values <- numeric(n_iterations)

# Begin split-half loop
for (i in 1:n_iterations) {
  cat("Iteration:", i, "\n")
  
  trainIndex <- createDataPartition(M_HC2$site, p = 0.5, list = FALSE)
  train_set <- M_HC2[trainIndex, ]
  test_set <- M_HC2[-trainIndex, ]
  
  model_train <- gamlss(
    cortex ~ bs(Age, df = 8) + Gender + sequence + random(as.factor(site)),
    sigma.fo = ~bs(Age) + Gender,
    nu.fo = ~1,
    tau.fo = ~1,
    family = JSU,
    data = train_set,
    control = gamlss.control(n.cyc = 200)
  )
  
  # Predict on the test set
  predictions <- predict(model_train, newdata = test_set, type = "response")
  
  # Calculate R²
  SS_total <- sum((test_set$cortex - mean(test_set$cortex))^2)
  SS_residual <- sum((test_set$cortex - predictions)^2)
  R_squared_values[i] <- 1 - (SS_residual / SS_total)
  
  # Compute quantile residuals on training set
  quant_residuals <- resid(model_train, what = "z-scores", type = "simple")
  
  # Compute skewness, kurtosis, and Shapiro-Wilk test
  skewness_values[i] <- skewness(quant_residuals)
  kurtosis_values[i] <- kurtosis(quant_residuals) - 3  # Excess kurtosis
  shapiro_result <- shapiro.test(sample(quant_residuals, min(5000, length(quant_residuals))))  # cap size
  shapiro_W_values[i] <- shapiro_result$statistic
  shapiro_p_values[i] <- shapiro_result$p.value
}

# Combine into data frame
fit_results <- data.frame(
  Iteration = 1:n_iterations,
  R_squared = R_squared_values,
  Skewness = skewness_values,
  Kurtosis = kurtosis_values,
  Shapiro_W = shapiro_W_values,
  Shapiro_p = shapiro_p_values
)

# Save results
saveRDS(fit_results, file = "xxx.rds")

# ---------------------------------------------
# Example visualization using the last iteration
# ---------------------------------------------

# Compute residuals again from the last model (optional)
test_residuals <- test_set$cortex - predictions

# Visualize residual characteristics
par(mfrow = c(2, 2))  # Layout for 4 plots

# 1. Residuals vs Fitted
plot(predictions, test_residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lwd = 2)

# 2. Histogram of R-squared values
hist(R_squared_values, breaks = 30, col = "grey80", border = "white",
     main = "Histogram of R² Across Iterations", xlab = "R-squared")

# 3. Q-Q Plot of residuals
qqnorm(quant_residuals, main = "Normal Q-Q Plot of Residuals")
qqline(quant_residuals, col = "red", lwd = 2)

# 4. Kernel density estimate
plot(density(quant_residuals), main = "Kernel Density of Residuals",
     xlab = "Residuals", ylab = "Density", col = "blue", lwd = 2)

# Reset layout
par(mfrow = c(1, 1))
