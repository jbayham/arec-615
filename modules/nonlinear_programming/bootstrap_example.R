#The purpose of this script is to illustrate the bootstrap method for estimating the standard errors of regression coefficients in R.
library(pacman)
p_load(tidyverse)

#Generate some example data
set.seed(123)
n <- 500
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 3 + 2*x1 - 1*x2 + rnorm(n)
data <- data.frame(y, x1, x2)

#Fit the initial linear model
initial_model <- lm(y ~ x1 + x2, data = data)
summary(initial_model)

#Function to perform bootstrap resampling and fit the model
bootstrap_function <- function(data, indices) {
  d <- data[indices, ] # Resample the data
  model <- lm(y ~ x1 + x2, data = d)
  return(coef(model)) # Return the coefficients
}

#Number of bootstrap samples
n_boot <- 1000
set.seed(123)

#Perform bootstrap resampling
boot_results <- replicate(n_boot, {
  indices <- sample(1:nrow(data), replace = TRUE)
  bootstrap_function(data, indices)
})
boot_results <- t(boot_results)
colnames(boot_results) <- c("(Intercept)", "x1", "x2")

#Construct typical regression table but with bootstrap estimates
boot_summary <- data.frame(
  Term = colnames(boot_results),
  Estimate = colMeans(boot_results),
  Std_Error = apply(boot_results, 2, sd),
  CI_Lower = apply(boot_results, 2, function(x) quantile(x, 0.025)),
  CI_Upper = apply(boot_results, 2, function(x) quantile(x, 0.975))
)
print(boot_summary)

#Visualize the bootstrap distributions of the coefficients
boot_results_df <- as.data.frame(boot_results)
boot_results_long <- pivot_longer(boot_results_df, cols = everything(), names_to = "Term", values_to = "Coefficient")
ggplot(boot_results_long, aes(x = Coefficient)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  facet_wrap(~ Term, scales = "free") +
  theme_minimal() +
  labs(title = "Bootstrap Distributions of Regression Coefficients",
       x = "Coefficient Value",
       y = "Frequency")

#Compare with standard errors from the initial model
initial_summary <- summary(initial_model)$coefficients
comparison <- data.frame(
  Term = rownames(initial_summary),
  Initial_Estimate = initial_summary[, "Estimate"],
  Initial_Std_Error = initial_summary[, "Std. Error"],
  Bootstrap_Estimate = boot_summary$Estimate,
  Bootstrap_Std_Error = boot_summary$Std_Error
)
print(comparison)
