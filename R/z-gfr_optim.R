# This code fits a normal distribution for GFR at birth assuming linear decline
# and the know values for <60
# Observed data
x_vals <- 60 + c(40, 60, 70) #
p_obs  <- c(0.0027, 0.02, 0.084)  # raw vals per group
p_obs  <- cumsum(p_obs) # vals below age X

# Define the loss function to minimize
loss_function <- function(params) {
  mu <- params[1]
  sigma <- params[2]
  # Predicted cumulative probabilities under N(mu, sigma)
  p_pred <- pnorm(x_vals, mean = mu, sd = sigma)
  # Sum of squared differences
  sum((p_pred - p_obs)^2)
}

# Initial guesses for mu and sigma
init_params <- c(mu = 160, sigma = 20)

# Run the optimizer
result <- optim(par = init_params, fn = loss_function, method = "L-BFGS-B",
  lower = c(-Inf, 1e-6))  # Ensure sigma > 0

# Extract results
mu_est <- result$par[1]
sigma_est <- result$par[2]

# Print results
cat("Optimized mean (mu):", mu_est, "\n") # 146.4956
cat("Optimized standard deviation (sigma):", sigma_est, "\n") # 13.25594

x <- rnorm(1e5, mu_est, sigma_est)
mean(x < 60 + 40) # prop of < 60 and 40yo
mean(x < 60 + 60)
mean(x < 60 + 70)

library(ggplot2)

ggplot(data.frame(x = x), aes(x = x)) +
  geom_density() +
  geom_vline(xintercept = 60 + 40) +
  annotate("label", x = 60 + 40, y = 0.02, label = "40yo") +
  annotate("label", x = 60 + 40, y = 0.015, label = mean(x < 60 + 40)) +
  geom_vline(xintercept = 60 + 60) +
  annotate("label", x = 60 + 60, y = 0.02, label = "60yo") +
  annotate("label", x = 60 + 60, y = 0.015, label = mean(x < 60 + 60)) +
  geom_vline(xintercept = 60 + 70) +
  annotate("label", x = 60 + 70, y = 0.02, label = "70yo") +
  annotate("label", x = 60 + 70, y = 0.015, label = mean(x < 60 + 70))
