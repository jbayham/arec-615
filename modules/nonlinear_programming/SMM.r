
# ----------------------------------------------
# Simulated Method of Moments example: abatement model
# ----------------------------------------------

library(stats)
library(Matrix)
library(nloptr)   # or another optimizer for constrained optimization

# -----------------------
# 1. Data / “empirical” moment setup
# -----------------------

# (In practice, replace this with your observed data)
N_data <- 500

set.seed(999)  # master seed for reproducibility
n_eps <- 500   # number of simulation draws per firm
eps_mat <- matrix(rnorm(N_data * n_eps, mean = 0, sd = 1), nrow = n_eps, ncol = N_data)

# Suppose e0_i are drawn from some (known) distribution, e.g. uniform or empirical
e0_data <- runif(N_data, 10, 50)

# Suppose observed abatement a_i (simulated or real) — for now simulate under “true” theta
theta_true <- c(theta1 = 1.0, theta2 = 0.5, sigma = 2.0)
P_penalty <- 10.0

# A helper: given e0 and theta, compute optimal a by (inner) approximate minimization
compute_opt_abate <- function(e0, theta, P, eps_vec) {
  # theta = c(theta1, theta2, sigma)
  # we approximate E[max{0, e0 - a + eps}] by Monte Carlo
  # We do a small (inner) optimization over a ≥ 0
  
  # eps_vec is fixed shocks for this agent (mean 0, sd=1)
  eps <- eps_vec * theta["sigma"]   # scale by sigma
  
  # objective in a for a single agent i
  obj_fun <- function(a) {
    # cost
    cost <- theta["theta1"] * a + 0.5 * theta["theta2"] * a^2
    # penalty expectation approx
    resid <- e0 - a + eps
    penalty_term <- mean(pmax(resid, 0))
    return(cost + P * penalty_term)
  }
  
  # use a simple optimization; lower bound = 0, no upper bound
  # Use optimize() over a scalar interval
  # Pick a search interval: [0, e0 + 3*sigma]
  # ensure a valid interval
  upper <- max(1e-6, e0 + 3 * theta["sigma"])
  # if upper <= 0, bail out with big penalty
  if (is.na(upper)) return(0)
  if (upper <= 0) return(0)  # or return some safe value
  res <- optimize(obj_fun, interval = c(0, upper))
  a_opt <- res$minimum
  return(a_opt)
}

# Generate “observed” a_data using the true parameters
N <- length(e0_data)
a_data <- numeric(N)
for (i in seq_len(N)) {
  a_data[i] <- compute_opt_abate(
    e0 = e0_data[i],
    theta = theta_true,
    P = P_penalty,
    eps_vec = eps_mat[, i]   # pass column i
  )
}

# empirical moments
m_data <- c(
  mean_a = mean(a_data),
  var_a  = var(a_data),
  cov_a_e0 = cov(a_data, e0_data)
)

print(m_data)

# -----------------------
# 2. Simulator: given theta, generate simulated moments
# -----------------------
simulate_moments <- function(theta, e0_vector, P, eps_mat) {
  # e0_vector: vector of exogenous e0 for "simulated firms"
  # returns a vector of moments (same length as m_data)
  
  N <- length(e0_vector)
  a_sim <- numeric(N)
  
  for (i in seq_len(N)) {
    a_sim[i] <- compute_opt_abate(e0_vector[i], theta, P, eps_mat[,i])
  }
  
  return(c(
    mean_a = mean(a_sim),
    var_a  = var(a_sim),
    cov_a_e0 = cov(a_sim, e0_vector)
  ))
}

# quick test: simulate at true theta
m_sim_true <- simulate_moments(theta_true, e0_data, P_penalty, eps_mat)
print(m_sim_true)

# -----------------------
# 3. Objective function for SMM
# -----------------------
smm_obj <- function(param_vec, e0_vector, P, m_data, eps_mat) {
  if (length(param_vec) < 3) return(1e10)
  
  # Unpack explicitly (avoid relying on names)
  theta1 <- param_vec[1]
  theta2 <- param_vec[2]
  sigma  <- param_vec[3]
  
  # Check validity
  if (is.na(theta1) || is.na(theta2) || is.na(sigma)) return(1e10)
  if (theta1 <= 0 || theta2 <= 0 || sigma <= 0) return(1e10)
  
  theta <- c(theta1 = theta1, theta2 = theta2, sigma = sigma)
  
  # simulate moments
  m_sim <- simulate_moments(theta, e0_vector, P, eps_mat)
  
  # simple weighting: identity (can extend to optimal weighting later)
  diff = m_sim - m_data
  #diff = (m_sim - m_data)/m_data  # relative differences
  weight_matrix <- diag(length(theta)) #equal weights

  out <- t(diff) %*% weight_matrix %*% (diff)
  #diff <- sum((m_sim - m_data)^2)
  return(out)
}

# -----------------------
# 4. Estimate theta by minimizing objective
# -----------------------
# initial guess
init <- c(theta1 = 0.5, theta2 = 0.2, sigma = 1.5)

# Use a bounded optimizer (e.g. nloptr or stats::optim)
res <- nloptr(
  x0 = init,
  eval_f = smm_obj,
  lb = c(0, 0, 0.0001),
  ub = c(10, 10, 10),
  opts = list(algorithm = "NLOPT_LN_BOBYQA",
              maxeval = 600,
              print_level = 3,
              xtol_rel = 1e-9),
  e0_vector = e0_data,
  P = P_penalty,
  m_data = m_data,
  eps_mat = eps_mat
)

print(res)
theta_hat <- res$solution
names(theta_hat) <- c("theta1", "theta2", "sigma")
theta_hat

res$hessian  # if needed for standard errors

# Compare to true
cat("True theta:", theta_true, "\n")
cat("Estimated theta:", theta_hat, "\n")

# -----------------------
# 5. Sensitivity checks / diagnostics
# -----------------------
# e.g. vary n_eps_inner (Monte Carlo draws), plot obj surface slices, etc.
# For example:
# for (n in c(50, 100, 200, 500)) {
#   cat("n_eps_inner = ", n, ": objective at true theta = ",
#       smm_obj(theta_true, e0_data, P_penalty, m_data, eps_mat), "\n")
# }

m_hat <- simulate_moments(theta_hat, e0_data, P_penalty, eps_mat)

moment_df <- data.frame(
  moment = names(m_data),
  empirical = as.numeric(m_data),
  simulated = as.numeric(m_hat)
)

print(moment_df)

library(ggplot2)
library(tidyverse)

moment_df %>%
  pivot_longer(cols = c("empirical", "simulated"),names_to = "source", values_to = "value") %>%
  ggplot( aes(x=moment,y=value,fill=source)) +
  geom_col(position = "dodge") +
  labs(y="Value", fill="Source",
       title="Empirical vs Simulated Moments at Estimated Parameters") +
  theme_minimal()


###########################
# Plot optimal abatement vs e0 at estimated parameters
tibble(
  e0 = e0_data,
  a = a_data
) %>%
  ggplot(aes(x=e0,y=a)) +
  geom_point(alpha=.3,color="blue") +
  labs(x="e0 (exogenous emissions)", y="a (abatement)",
       subtitle="Optimal abatement vs exogenous emissions at estimated parameters") +
  theme_minimal()



