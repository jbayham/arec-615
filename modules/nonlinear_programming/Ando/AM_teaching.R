# Ando and Mallory (2012): an interactive portfolio lab with SQP
# AREC 615. Open in RStudio and run each numbered section in order.
# Select lines and press Cmd+Enter (Mac) or Ctrl+Enter (Windows/Linux).
# Install once if needed: install.packages(c("nloptr", "ggplot2"))
# This script is self-contained: no working-directory changes or data files.
# Inputs: main paper Table 1, DOI 10.1073/pnas.1114653109.
# These rounded inputs reconstruct the model, not every published number.
# The instructor's reproduce_SI.R separately audits the supplied SI results.

library(nloptr)
library(ggplot2)

# 1. Define joint climate outcomes and beliefs -----------------------------
# Rows are possible futures; columns are regions. We choose shares BEFORE
# learning which future occurs. All regions experience the SAME scenario.
regions <- c("Western", "Central", "Eastern")
scenarios <- c("Historic", "+2 C", "+4 C", "+4 C, wetter")
CCI <- matrix(c(
  0.290, 0.718, 0.317,
  0.178, 0.587, 0.561,
  0.124, 0.251, 0.584,
  0.168, 0.503, 0.654
), nrow = 4, byrow = TRUE, dimnames = list(scenarios, regions))

# Table 1 units: THOUSAND dollars per acre. Keep these units for Figure 3.
cost <- matrix(c(
  0.601, 0.697, 1.210,
  0.631, 0.720, 1.230,
  0.631, 0.720, 1.230,
  0.536, 0.659, 1.200
), nrow = 4, byrow = TRUE, dimnames = list(scenarios, regions))
probabilities <- list(
  "No change likely" = c(0.80, 0.10, 0.05, 0.05),
  "Uniform" = rep(0.25, 4)
)
returns <- list("CCI" = CCI, "CCI/cost" = CCI / cost)
# E[CCI/cost] generally differs from E[CCI]/E[cost]. Divide FIRST.


CCI
cost

# 2. Build expected returns and population covariance ----------------------
p <- probabilities$Uniform
mu <- drop(crossprod(p, CCI))
names(mu) <- regions
deviations <- sweep(CCI, 2, mu, "-")
Sigma <- crossprod(deviations, sweep(deviations, 1, p, "*"))
# These are specified scenario probabilities, not a sample: no n-1 divisor.
mu
Sigma
cov2cor(Sigma)

# 3. Evaluate equal thirds before optimizing -------------------------------
w_equal <- setNames(rep(1/3, 3), regions)
scenario_outcomes <- drop(CCI %*% w_equal)
mean_equal <- sum(p * scenario_outcomes)
variance_equal <- sum(p * (scenario_outcomes - mean_equal)^2)
data.frame(scenario = scenarios, probability = p, CCI = scenario_outcomes)
c(mean = mean_equal, sd = sqrt(variance_equal))
# Check that scenario arithmetic and matrix algebra give the same variance.
stopifnot(abs(variance_equal - drop(t(w_equal) %*% Sigma %*% w_equal)) < 1e-12)

# 4. Express the objective, derivatives, and constraints --------------------
# Minimize variance subject to sum(w)=1, mu'w=0.51, and w>=0.
# The equalities return zero when satisfied; Jacobian ROWS are constraints.
# This is the same simple, unscaled example displayed in the slides.
moments_example <- list(mu = mu, Sigma = Sigma)
mu <- moments_example$mu
Sigma <- moments_example$Sigma
target <- 0.51
objective <- function(w) drop(t(w) %*% Sigma %*% w)
gradient <- function(w) drop(2 * Sigma %*% w)
equalities <- function(w) c(sum(w) - 1, sum(mu * w) - target)
jacobian <- function(w) rbind(rep(1, 3), mu)

objective(w_equal)
gradient(w_equal)
equalities(w_equal)  # Equal thirds satisfy the land constraint, not the target.
jacobian(w_equal)

# 5. Solve with sequential quadratic programming ---------------------------
fit <- nloptr::nloptr(
  x0 = rep(1/3, 3), lb = rep(0, 3),
  eval_f = objective, eval_grad_f = gradient,
  eval_g_eq = equalities, eval_jac_g_eq = jacobian,
  opts = list(algorithm = "NLOPT_LD_SLSQP",
              xtol_rel = 1e-12, maxeval = 2000)
)
setNames(fit$solution, names(mu))

fit$status
fit$message
equalities(fit$solution)
stopifnot(fit$status %in% 1:4,
          max(abs(equalities(fit$solution))) < 1e-8,
          min(fit$solution) >= -1e-8)
w_star <- setNames(fit$solution, regions)
c(mean = sum(mu * w_star), sd = sqrt(objective(w_star)))
data.frame(region = regions, equal_share = w_equal, optimal_share = w_star)
# Expected shares: about 0.03987 Western, 0.38487 Central, 0.57526 Eastern.
# Expected CCI = 0.51; standard deviation = about 0.05777.
# A successful solver status should always be paired with feasibility checks.

# 6. Inspect the first-order conditions ------------------------------------
# L = w'Sigma w + lambda(target-mu'w) + eta(1-sum(w)) - nu'w.
# At this interior solution, nu=0, so 2 Sigma w = eta*1 + lambda*mu.
A <- cbind(eta = 1, lambda = mu)
dual <- qr.solve(A, gradient(w_star))
nu <- gradient(w_star) - drop(A %*% dual)
dual
nu
w_star * nu
# The marginal variance is 2*sum_j Sigma[i,j]*w[j]. Setting w[i]=0
# removes its own term, but does not remove covariance with other regions.

# 7. Wrap the calculations to solve different portfolios -------------------
# The helper below uses the SAME SQP algorithm. Scaling makes its stopping
# tolerances less sensitive to return units. It reports original-unit outcomes.
# At a unique extreme mean, the constraints already determine a one-region
# allocation. We return that allocation directly instead of iterating.
portfolio_moments <- function(R, p) {
  stopifnot(is.matrix(R), all(is.finite(R)), length(p) == nrow(R),
            all(is.finite(p)), all(p >= 0), abs(sum(p) - 1) < 1e-10)
  mu <- drop(crossprod(p, R))
  names(mu) <- colnames(R)
  deviations <- sweep(R, 2, mu, "-")
  # Population covariance under the assumed probabilities: no n-1 correction.
  Sigma <- crossprod(deviations, sweep(deviations, 1, p, "*"))
  list(mu = mu, Sigma = Sigma, R = R, p = p)
}

moments_example <- portfolio_moments(CCI, probabilities$Uniform)
moments_example$mu
moments_example$Sigma
cov2cor(moments_example$Sigma)

portfolio_outcomes <- function(w, moments) {
  mean_return <- sum(w * moments$mu)
  variance <- drop(crossprod(w, moments$Sigma %*% w))
  c(mean = mean_return, variance = variance, sd = sqrt(max(0, variance)))
}

equal_weights <- setNames(rep(1/3, 3), regions)
fws_weights <- setNames(c(0.14, 0.76, 0.10), regions) # Table 2, Fig. 3 H/I
portfolio_outcomes(equal_weights, moments_example)

# 3. Solve one constrained portfolio problem -------------------------------
# min w' Sigma w subject to sum(w)=1, mu'w=target, w>=0.
# target=NULL removes the return equality and finds global minimum variance.
# The paper's Eq. 1 prints wi>0, but its text and zero-weight results require
# wi>=0. We follow the economically meaningful, closed feasible set.
solve_portfolio <- function(moments, target = NULL) {
  mu <- moments$mu
  Sigma <- moments$Sigma
  n <- length(mu)
  # SQP supports positive semidefinite covariance, including the singular SI case.
  if (min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values) < -1e-10)
    stop("Covariance must be positive semidefinite.")
  if (!is.null(target)) {
    stopifnot(length(target) == 1, is.finite(target))
    if (target < min(mu) - 1e-10 || target > max(mu) + 1e-10)
      stop("Expected-return target is infeasible.")
    # At either extreme, a unique best/worst region fixes the weights exactly.
    edge <- which(abs(mu - target) < 1e-12)
    if (length(edge) == 1 &&
        (abs(target - min(mu)) < 1e-12 || abs(target - max(mu)) < 1e-12)) {
      w <- setNames(rep(0, n), names(mu))
      w[edge] <- 1
      return(list(w = w, outcomes = portfolio_outcomes(w, moments),
                  target = target, fit = NULL, endpoint = TRUE))
    }
  }
  # Scale objective and return equality to make tolerances insensitive to units.
  # This leaves the allocation unchanged; reported outcomes use original units.
  variance_scale <- max(abs(Sigma))
  if (variance_scale == 0) variance_scale <- 1
  return_scale <- max(abs(mu))
  if (return_scale == 0) return_scale <- 1
  equalities <- function(w) {
    if (is.null(target)) return(sum(w) - 1)
    c(sum(w) - 1, (sum(mu * w) - target) / return_scale)
  }
  equality_jacobian <- function(w) {
    if (is.null(target)) return(matrix(1, nrow = 1, ncol = n))
    rbind(rep(1, n), mu / return_scale)
  }
  fit <- nloptr(
    x0 = rep(1/n, n), lb = rep(0, n), ub = rep(1, n),
    eval_f = function(w) drop(crossprod(w, Sigma %*% w)) / variance_scale,
    eval_grad_f = function(w) drop(2 * Sigma %*% w) / variance_scale,
    eval_g_eq = equalities, eval_jac_g_eq = equality_jacobian,
    opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-12,
                ftol_abs = 1e-15, tol_constraints_eq = rep(1e-10, if(is.null(target)) 1 else 2),
                maxeval = 2000)
  )
  if (!(fit$status %in% 1:4)) stop("SQP did not converge: ", fit$message)
  w <- setNames(fit$solution, names(mu))
  stopifnot(abs(sum(w) - 1) < 1e-8, min(w) > -1e-8)
  if (!is.null(target)) stopifnot(abs(sum(mu * w) - target) < 1e-8)
  list(w = w, outcomes = portfolio_outcomes(w, moments), target = target,
       fit = fit, endpoint = FALSE)
}

example_fit <- solve_portfolio(moments_example, target = 0.51)
example_fit$w
example_fit$outcomes


# 8. Trace only the efficient branch ---------------------------------------
# First remove the mean equality to find global minimum variance (GMV).
gmv <- solve_portfolio(moments_example)
gmv$w
gmv$outcomes
# Then solve for means from GMV up to the largest feasible mean.
# Targets below the GMV mean give a dominated lower branch.
trace_frontier <- function(moments, npoints = 100) {
  minimum <- solve_portfolio(moments)
  targets <- seq(minimum$outcomes["mean"], max(moments$mu), length.out = npoints)
  do.call(rbind, lapply(targets, function(target) {
    fit <- solve_portfolio(moments, target)
    data.frame(target = target, mean = unname(fit$outcomes["mean"]),
               sd = unname(fit$outcomes["sd"]), t(fit$w), row.names = NULL)
  }))
}


frontier <- trace_frontier(moments_example)
head(frontier)
frontier_plot <- ggplot(frontier, aes(sd, mean)) +
  geom_path(linewidth = 1, colour = "#237468") +
  geom_point(aes(x = sqrt(variance_equal), y = mean_equal),
             data = data.frame(label = "Equal thirds"), shape = 17, size = 3) +
  labs(x = "Standard deviation of CCI", y = "Expected CCI",
       title = "Efficient conservation portfolios", subtitle = "Uniform probabilities; triangle = equal thirds") +
  theme_minimal(base_size = 14)
if (interactive()) print(frontier_plot)

# Regional shares along the same efficient branch.
shares <- do.call(rbind, lapply(regions, function(region)
  data.frame(mean = frontier$mean, region = region, share = frontier[[region]])))
shares_plot <- ggplot(shares, aes(mean, share, colour = region)) +
  geom_line(linewidth = 1) +
  labs(x = "Target expected CCI", y = "Share of protected land", colour = "Region") +
  theme_minimal(base_size = 14)
if (interactive()) print(shares_plot)

# 9. Compare beliefs and incorporate land costs ----------------------------
# For CCI/cost, shares allocate spending. Divide each scenario's benefit by
# its cost BEFORE calculating moments: E[CCI/cost] is not E[CCI]/E[cost].
comparison <- do.call(rbind, lapply(names(returns), function(metric) {
  do.call(rbind, lapply(names(probabilities), function(belief) {
    m <- portfolio_moments(returns[[metric]], probabilities[[belief]])
    f <- trace_frontier(m)
    f$metric <- metric
    f$belief <- belief
    f
  }))
}))
comparison_plot <- ggplot(comparison, aes(sd, mean, colour = belief)) +
  geom_path(linewidth = 1) + facet_wrap(~metric, scales = "free") +
  labs(x = "Standard deviation", y = "Expected return", colour = "Beliefs") +
  theme_minimal(base_size = 14)
if (interactive()) print(comparison_plot)
# All results remain in memory. To redisplay a plot, run its object name.
# The cost metric is CCI per thousand dollars; its axis differs from raw CCI.
