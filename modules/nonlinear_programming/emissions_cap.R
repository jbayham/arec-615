# Production under an emissions cap: classroom walkthrough
# Run sections interactively in RStudio, or source this file.
# Install once if needed: install.packages("nloptr")
library(nloptr)
library(ggplot2)

# ---- Find the emissions multiplier by rootfinding ----
h <- function(lambda) {
  0.5 * (12 / (2 + lambda))^2 +
    (12 / (2 + 2 * lambda))^2 - 17
}

lambda_root <- uniroot(h, interval = c(0, 2), tol = 1e-10)$root
q_root <- c(12 / (2 + lambda_root),
            12 / (2 + 2 * lambda_root))

#Print the solution
c(lambda = lambda_root, q1 = q_root[1], q2 = q_root[2],
  excess_emissions = h(lambda_root))


lambda_grid <- data.frame(lambda = seq(0, 2, length.out = 201))
lambda_grid$excess <- h(lambda_grid$lambda)
root_plot <- ggplot(lambda_grid, aes(lambda, excess)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_vline(xintercept = lambda_root, linetype = "dashed",
             colour = "firebrick") +
  annotate("point", x = lambda_root, y = 0,
           colour = "firebrick", size = 3) +
  annotate("text", x = lambda_root + 0.08, y = 5,
           label = "Root: lambda = 1", hjust = 0) +
  labs(x = expression(lambda),
       y = expression(h(lambda) == "Emissions minus cap")) +
  theme_minimal()
print(root_plot)


# ---- Define the economic model ----
profit <- function(q) sum(12*q - q^2)
profit_gradient <- function(q) 12 - 2*q
emissions <- function(q) 0.5*q[1]^2 + q[2]^2
emissions_gradient <- function(q) c(q[1], 2*q[2])
E <- 17

profit(c(6, 6))
emissions(c(6, 6))

# ---- Translate to solver conventions ----
objective <- function(q) -profit(q)
objective_gradient <- function(q) -profit_gradient(q)
cap_constraint <- function(q) emissions(q) - E
cap_jacobian <- function(q) matrix(emissions_gradient(q), nrow = 1)

# ---- Solve with SLSQP ----
fit <- nloptr::nloptr(
  x0 = c(2, 2),                    # a feasible starting allocation
  eval_f = objective,
  eval_grad_f = objective_gradient,
  eval_g_ineq = cap_constraint,
  eval_jac_g_ineq = cap_jacobian,
  lb = c(0, 0),
  opts = list(
    algorithm = "NLOPT_LD_SLSQP",
    xtol_rel = 1e-9,
    ftol_abs = 1e-10,
    tol_constraints_ineq = 1e-9,
    maxeval = 500
  )
)
q_star <- fit$solution
fit$status
cat(paste(strwrap(fit$message, width = 65), collapse = "\n"), "\n")
q_star
c(profit = profit(q_star), emissions = emissions(q_star),
  cap_residual = cap_constraint(q_star))

# ---- Check the KKT conditions ----
g_profit <- profit_gradient(q_star)
g_emissions <- emissions_gradient(q_star)
lambda_hat <- sum(g_emissions*g_profit) / sum(g_emissions^2)
checks <- c(
  cap_violation = max(0, cap_constraint(q_star)),
  bound_violation = max(0, -q_star),
  stationarity = max(abs(g_profit - lambda_hat*g_emissions)),
  dual_violation = max(0, -lambda_hat),
  complementarity = abs(lambda_hat*cap_constraint(q_star))
)
lambda_hat
data.frame(check = names(checks), residual = unname(checks))

# ---- Tighten the cap and compare policies ----
solve_cap <- function(E, start = c(2, 2)) {
  fit <- nloptr::nloptr(
    x0 = start, eval_f = objective, eval_grad_f = objective_gradient,
    eval_g_ineq = function(q) emissions(q) - E,
    eval_jac_g_ineq = cap_jacobian, lb = c(0, 0),
    opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-9,
                ftol_abs = 1e-10, tol_constraints_ineq = 1e-9,
                maxeval = 500)
  )
  if (!(fit$status %in% 1:4) || emissions(fit$solution) > E + 1e-7) {
    stop("The solver did not return a converged, feasible solution.")
  }
  q <- fit$solution
  data.frame(cap = E, q1 = q[1], q2 = q[2],
             profit = profit(q), emissions = emissions(q))
}

delta <- 0.01
baseline <- solve_cap(17)
tighter <- solve_cap(17 - delta)
looser <- solve_cap(17 + delta)
rbind(tighter, baseline, looser)

# Compare the numerical marginal value with lambda_hat.
(looser$profit - tighter$profit) / (2*delta)

# Equal output (also an equal percentage cut from the baseline 6, 6).
q_equal <- rep(sqrt(17/1.5), 2)
data.frame(
  policy = c("Efficient allocation", "Equal output restriction"),
  q1 = c(q_star[1], q_equal[1]),
  q2 = c(q_star[2], q_equal[2]),
  emissions = c(emissions(q_star), emissions(q_equal)),
  profit = c(profit(q_star), profit(q_equal)),
  profit_loss = 72 - c(profit(q_star), profit(q_equal))
)

# ---- Plot the feasible set and profit contours ----
library(ggplot2)
q_grid <- expand.grid(q1 = seq(0, 6.5, length.out = 150),
                      q2 = seq(0, 6.5, length.out = 150))
q_grid$profit <- with(q_grid, 12*q1 - q1^2 + 12*q2 - q2^2)
boundary <- data.frame(q1 = seq(0, sqrt(34), length.out = 200))
boundary$q2 <- sqrt(pmax(0, 17 - 0.5*boundary$q1^2))
equal_output <- sqrt(17/1.5)
geometry_plot <- ggplot(q_grid, aes(q1, q2)) +
  geom_ribbon(data = boundary, aes(ymin = 0, ymax = q2),
              fill = "lightblue", alpha = 0.4) +
  geom_contour(aes(z = profit), breaks = c(35, 45, 55, 59, 65, 70),
               colour = "grey50") +
  geom_line(data = boundary, colour = "steelblue", linewidth = 1) +
  annotate("point", x = 4, y = 3, colour = "firebrick", size = 3) +
  annotate("text", x = 4.2, y = 3.4, label = "Optimum (4, 3)", hjust = 0) +
  annotate("point", x = equal_output, y = equal_output, shape = 17, size = 3) +
  annotate("point", x = 6, y = 6, shape = 4, size = 3) +
  annotate("text", x = 5.9, y = 6.3, label = "Unregulated", hjust = 1) +
  coord_equal() + labs(x = "Output of producer 1", y = "Output of producer 2") +
  theme_minimal()
print(geometry_plot)

