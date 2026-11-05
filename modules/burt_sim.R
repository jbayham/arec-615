# Burt (1964): empirical comparison at s = 54.78.
# Run: Rscript modules/burt_sim.R (from repository root).
# Base R only; sourcing defines results and plot_burt_comparison().
# Main source: references/Burt-1964.pdf, pp. 85, 90-91, eqs. (18), (22), (31).
# Companion: Burt, Hilgardia 36(2), pp. 92-93, eq. (6.3):
# https://ageconsearch.umn.edu/record/381909/files/v36n02p031.pdf
#
# Reproduces analytical approximations and reconstructs the numerical comparison
# from the published fitted value function. Does NOT rerun the original discrete
# stochastic DP. R(x) is documented only for 3.50 <= x <= 4.00: do not extrapolate
# it to build a global VFI model or a steady-state simulation.

# Published parameters and units ------------------------------------------
# s: groundwater stock; x: TOTAL surface + ground water use, million acre-feet.
# w: surface diversion; half reaches farms, half recharges groundwater.
# R, G, V: million dollars; first water derivatives: dollars/acre-foot.
r <- 0.06
beta <- 1 / (1 + r)
s_initial <- 54.78
w_mean <- 1.815
surface_share <- 0.5
c0 <- 14.58
c1 <- 0.2004
revenue_intercept <- -641.0690
revenue_linear <- 449.6900
revenue_quadratic <- 58.3920
x_fit_range <- c(3.50, 4.00)

R <- function(x) revenue_intercept + revenue_linear * x - revenue_quadratic * x^2
R_x <- function(x) revenue_linear - 2 * revenue_quadratic * x
cost <- function(s) c0 - c1 * s
groundwater_use <- function(x) x - surface_share * w_mean
G <- function(x, s) R(x) - cost(s) * groundwater_use(x)
G_x <- function(x, s) R_x(x) - cost(s)
G_s <- function(x, s) c1 * groundwater_use(x)
# Stochastic stock transition: s_next = s + w - x.

# Second-degree curvature: smaller root of eq. (22) ------------------------
G_xx <- -2 * revenue_quadratic
G_sx <- c1
G_ss <- 0
a1 <- (1 - beta) * G_xx - 2 * beta * G_sx - beta * G_ss
a0 <- G_sx^2 - G_ss * G_xx
discriminant <- a1^2 - 4 * beta * a0
stopifnot(discriminant > 0, a1 < 0)
# Equivalent to (-a1 - sqrt(discriminant)) / (2 * beta), avoiding cancellation.
V_ss_second <- 2 * a0 / (-a1 + sqrt(discriminant))

# Solve eq. (31); zero curvature gives the first-degree approximation.
approx_policy <- function(s, curvature = 0) {
  (r * (revenue_linear - cost(s)) + c1 * surface_share * w_mean -
     w_mean * curvature) / (r * 2 * revenue_quadratic + c1 - curvature)
}
# Eq. (18); setting curvature to zero gives eq. (9).
approx_marginal_value <- function(x, s, curvature = 0) {
  (G_s(x, s) + beta * (w_mean - x) * curvature) / (1 - beta)
}
x_first <- approx_policy(s_initial)
x_second <- approx_policy(s_initial, V_ss_second)
value_first <- approx_marginal_value(x_first, s_initial)
value_second <- approx_marginal_value(x_second, s_initial, V_ss_second)

# Numerical comparison reconstructed from Hilgardia eq. (6.3) --------------
# These coefficients were fitted to the original DP's value function. They
# are inputs from that study, not a new value function estimated here.
V_fitted <- function(s) 1865.62 + 33.5778 * s - 0.1984 * s^2
V_s_fitted <- function(s) 33.5778 - 2 * 0.1984 * s
V_ss_fitted <- -2 * 0.1984
# Eq. (16): G_x = beta * [V'(s) + (E(w) - x) V''(s)]. For quadratic V,
# the expected derivative depends only on mean supply.
fitted_policy <- function(s) {
  (revenue_linear - cost(s) - beta * (V_s_fitted(s) + w_mean * V_ss_fitted)) /
    (2 * revenue_quadratic - beta * V_ss_fitted)
}
x_numerical_fit <- fitted_policy(s_initial)
value_numerical_fit <- V_s_fitted(s_initial)

# Computed versus reported (Management Science, p. 91) ---------------------
results <- data.frame(
  method = c("First degree", "Second degree", "Numerical fit reconstruction"),
  use_computed = c(x_first, x_second, x_numerical_fit),
  use_reported = c(3.7388, 3.7402, 3.7181),
  value_computed = c(value_first, value_second, value_numerical_fit),
  value_reported = c(10.03, 9.85, 11.84)
)
results$use_difference <- results$use_computed - results$use_reported
results$value_difference <- results$value_computed - results$value_reported
results$groundwater_computed <- groundwater_use(results$use_computed)
results$expected_stock_change <- w_mean - results$use_computed

# Check original equations independently of the closed-form policy formulas.
residual_first <- G_x(x_first, s_initial) - G_s(x_first, s_initial) / r
residual_second <- G_x(x_second, s_initial) -
  (G_s(x_second, s_initial) + (w_mean - x_second) * V_ss_second) / r
residual_fit <- G_x(x_numerical_fit, s_initial) - beta *
  (V_s_fitted(s_initial) + (w_mean - x_numerical_fit) * V_ss_fitted)
stopifnot(
  max(abs(c(residual_first, residual_second, residual_fit))) < 1e-10,
  abs(beta * V_ss_second^2 + a1 * V_ss_second + a0) < 1e-12,
  G_xx + beta * V_ss_second < 0,
  all(results$use_computed >= x_fit_range[1]),
  all(results$use_computed <= x_fit_range[2]),
  # These tolerances describe discrepancies in printed inputs/results,
  # not numerical solver accuracy. No coefficients are tuned to match.
  max(abs(results$use_difference)) < 0.0002,
  max(abs(results$value_difference)) < 0.01
)

# Compare conditions at the published stock, within the fitted output range.
plot_burt_comparison <- function() {
  x <- seq(x_fit_range[1], x_fit_range[2], length.out = 501)
  lhs <- G_x(x, s_initial)
  rhs_first <- G_s(x, s_initial) / r
  rhs_second <- (G_s(x, s_initial) + (w_mean - x) * V_ss_second) / r
  rhs_fit <- beta * (V_s_fitted(s_initial) + (w_mean - x) * V_ss_fitted)
  matplot(x, cbind(lhs, rhs_first, rhs_second, rhs_fit), type = "l",
          col = c("black", "steelblue", "darkorange", "purple"),
          lty = c(1, 2, 3, 4), lwd = 2,
          xlab = "Total annual water use (million acre-feet)",
          ylab = "Marginal benefit / continuation value ($/acre-foot)",
          main = "Burt (1964): storage = 54.78 million acre-feet")
  points(results$use_computed, G_x(results$use_computed, s_initial),
         pch = 19, col = c("steelblue", "darkorange", "purple"))
  legend("topright", c("Marginal current net output", "First degree",
                       "Second degree", "Fitted numerical continuation value"),
         col = c("black", "steelblue", "darkorange", "purple"),
         lty = c(1, 2, 3, 4), lwd = 2, bty = "n", cex = 0.8)
}

if (sys.nframe() == 0L) {
  cat("Burt (1964), pp. 90-91\n")
  cat(sprintf("Second-degree V''(s): %.9f (paper: 0.00575)\n", V_ss_second))
  print(results, digits = 8, row.names = FALSE)
  cat("\nUse: million acre-feet/year. Marginal value: dollars/acre-foot.\n")
  cat("Printed inputs yield small discrepancies; no coefficients were tuned.\n")
  cat("The numerical row uses a published value-function fit, not a new DP run.\n")
}
