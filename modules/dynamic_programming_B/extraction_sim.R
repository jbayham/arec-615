# Extraction problem simulation (Hotelling rule)
# Model: maximize ∫ e^{-rho t} [p x(t) - 0.5 * c * x(t)^2] dt
# s.t. S'(t) = -x(t), S(0) = S0, x(t) >= 0, S(t) >= 0
# FOC: p - c x = lambda (present-value co-state), and lambda' = rho * lambda
# With p=1 (constant), solution has lambda(t) = lambda0 * exp(rho t),
# x(t) = max((p - lambda(t))/c, 0), and extraction stops when lambda reaches p.

# Parameters (feel free to tweak c)
p   <- 1.0
rho <- 0.05
S0  <- 10.0
c   <- 0.5   # slope of marginal extraction cost (MC = c * x)

# Total extraction up to the exhaustion time T where lambda(T)=p
# T(lambda0) = (1/rho) * log(p / lambda0)
# Q(lambda0) = ∫_0^T (p - lambda0 * e^{rho t})/c dt
#            = (1/c) [ p T - (lambda0/rho) (e^{rho T} - 1) ]
#            = (1/c) [ p T - (1/rho) (p - lambda0) ]
# We choose lambda0 so that Q(lambda0) = S0.

extract_total <- function(lambda0, p, rho, c) {
  T <- log(p / lambda0) / rho
  Q <- (1 / c) * (p * T - (1 / rho) * (p - lambda0))
  return(Q)
}

# Root function for uniroot: extract_total(lambda0) - S0 = 0
f_root <- function(lambda0) extract_total(lambda0, p, rho, c) - S0

# Solve for lambda0 in (0, p). lambda0 is the initial scarcity rent.
lambda0 <- uniroot(f_root, interval = c(1e-10, p - 1e-10))$root
# Exhaustion time T* is defined by lambda(T*) = p
T_star  <- log(p / lambda0) / rho

# Time grid (simulate up to exhaustion)
N <- 600
t <- seq(0, T_star, length.out = N)

# Co-state (scarcity rent) and controls
lambda_t <- lambda0 * exp(rho * t)
x_t      <- pmax((p - lambda_t) / c, 0)

# Stock path (analytic expression up to T) and clipped at zero
S_t <- S0 - (1 / c) * (p * t - (lambda0 / rho) * (exp(rho * t) - 1))
S_t <- pmax(S_t, 0)

# Hotelling growth rate: lambda_dot / lambda = rho (constant)
lambda_dot_over_lambda <- rep(rho, length(t))

# Print key solution values
cat(sprintf("Solved lambda0 = %.6f, exhaustion time T* = %.4f\n", lambda0, T_star))

# Utility to determine script directory for saving figures
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(sub("^--file=", "", file_arg[1])))
  }
  # Fallback: current working directory (e.g., when run interactively)
  return(getwd())
}

out_dir <- get_script_dir()

# Plot trajectories: x(t), S(t), lambda(t), and Hotelling growth
png(file.path(out_dir, "extraction_sim.png"), width = 1000, height = 900)
par(mfrow = c(2, 2), mar = c(4.2, 4.5, 2.5, 1.2))

# 1) Extraction rate x(t)
plot(t, x_t, type = "l", lwd = 2, col = "#2C7BB6",
     xlab = "Time t", ylab = "x(t)", main = "Optimal extraction x(t)")
abline(v = T_star, lty = 2, col = "gray50")
legend("topright", legend = c("x(t)", "exhaustion T*"),
       lty = c(1, 2), lwd = c(2, 1), col = c("#2C7BB6", "gray50"), bty = "n")

# 2) Stock S(t)
plot(t, S_t, type = "l", lwd = 2, col = "#1A9641",
     xlab = "Time t", ylab = "S(t)", main = "Resource stock S(t)")
abline(v = T_star, lty = 2, col = "gray50")

# 3) Scarcity rent lambda(t) and net price p - c x(t) (they coincide)
rent_implied <- p - c * x_t
plot(t, lambda_t, type = "l", lwd = 2, col = "#D7191C",
     xlab = "Time t", ylab = expression(lambda(t)), main = "Scarcity rent lambda(t) = p - c x(t)")
lines(t, rent_implied, lty = 3, lwd = 2, col = "#FDAE61")
abline(v = T_star, lty = 2, col = "gray50")
legend("topleft", legend = c(expression(lambda(t)), expression(p - c %.% x(t)), "T*"),
       lty = c(1, 3, 2), lwd = c(2, 2, 1), col = c("#D7191C", "#FDAE61", "gray50"), bty = "n")

# 4) Hotelling growth: d ln lambda / dt = rho
plot(t, lambda_dot_over_lambda, type = "l", lwd = 2, col = "#2B8CBE",
     xlab = "Time t", ylab = expression(dot(lambda)/lambda),
     ylim = c(0, max(rho * 1.2, 0.1)),
     main = "Hotelling rule: d ln lambda / dt = rho (constant)")
abline(h = rho, lty = 2, col = "#2B8CBE")
text(x = max(t) * 0.7, y = rho * 1.02, labels = sprintf("rho = %.3f", rho), pos = 3, col = "#2B8CBE")

mtext(sprintf("p=%.1f, rho=%.2f, S0=%.1f, c=%.2f; T* = %.2f", p, rho, S0, c, T_star),
      side = 1, outer = TRUE, line = -2, cex = 0.9)

dev.off()

# Also display the plots in an interactive session
if (interactive()) {
  par(mfrow = c(3, 1), mar = c(4.2, 4.5, 2.5, 1.2))
  plot(t, x_t, type = "l", lwd = 2, col = "#2C7BB6", xlab = "Time t", ylab = "x(t)", main = "Optimal extraction x(t)")
  abline(v = T_star, lty = 2, col = "gray50")
  plot(t, S_t, type = "l", lwd = 2, col = "#1A9641", xlab = "Time t", ylab = "S(t)", main = "Resource stock S(t)")
  abline(v = T_star, lty = 2, col = "gray50")
  plot(t, lambda_t, type = "l", lwd = 2, col = "#D7191C", xlab = "Time t", ylab = expression(lambda(t)), main = "Scarcity rent lambda(t)")
  abline(v = T_star, lty = 2, col = "gray50")
}
