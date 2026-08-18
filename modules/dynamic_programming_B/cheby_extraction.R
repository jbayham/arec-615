

# Recover policy from Chebyshev-approximated value function
library(rootSolve)

# ---- Parameters (discrete time Hotelling) ----
beta  <- 0.95
p     <- 1.0
c_mc  <- 1.0           # marginal cost slope so C(x) = 1/2 c_mc x^2
Smax  <- 10            # domain for approximation of V(s)
N     <- 8             # number of Chebyshev basis terms
M     <- N             # collocation points

# ---- Chebyshev nodes on [0, Smax] ----
chebpts <- function(N) {
  k <- 0:(N-1)
  x <- cos(pi * (2*k + 1) / (2*N))
  rev(x) # from -1 to 1
}
s_nodes <- 0.5 * (Smax * (1 + chebpts(M)))

# ---- Chebyshev basis and its derivative wrt s ----
cheb_basis <- function(s, N = 4, Smax = 10) {
  z <- 2 * s / Smax - 1
  # Recurrence for T_n(z)
  T <- matrix(NA, nrow = length(z), ncol = N)
  T[,1] <- 1
  if (N > 1) T[,2] <- z
  if (N > 2) {
    for (j in 3:N) T[,j] <- 2 * z * T[,j-1] - T[,j-2]
  }
  colnames(T) <- paste0("T", 0:(N-1))
  as.matrix(T)
}

cheb_basis_deriv <- function(s, N = 4, Smax = 10) {
  z <- 2 * s / Smax - 1
  dzds <- 2 / Smax
  # Compute T_n(z) and dT_n/dz via stable recurrence
  # Initialize
  T <- matrix(0, nrow = length(z), ncol = N)
  dTdz <- matrix(0, nrow = length(z), ncol = N)
  T[,1] <- 1
  if (N > 1) {
    T[,2] <- z
    dTdz[,2] <- 1
  }
  if (N > 2) {
    for (n in 3:N) {
      # T_n = 2 z T_{n-1} - T_{n-2}
      T[,n] <- 2 * z * T[,n-1] - T[,n-2]
      # dT_n/dz = 2 T_{n-1} + 2 z dT_{n-1}/dz - dT_{n-2}/dz
      dTdz[,n] <- 2 * T[,n-1] + 2 * z * dTdz[,n-1] - dTdz[,n-2]
    }
  }
  # dT_n/ds = dT_n/dz * dz/ds -- chain rule
  dTds <- dTdz * dzds
  colnames(dTds) <- paste0("dT", 0:(N-1), "ds")
  as.matrix(dTds)
}

V_hat <- function(s, theta) {
  # Uses global N and Smax from the chunk scope
  Phi <- cheb_basis(s, N, Smax)
  as.numeric(Phi %*% theta)
}

V_hat_prime <- function(s, theta) {
  # Uses global N and Smax from the chunk scope
  Phi_d <- cheb_basis_deriv(s, N, Smax)
  as.numeric(Phi_d %*% theta)
}

# ---- Collocation residuals with parameters (p, c_mc, beta) ----
residuals_fun <- function(theta) {
  R <- numeric(M)
  for (i in 1:M) {
    s <- s_nodes[i]
    # Maximize Bellman RHS over x in [0, s]
    objective <- function(x) {
      prof <- p * x - 0.5 * c_mc * x^2 + beta * V_hat(s - x, theta)
      return(-prof) # minimize negative profit
    }
    opt <- optimize(objective, c(0, s)) 
    R[i] <- V_hat(s, theta) - (-opt$objective)
  }
  R
}



# ---- Solve for Chebyshev coefficients theta* ----
theta_init <- rep(0, N)
theta_star <- multiroot(f = residuals_fun, start = theta_init)$root

V_hat(s_nodes, theta_star)  # Test initial V_hat at nodes


# ---- Policy from FOC (Euler) using V' ----
policy_x <- function(s) {
  if (s <= 0) return(0)
  f <- function(x) p - c_mc * x - beta * V_hat_prime(s - x, theta_star)
  # Try bracketing in [0, s]
  f0 <- f(0); fs <- f(s)
  if (is.finite(f0) && is.finite(fs) && f0 * fs < 0) {
    return(uniroot(f, c(0, s))$root)
  }
  # Fall back to direct 1D optimization on the Bellman RHS
  obj <- function(x) p * x - 0.5 * c_mc * x^2 + beta * V_hat(s - x, theta_star)
  vals <- c(obj(0), obj(s))
  if (is.finite(vals[1]) && is.finite(vals[2])) {
    return(c(0, s)[which.max(vals)])
  }
  # Robust final fallback
  optimize(function(x) -obj(x), c(0, s))$minimum
}

# ---- Evaluate policy on a grid and compare with direct maximization ----
s_grid <- seq(0, Smax, length.out = 101)
x_policy <- vapply(s_grid, policy_x, numeric(1))

x_direct <- vapply(s_grid, function(s) {
  if (s <= 0) return(0)
  objective <- function(x) -(p * x - 0.5 * c_mc * x^2 + beta * V_hat(s - x, theta_star))
  optimize(objective, c(0, s))$minimum
}, numeric(1))

par(mfrow = c(1, 2), mar = c(4.2, 4.5, 2.5, 1.2))
plot(s_grid, x_policy, type = "l", lwd = 2, col = "#2C7BB6",
     xlab = "Stock s", ylab = "Policy x*(s)", main = "Policy via Euler (V')")
lines(s_grid, x_direct, lty = 2, lwd = 2, col = "#D95F02")
legend("topleft", c("Euler/FOC", "Direct maximize"), lty = c(1, 2), lwd = 2,
       col = c("#2C7BB6", "#D95F02"), bty = "n")

plot(s_grid, V_hat(s_grid, theta_star), type = "l", lwd = 2, col = "#1A9641",
     xlab = "Stock s", ylab = "V(s)", main = "Approximated Value Function")

# ---- Simulate a path using the policy until depletion ----
simulate_path <- function(S0, T = 50) {
  s <- S0
  S <- numeric(T + 1); X <- numeric(T)
  S[1] <- s
  t_last <- T
  for (t in 1:T) {
    x <- policy_x(s)
    X[t] <- x
    s <- max(s - x, 0)
    S[t + 1] <- s
    if (s <= 1e-8) { t_last <- t; break }
  }
  list(S = S[1:(t_last + 1)], X = X[1:t_last])
}

path <- simulate_path(S0 = 8, T = 60)
t_idx <- seq_along(path$X)

par(mfrow = c(1, 2), mar = c(4.2, 4.5, 2.5, 1.2))
plot(t_idx, path$X, type = "l", lwd = 2, col = "#2C7BB6",
     xlab = "t", ylab = "x_t", main = "Extraction over time")
plot(0:(length(path$S) - 1), path$S, type = "l", lwd = 2, col = "#1A9641",
     xlab = "t", ylab = "S_t", main = "Stock over time")

