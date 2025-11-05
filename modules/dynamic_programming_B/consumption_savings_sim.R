# --- Parameters ---
r     <- 0.15
beta  <- .99 #1/(1+r) #0.96
sigma <- 2
y     <- 1            # small income to avoid corners

# Analytical growth from Euler with CRRA
cons_growth_analytical <- (beta * (1 + r))^(1 / sigma)

# --- Grid & utility ---
agrid <- seq(0, 50, length.out = 400)
n <- length(agrid)
u <- function(c) ifelse(c > 0, c^(1 - sigma) / (1 - sigma), -Inf)

# --- VFI ---
V <- rep(0, n)
policy <- rep(0, n)
tol <- 1e-6
diff <- 1
max_iter <- 2000
iter <- 0

while (diff > tol && iter < max_iter) {
  V_new <- rep(NA, n)
  for (i in seq_len(n)) {
    a <- agrid[i]
    # feasible next assets must satisfy c = (1+r)a + y - a' >= 0
    a_next <- agrid[agrid <= (1 + r) * a + y]
    if (length(a_next) == 0) {
      # consume everything
      V_new[i] <- u((1 + r) * a + y)
      policy[i] <- 0
    } else {
      c <- (1 + r) * a + y - a_next
      # continuation value via interpolation on current V
      Vcont <- approx(agrid, V, xout = a_next, rule = 2, ties = "ordered")$y
      val <- u(c) + beta * Vcont
      # ignore -Inf in maximization
      valid <- is.finite(val)
      V_new[i] <- max(val[valid])
      policy[i] <- a_next[ which.max(val) ]
    }
  }
  diff <- max(abs(V_new - V), na.rm = TRUE)
  V <- V_new
  iter <- iter + 1
}

# --- Helper: interpolate policy at any a ---
pol <- function(a) {
  approx(agrid, policy, xout = a, rule = 2, ties = "ordered")$y
}

# --- Compute c_{t+1}/c_t along the optimal path (NOT across grid neighbors) ---
a_t  <- 10                  # pick an interior starting asset
a_tp1 <- pol(a_t)
c_t   <- (1 + r) * a_t + y - a_tp1

a_tp2 <- pol(a_tp1)
c_tp1 <- (1 + r) * a_tp1 + y - a_tp2

cons_growth_vfi <- c_tp1 / c_t

data.frame(
  Method = c("Analytical Euler", "VFI (simulated, path-based)"),
  `c_{t+1}/c_t` = c(cons_growth_analytical, cons_growth_vfi)
)

# Policy vs 45-degree line
plot(agrid, policy, type = "l", lwd = 2,
     xlab = expression(a[t]), ylab = expression(a[t+1]),
     main = "Optimal Savings Policy Function")
abline(0, 1, lty = 2, col = "gray")
points(a_t, a_tp1, pch = 19, col = "red")
text(a_t, a_tp1, labels = "Sim path step", pos = 4, col = "red")

########################
#With Borrowing
# Parameters
beta <- 0.96
r <- 0.04
sigma <- 2

# Analytical Euler equation
cons_growth <- (beta * (1 + r))^(1 / sigma)
cons_growth


# Value function iteration setup

# Grid for assets
agrid <- seq(0, 10, length.out = 200)
n <- length(agrid)

# Utility function
u <- function(c) ifelse(c > 0, c^(1 - sigma) / (1 - sigma), -1e6)

# Value function iteration
V <- rep(0, n)
policy <- rep(0, n)
tol <- 1e-6
diff <- 1

while (diff > tol) {
  V_new <- rep(NA, n)
  for (i in seq_len(n)) {
    a <- agrid[i]
    c <- (1 + r) * a - agrid  # consumption for each possible next a'
    val <- u(c) + beta * V    # RHS of Bellman equation
    V_new[i] <- max(val)
    policy[i] <- agrid[which.max(val)]
  }
  diff <- max(abs(V_new - V))
  V <- V_new
}

# Compute implied consumption policy
c_policy <- (1 + r) * agrid - policy

# Compute consumption growth ratio near steady state
steady_idx <- which.min(abs(policy - agrid)) # near fixed point
growth_vfi <- c_policy[steady_idx + 1] / c_policy[steady_idx]
growth_vfi


data.frame(
  Method = c("Analytical Euler", "VFI (simulated)"),
  `c_{t+1}/c_t` = c(cons_growth, growth_vfi)
)
