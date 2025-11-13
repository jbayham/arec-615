# Parameters ---------------------------------------------------------------
beta <- 0.95       # discount factor
r    <- (1 - beta) / beta
p    <- 1.0
b    <- 0.2
c0   <- 0.1
c1   <- 0.3
Sbar <- 10
w    <- 0.5         # recharge per period

# Grids -------------------------------------------------------------------
Ns <- 201
S  <- seq(0, Sbar, length.out = Ns)
Nx <- 201
X  <- seq(0, Sbar, length.out = Nx)

# Functions ---------------------------------------------------------------
c_s <- function(s) c0 + c1 * (Sbar - s)    # per-unit cost
G    <- function(x, s) p*x - 0.5*b*x^2 - c_s(s)*x

# Bellman operator --------------------------------------------------------
V     <- rep(0, Ns)
V_new <- rep(0, Ns)
V_store <- vector("list", 2002)
pol_x <- rep(0, Ns)
tol   <- 1e-6
diff  <- Inf
iter  <- 0
maxit <- 2000

while(diff > tol && iter < maxit) {
  for (i in seq_along(S)) {
    s  <- S[i]
    # feasible x cannot exceed available stock
    X_feas <- X[X <= s + w]
    obj <- sapply(X_feas, function(x) {
      s_next <- pmax(0, pmin(Sbar, s + w - x))
      V_cont <- approx(S, V, xout = s_next, rule = 2)$y
      G(x, s) + beta * V_cont
    })
    k <- which.max(obj)
    V_new[i] <- obj[k]
    pol_x[i] <- X_feas[k]
  }
  V_store[[iter + 1]] <- V_new
  diff <- max(abs(V_new - V))
  V <- V_new
  iter <- iter + 1
  message(sprintf("Iteration %d: supnorm diff = %.8f", iter, diff))
}

cat("Converged in", iter, "iterations\n")

# Plot results ------------------------------------------------------------
library(ggplot2)
df <- data.frame(s = S, V = V, xstar = pol_x)

ggplot(df, aes(s, V)) +
  geom_line(linewidth = 1) +
  labs(title = "Value Function", x = "Stock s", y = "V(s)")

ggplot(df, aes(s, xstar)) +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(title = "Optimal Extraction Policy", x = "Stock s", y = "x*(s)")

#Convert V_store into dataframe with iterations to plot the evolution
V_evolution <- do.call(cbind, V_store[1:iter])
df_evolution <- data.frame(s = S, V_evolution)
library(dplyr)
library(tidyr)
df_long <- df_evolution %>%
  pivot_longer(-s, names_to = "iteration", values_to = "V",names_prefix = "X") %>%
  mutate(iteration = as.numeric(iteration))

ggplot(df_long, aes(s, V, group = iteration, color = iteration)) +
  geom_line() +
  scale_color_viridis_c() +
  labs(title = "Evolution of Value Function over Iterations",
       x = "Stock s", y = "V(s)", color = "Iteration") +
  theme_minimal()


# Check Burt condition -----------------------------------------------------
lhs <- p - b*pol_x - c_s(S)
rhs <- (c1 * pol_x) / r

df_burt <- data.frame(s = S, lhs, rhs)

ggplot(df_burt, aes(s)) +
  geom_line(aes(y = lhs, color = "LHS: ∂G/∂x")) +
  geom_line(aes(y = rhs, color = "RHS: (1/r)∂G/∂s")) +
  labs(title = "Burt Condition Check",
       x = "Stock s", y = "Marginal Value",
       color = "") +
  theme_minimal()

# Find steady state where x*(s) = w ---------------------------------------

s_star <- approx(x = pol_x, y = S, xout = w)$y

ggplot(df, aes(s, xstar)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_hline(yintercept = w, linetype = "dashed") +
  geom_vline(xintercept = s_star, linetype = "dotted", color = "red") +
  annotate("text", x = s_star + 0.3, y = w + 0.3,
           label = sprintf("steady state ≈ %.2f", s_star)) +
  labs(title = "Steady State from Burt's Condition",
       subtitle = "x*(s) = w",
       x = "Stock s", y = "Optimal extraction x*(s)")


