# -------------------------------------------
# Infinite-horizon fishery: discrete VFI example
# AREC 615 - Dynamic Programming in Economics
# -------------------------------------------

# Parameters
beta <- 0.98    # discount factor
r <- 0.8        # intrinsic growth rate
K <- 30         # carrying capacity
p <- 1          # price per unit harvest
c <- 0.05          # cost per unit harvest

# State space (discrete stock levels)
Smax <- 20
S <- 0:Smax

# Logistic growth function (deterministic)
G <- function(x) {
  x_next <- x + r * x * (1 - x / K)
  # keep within bounds and round to nearest integer state
  pmax(0, pmin(Smax, round(x_next)))
}

# Initialize value and policy
V <- rep(0, length(S))
Vstor <- vector("list", length = 0)
policy <- rep(0, length(S))

# Convergence settings
tol <- 1e-8
diff <- Inf
iter <- 0
max_iter <- 10000

# -------------------------------------------
# Value Function Iteration
# -------------------------------------------

while (diff > tol && iter < max_iter) {
  V_new <- numeric(length(S))
  for (i in seq_along(S)) {
    s <- S[i]
    # feasible harvests h = 0,...,s
    h_set <- 0:s
    # next state after harvest
    s_next <- G(s - h_set)
    # current payoff
    profit <- (p - c) * h_set
    # Bellman RHS for each h
    val_h <- profit + beta * V[s_next + 1]  # +1 for R's 1-indexing
    # choose best
    V_new[i] <- max(val_h)
    policy[i] <- h_set[which.max(val_h)]
  }
  diff <- max(abs(V_new - V))
  V <- V_new
  iter <- iter + 1
  Vstor[[iter]] <- V
}

cat("VFI converged in", iter, "iterations.\n")

# -------------------------------------------
# Results
# -------------------------------------------
library(tidyverse)
#Plot how elements of the value function evolve over iterations
to_plot <- map(1:21, function(i) {
  data.frame(
    stock = i - 1,
    value = map_dbl(Vstor, i),
    iteration = 1:iter
  )}) %>%
  bind_rows()

to_plot %>%
  filter(#iteration %% 10 == 0 | iteration == 1 | iteration == iter,
         iteration %in% c(1:300),
         stock %in% c(1,7,15)) %>%
  ggplot(aes(x = iteration, y = value)) +
  geom_line() +
  facet_wrap(~ stock, ncol = 5) 


results <- data.frame(
  stock = S,
  value = round(V, 3),
  harvest = policy,
  escapement = S - policy
)

print(results, row.names = FALSE)

# Plot policy function
plot(S, policy, type = "b", pch = 19, col = "steelblue",
     xlab = "Stock (s)", ylab = "Optimal harvest h*(s)",
     main = "Optimal Harvest Policy (VFI)")
abline(v = 4, lty = 2, col = "gray")  # approximate target escapement



# -------------------------------------------
# Phase diagram: stock dynamics under optimal policy
# -------------------------------------------

# Compute next-period stock under optimal policy
s_next <- G(S - policy)

# Plot state transition
plot(S, s_next, type = "b", pch = 19, col = "darkorange",
     xlab = expression(paste("Current stock  ", s[t])),
     ylab = expression(paste("Next-period stock  ", s[t+1])),
     main = "Phase Diagram of the Fishery")

# Add 45-degree line
abline(0, 1, lty = 2, col = "gray40")

# Find steady state visually (intersection point)
steady_index <- which.min(abs(s_next - S))
points(S[steady_index], s_next[steady_index],
       pch = 19, col = "red", cex = 1.4)
text(S[steady_index], s_next[steady_index] + 1,
     labels = bquote(s^"*" == .(S[steady_index])),
     col = "red", cex = 0.9)

# Optional: annotate regions
text(15, 10, "Stock declines", col = "gray30")
text(3, 6, "Stock increases", col = "gray30")

# create bar chart with the composition of harvest and escapement at steady state using ggplot2
library(ggplot2)
results %>%
  tidyr::pivot_longer(cols = c(harvest, escapement),
                      names_to = "type", values_to = "amount") %>%
  ggplot(aes(x = stock, y = amount, fill = type)) +
  geom_col( width = 0.5) +
  labs(title = paste("Harvest and Escapement at Steady State (s* =", S[steady_index], ")"),
       fill = "Type") +
  theme_minimal()
