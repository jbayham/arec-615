# -------------------------------------------
# Infinite-horizon fishery: discrete VFI example
# AREC 615 - Dynamic Programming in Economics
# -------------------------------------------

# Parameters
beta <- 0.95    # discount factor
r <- 0.5        # intrinsic growth rate
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
  for (ii in seq_along(S)) {
    s <- S[ii]
    # feasible harvests h = 0,...,s
    h_set <- 0:s
    # next state after harvest
    s_next <- G(s - h_set)
    # current payoff
    profit <- (p - c) * h_set
    # Bellman RHS for each h
    val_h <- profit + beta * V[s_next + 1]  # +1 for R's 1-indexing
    # choose best
    V_new[ii] <- max(val_h)
    policy[ii] <- h_set[which.max(val_h)]
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

# Plot the optimal harvest policy at each feasible stock.
policy_plot <- ggplot(results, aes(x = stock, y = harvest)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_point(color = "steelblue", size = 2) +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  labs(
    title = "Optimal Harvest Policy (VFI)",
    x = "Current stock (s)", y = "Optimal harvest h*(s)",
    caption = "Points are feasible stock levels; connecting lines are visual guides."
  ) +
  theme_minimal(base_size = 12)
print(policy_plot)

# -------------------------------------------
# Phase diagram: stock dynamics under optimal policy
# -------------------------------------------

results$next_stock <- G(results$escapement)
# Identify every exact fixed point on the discrete grid, including extinction.
steady_states <- results %>% filter(next_stock == stock)

phase_plot <- ggplot(results, aes(x = stock, y = next_stock)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
  geom_line(color = "darkorange", linewidth = 0.7) +
  geom_point(color = "darkorange", size = 2) +
  geom_point(data = steady_states, shape = 21, fill = "white",
             color = "firebrick", size = 3, stroke = 1) +
  geom_text(data = steady_states, aes(label = paste0("s* = ", stock)),
            nudge_y = 0.8, color = "firebrick", size = 3.5) +
  coord_equal(xlim = c(0, Smax), ylim = c(0, Smax + 1.5)) +
  labs(
    title = "Stock Dynamics under the Optimal Harvest Policy",
    subtitle = "Above the dashed line: stock increases; below: stock decreases",
    x = expression(paste("Current stock  ", s[t])),
    y = expression(paste("Next-period stock  ", s[t+1])),
    caption = paste(
      "Dashed line: unchanged stock. Outlined points: steady states.",
      "Connecting lines are visual guides between discrete states.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12)
print(phase_plot)

# Harvest and escapement at every stock
composition_plot <- results %>%
  pivot_longer(cols = c(harvest, escapement),
               names_to = "type", values_to = "amount") %>%
  ggplot(aes(x = stock, y = amount, fill = type)) +
  geom_col(width = 0.5) +
  labs(title = "Harvest and Escapement by Current Stock",
       x = "Current stock (s)", y = "Amount", fill = "Type") +
  theme_minimal(base_size = 12)
print(composition_plot)
