# -------------------------------------------
# Infinite-horizon fishery: discrete VFI example
# AREC 615 - Dynamic Programming in Economics
# -------------------------------------------

# Parameters
beta <- 0.95    # discount factor
r <- 0.5        # intrinsic growth rate
Smax <- 20      # carrying capacity and largest state
p <- 1          # price per unit harvest
c <- 0          # cost per unit harvest

# State space (discrete stock levels)
S <- 0:Smax

# Logistic growth function (deterministic)
G <- function(x) {
  x + r * x * (1 - x / Smax)
}

# Numerical transition: round growth to the nearest integer state.
# R rounds exact half-integers to the nearest even integer.
f_grid <- function(s, h) round(G(s - h))

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
    s_next <- f_grid(s, h_set)
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

if (diff > tol) stop("VFI did not converge within max_iter.")

# Recover the policy using the final value vector; break ties toward less harvest.
for (ii in seq_along(S)) {
  h_set <- 0:S[ii]
  val_h <- (p - c) * h_set + beta * V[f_grid(S[ii], h_set) + 1]
  policy[ii] <- h_set[which.max(val_h)]
}
cat("VFI converged in", iter, "iterations.\n")

# -------------------------------------------
# Results
# -------------------------------------------
library(tidyverse)
#Plot how elements of the value function evolve over iterations
to_plot <- map(seq_along(S), function(i) {
  data.frame(
    stock = S[i],
    value = map_dbl(Vstor, i),
    iteration = 1:iter
  )}) %>%
  bind_rows()

convergence_plot <- to_plot %>%
  filter(stock %in% c(1, 7, 15)) %>%
  ggplot(aes(x = iteration, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  facet_wrap(~ stock, ncol = 3, labeller = label_both) +
  labs(title = "Value Function Iteration",
       x = "Iteration", y = "Value") +
  theme_minimal(base_size = 12)
print(convergence_plot)


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

results$next_stock <- f_grid(results$stock, results$harvest)
# Identify every exact fixed point on the discrete grid, including extinction.
steady_states <- results %>% filter(next_stock == stock)

# Cobweb paths: move vertically to the policy-induced transition, then
# horizontally to the 45-degree line to use next stock as current stock.
# Both paths use the discrete transition table, not the connecting lines.
trace_stock <- function(initial_stock, periods = 10) {
  stock <- initial_stock
  segments <- list()
  for (tt in seq_len(periods)) {
    next_stock <- results$next_stock[match(stock, results$stock)]
    if (next_stock == stock) break
    segments[[tt]] <- data.frame(
      x = c(stock, stock), y = c(stock, next_stock),
      xend = c(stock, next_stock), yend = c(next_stock, next_stock),
      path = paste("Start at", initial_stock)
    )
    stock <- next_stock
  }
  bind_rows(segments)
}
cobweb_paths <- bind_rows(trace_stock(2), trace_stock(16))

# Build the diagram cumulatively for classroom discussion. Each named object
# can be printed separately, and sourcing the script prints them in order.
# Keep the same axis limits and aspect ratio at every stage.
phase_plot_1 <- ggplot(results, aes(x = stock, y = next_stock)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
  scale_x_continuous(limits = c(0, Smax)) +
  scale_y_continuous(limits = c(0, Smax + 1.5)) +
  coord_equal() +
  labs(
    title = "Stock Dynamics under the Optimal Harvest Policy",
    subtitle = "1. The 45-degree line: next-period stock equals current stock",
    x = expression(paste("Current stock  ", s[t])),
    y = expression(paste("Next-period stock  ", s[t+1])),
    caption = "Above the dashed line: stock increases; below: stock decreases."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
print(phase_plot_1)

# Stage 2: add the policy-induced transition and mark its fixed points.
phase_plot_2 <- phase_plot_1 +
  geom_line(color = "darkorange", linewidth = 0.7) +
  geom_point(color = "darkorange", size = 2) +
  geom_point(data = steady_states, shape = 21, fill = "white",
             color = "firebrick", size = 3, stroke = 1) +
  geom_text(data = steady_states, aes(label = paste0("s* = ", stock)),
            nudge_y = 0.8, color = "firebrick", size = 3.5) +
  labs(
    subtitle = "2. Add next-period stock under optimal harvest",
    caption = paste(
      "Outlined points: steady states.",
      "Orange connecting lines are visual guides between discrete states.",
      sep = "\n"
    )
  )
print(phase_plot_2)

# Stage 3: trace rebuilding from stock 2 (2 -> 3 -> 4 -> 6).
phase_plot_3 <- phase_plot_2 +
  geom_segment(data = filter(cobweb_paths, path == "Start at 2"),
               aes(x = x, y = y, xend = xend, yend = yend, color = path),
               inherit.aes = FALSE, linewidth = 0.65,
               arrow = grid::arrow(length = grid::unit(0.10, "inches"),
                                   type = "closed")) +
  scale_color_manual(values = c("Start at 2" = "steelblue",
                                "Start at 16" = "#00796B"),
                     breaks = c("Start at 2", "Start at 16"), name = NULL) +
  labs(
    subtitle = "3. Start at stock 2: follow the blue arrows",
    caption = paste(
      "Arrows: vertical to next stock, then horizontal to the dashed line; repeat.",
      "Orange connecting lines are visual guides between discrete states.",
      sep = "\n"
    )
  )
print(phase_plot_3)

# Stage 4: add the path from high stock (16 -> 6).
phase_plot_4 <- phase_plot_3 +
  geom_segment(data = filter(cobweb_paths, path == "Start at 16"),
               aes(x = x, y = y, xend = xend, yend = yend, color = path),
               inherit.aes = FALSE, linewidth = 0.65,
               arrow = grid::arrow(length = grid::unit(0.10, "inches"),
                                   type = "closed")) +
  labs(subtitle = "4. Start at stock 16: add the green arrows")
print(phase_plot_4)

# Preserve the original name for the completed figure.
phase_plot <- phase_plot_4

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
