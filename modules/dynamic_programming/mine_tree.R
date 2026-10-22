# ------------------------------------------------------------
# Mine Extraction Problem: Finite-Horizon Dynamic Programming
# ------------------------------------------------------------
# The mine has a stock s_t ∈ {0,1,2,3,4}
# At each t, choose extraction x_t ∈ [0, s_t]
# Reward: π(x_t) = 8x_t - x_t^2
# Transition: s_{t+1} = s_t - x_t
# Horizon: T = 2 (decisions at t=0, t=1)
# ------------------------------------------------------------

rm(list = ls())

# Parameters
beta <- 0.9
pi <- function(x) 8 * x - x^2    # per-period profit function
S <- 0:4                         # possible stock levels
T <- 2                           # horizon

# ------------------------------------------------------------
# Step 1: Initialize terminal period value function V_T(s)
# ------------------------------------------------------------
V <- list()                      # list to hold value function at each period
V[[T + 1]] <- pi(S)              # terminal period t=2, given in problem
names(V[[T + 1]]) <- S

# Initialize policy storage (same structure as V)
policy <- list()
policy[[T + 1]] <- rep(NA, length(S))  # no decisions at terminal period

# ------------------------------------------------------------
# Step 2: Backward recursion over time
# ------------------------------------------------------------
for (t in T:1) {
  V[[t]] <- numeric(length(S))
  policy[[t]] <- numeric(length(S))
  
  for (s in S) {
    # Feasible extractions: x = 0,1,...,s
    feasible_x <- 0:s
    
    # For each feasible extraction, compute value
    total_value <- numeric(length(feasible_x))
    for (i in seq_along(feasible_x)) {
      x <- feasible_x[i]
      s_next <- s - x
      total_value[i] <- pi(x) + beta * V[[t + 1]][s_next + 1]
    }
    
    # Find maximum value and corresponding extraction
    V[[t]][s + 1] <- max(total_value)
    policy[[t]][s + 1] <- feasible_x[which.max(total_value)]
  }
}

# ------------------------------------------------------------
# Step 3: Combine results into a readable table
# ------------------------------------------------------------
value_table <- data.frame(
  s = S,
  V0 = round(V[[1]], 3),
  V1 = round(V[[2]], 3),
  V2 = round(V[[3]], 3)
)

policy_table <- data.frame(
  s = S,
  x0 = policy[[1]],
  x1 = policy[[2]],
  x2 = S
  )

value_table
policy_table
# The policy table tells you the optimal extraction x_t for each stock level s_t at each time t. If we start at s=4, we extract 2 units at t=0, then 1 units at t=1, then 1 unit at the final T=2 (since there is no value beyond T=2. We trace across the elements of the policy table to see this optimal path.

# Example: Optimal extraction path starting from s=4
s_current <- 4
for (t in 0:T) {
  x_optimal <- policy_table[s_current+1,t + 2]
  cat("At time", t, "with stock", s_current, "extract", x_optimal, "\n")
  s_current <- s_current - x_optimal
}
# Plot of the optimal extraction path
library(ggplot2)
extraction_path <- data.frame(
  time = 0:T,
  stock = numeric(T + 1),
  extraction = numeric(T + 1)
)
s_current <- 4
for (t in 0:T) {
  x_optimal <- policy_table[s_current + 1, t + 2]
  extraction_path$stock[t + 1] <- s_current
  extraction_path$extraction[t + 1] <- x_optimal
  s_current <- s_current - x_optimal
}
ggplot(extraction_path, aes(x = time)) +
  geom_line(aes(y = stock, color = "Stock Level")) +
  geom_point(aes(y = stock, color = "Stock Level")) +
  geom_bar(aes(y = extraction, fill = "Extraction"), stat = "identity", alpha = 0.5) +
  labs(title = "Optimal Extraction Path Over Time",
       y = "Quantity",
       color = "Legend",
       fill = "Legend") +
  scale_color_manual(values = c("Stock Level" = "blue")) +
  scale_fill_manual(values = c("Extraction" = "orange")) +
  theme_minimal()

# Create a plot showing the optimal decisions over time using the value function. Highlight the path x0=2, x1=1, and x2=1 starting from s=4.
value_df <- data.frame(
  stock = rep(S, times = T + 1),
  time = rep(0:T, each = length(S)),
  value = unlist(lapply(V, function(v) v))
)

ggplot(value_df, aes(x = stock, y = value, color = factor(time))) +
  geom_line() +
  geom_point() +
  geom_line(data = data.frame(stock = c(4, 2, 1), time = c(0, 1, 2),
                                       value = c(V[[1]][5], V[[2]][3], V[[3]][2])),
             aes(x = stock, y = value), color = "purple", size = 2,alpha=.5) +
  labs(title = "Value Function Over Time",
       x = "Stock Level",
       y = "Value",
       color = "Time Period") +
  theme_minimal()
# The plot shows that the value of having 1 is the same regardless of time because you extract it asap. The value of having 4 is much higher at t=0 than at t=1 or t=2 because you have more time to extract and earn profits from it.


