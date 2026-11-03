# -------------------------------------------------------------
# Example: Discrete-time dynamic programming (forest harvest)
# -------------------------------------------------------------
# State variable: forest age
# Decision variable: harvest (u = 1) or wait (u = 0)
# Horizon: T = 3 periods
# -------------------------------------------------------------

# ------------------------
# Define model parameters
# ------------------------

# State variable: possible ages of the forest
x <- matrix(c(0, 50, 100), nrow = 1, ncol = 3)

# Decision variable: 0 = do not cut, 1 = cut
u <- matrix(c(0, 1), nrow = 1, ncol = 2)

# Immediate payoff if cut at each age (indexed by state)
pay <- matrix(c(-2, 8, 10), nrow = 1, ncol = 3)

# Salvage value if left standing at terminal period (also indexed by state)
salv <- matrix(c(0, 20, 60), nrow = 1, ncol = 3)

# Discount factor: 50 years at 4% discount rate
beta <- (1 / (1.04))^50

# Total number of periods
T <- 3

# ------------------------
# Terminal period (t = T)
# ------------------------
# In the final period, the decision is to cut or not cut.
# Payoff matrix: rows = states, columns = decisions
uT <- cbind(t(salv), t(pay))

# Take the maximum over decisions for each state
rowmax <- apply(uT, 1, max) #apply function row-wise
loc <- apply(uT, 1, which.max)

# Value function matrix: rows = time, columns = state
V <- matrix(NA, ncol = length(x), nrow = T)
V[T, ] <- rowmax

# Policy index matrix: which decision gives the max
I <- matrix(NA, ncol = length(x), nrow = T)
I[T, ] <- loc

# Store the optimal policy (actual control variable)
pol <- matrix(NA, nrow = T, ncol = length(x))
pol[T, ] <- u[I[T, ]]
pol  # Optimal decision in final period

# ------------------------
# Transition dynamics
# ------------------------
# newstate(j, k) = next state given current state j and control k
# If not cut (u=0), forest ages by 50 years (up to 100).
# If cut (u=1), forest resets to 0 (replanting).
newstate <- matrix(NA, nrow = length(x), ncol = length(u))
for (j in 1:length(x)) {
  for (k in 1:length(u)) {
    if (u[k] == 0) {
      if (j < 3) {
        newstate[j, k] <- x[j] + 50
      } else {
        newstate[j, k] <- x[j]  # stays at 100 if already mature
      }
    } else {
      newstate[j, k] <- 0  # reset if harvested
    }
  }
}
newstate  # Display transition matrix

# ------------------------
# Backward recursion
# ------------------------
# For t = T-1 down to 1, compute V_t(x) and optimal policy.
for (t in (T - 1):1) {
  ut1 <- matrix(NA, nrow = length(x), ncol = length(u))
  
  for (ii in 1:length(x)) {
    # Value if not cut: no immediate reward, future value discounted
    ut1[ii, 1] <- 0 + beta * V[t + 1, match(newstate[ii, 1], x)]
    
    # Value if cut: get immediate payoff + discounted continuation
    ut1[ii, 2] <- pay[ii] + beta * V[t + 1, match(newstate[ii, 2], x)]
  }
  
  # Optimal value and policy for each state
  V[t, ] <- apply(ut1, 1, max) #the max of each row
  I[t, ] <- apply(ut1, 1, which.max) #the index of the max of each row
  pol[t, ] <- u[I[t, ]] #translate the index to the actual control variable (0 or 1)
  
  rm(ut1)
}

# ------------------------
# Results
# ------------------------
# Optimal policy (cut = 1, wait = 0) by period and state
pol

# Value function by period and state
V

#Convert uT to a data frame and plot using ggplot2
library(tidyverse)
uT_df <- as.data.frame(uT)
colnames(uT_df) <- c("Not_Cut", "Cut")
uT_df$State <- factor(c("Age_0", "Age_050", "Age_100"))
pivot_longer(uT_df, cols = c("Not_Cut", "Cut"), names_to = "Decision", values_to = "Value") %>%
  ggplot(aes(x = State, y = Value, fill = Decision)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cut or Not at Horizon", y = "Payoff", x = "Forest Age State") +
  theme_minimal()

#The salvage value is high so don't cut stands at 50 or 100 in final period.

#Plot the value function over time for each state
V_df <- as.data.frame(V)
colnames(V_df) <- c("Age_0", "Age_050", "Age_100")
V_df$Period <- factor(1:T)
pivot_longer(V_df, cols = c("Age_0", "Age_050", "Age_100"), names_to = "State", values_to = "Value") %>%
  ggplot(aes(x = Period, y = Value, color = State, group = State)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Value Function Over Time", y = "Value", x = "Period") +
  theme_minimal()


# Compare the optimal decision and its value on the same date-state grid.
# V includes current payoff and discounted future payoffs, valued at each date.
policy_value_df <- expand.grid(Period = seq_len(T), Age = as.vector(x)) %>%
  mutate(
    Policy = as.vector(pol),
    Value = as.vector(V),
    Decision = case_when(
      Policy == 1 ~ "Cut",
      Period == T ~ "Salvage",
      TRUE ~ "Wait"
    ),
    Decision = factor(Decision, levels = c("Wait", "Cut", "Salvage")),
    Period = factor(Period, levels = seq_len(T)),
    Age = factor(Age, levels = as.vector(x))
  )

panel_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

policy_plot <- ggplot(policy_value_df, aes(x = Period, y = Age)) +
  geom_tile(aes(fill = Decision), color = "white", linewidth = 1) +
  geom_text(aes(label = Decision), size = 4) +
  scale_fill_manual(
    values = c("Wait" = "lightblue", "Cut" = "salmon", "Salvage" = "#C7DFC5"),
    drop = FALSE
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  coord_equal() +
  labs(
    title = "Optimal policy",
    subtitle = "Best action at each date and stand age",
    x = "Period", y = "Stand age (years)", fill = "Decision"
  ) +
  panel_theme

value_plot <- ggplot(policy_value_df, aes(x = Period, y = Age)) +
  geom_tile(aes(fill = Value), color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", Value)), size = 4) +
  scale_fill_gradient(low = "#F0F5FA", high = "#7BAACB") +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  coord_equal() +
  labs(
    title = "Optimal payoff: V(t, age)",
    subtitle = "Current plus discounted future payoffs",
    x = "Period", y = "Stand age (years)", fill = "Value"
  ) +
  panel_theme

policy_value_plot <- patchwork::wrap_plots(policy_plot, value_plot, nrow = 1) +
  patchwork::plot_annotation(
    title = "Forest harvest: decisions and values",
    caption = paste(
      "Each cell is a date-state combination, not a trajectory.",
      "Salvage means leave standing and collect terminal value.",
      "Values are measured at each cell's date."
    )
  )
print(policy_value_plot)
