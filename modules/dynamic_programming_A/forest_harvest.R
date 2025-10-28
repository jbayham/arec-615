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
  V[t, ] <- apply(ut1, 1, max)
  I[t, ] <- apply(ut1, 1, which.max) %>% unlist()
  pol[t, ] <- u[I[t, ]]
  
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
library(ggplot2)
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


#Plot the optimal policy over time for each state in a heatmap
pol_df <- as.data.frame(pol)
colnames(pol_df) <- c("Age_0", "Age_050", "Age_100")
pol_df$Period <- factor(1:T)
pivot_longer(pol_df, cols = c("Age_0", "Age_050", "Age_100"), names_to = "State", values_to = "Policy") %>%
  ggplot(aes(y = State, x = Period, fill = factor(Policy))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "salmon"), labels = c("Wait", "Cut")) +
  labs(title = "Optimal Policy Over Time", y = "Period", x = "Forest Age State", fill = "Policy") +
  theme_minimal()
