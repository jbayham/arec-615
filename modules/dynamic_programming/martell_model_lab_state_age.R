# Martell (1980): stand age as the state, harvest or wait as the decision
# AREC 615. Run the numbered sections interactively in RStudio.
# Companion notes: martell.qmd. Requires ggplot2; numerical work uses base R.
# From the repository root: Rscript modules/dynamic_programming/martell_model_lab_state_age.R
# No downloads or working-directory changes are needed.
# Martell's notation: i is a PERCENT (5, not .05), alpha is the discount factor,
# PI[j]: conditional probability a stand grown for j-1 years ignites and burns
# during growing year j, with no harvesting; PIH[j]: the same with harvesting.
# P[j]: probability the stand survives years 1,...,j-1 and burns in year j.
# R[j]: net revenue received at year-end if harvested and not burned in year j.
# S[j]: net revenue received at year-end if burned and salvaged in year j.
# f: soil expectation value, the expected present value of all future stands,
# starting with bare land, under an optimal rotation policy over an infinite horizon.
# k: planned rotation interval (years); harvest occurs during growing year k.
# All money is dollars per hectare unless otherwise noted.

library(ggplot2)
make_plots <- interactive()

# 1. Martell's simplified jack pine example (p. 32) ------------------------
i <- 5
alpha <- 1 / (1 + i / 100)
N <- 100                       # maximum allowable rotation (years); within yield-curve range
j <- seq_len(N)                 # growing year, starting at 1
A <- 0:(N - 1)                  # completed age at START of that year: A = j - 1
v <- 2                         # net return per cubic metre of wood ($/m^3)
C <- 0                         # cost of planting the stand, paid at each cycle's start
PI <- 0.025                    # baseline: one of Martell's Table 1 scenarios
PIH <- PI                      # same risk during a harvest year
S <- rep(0, N)                  # S[j] = net revenue if burned and salvaged during year j

# GMV: gross merchantable volume (cubic metres per hectare); A: age (years).
GMV <- function(A) {
  stopifnot(all(is.finite(A)), all(A >= 0 & A <= 100))
  volume <- numeric(length(A))
  positive <- A > 22
  volume[positive] <- 397.49 - 1865.5 * A[positive]^(-0.5)
  volume
}
R <- v * GMV(j)                # R[j]: net revenue if harvested and not burned during year j
# Note the indexing: beginning age 30 -> growing year 31 -> net harvest revenue R[31].
# For ages <=22 use Martell's explicit zero-volume convention, not extrapolation.
stopifnot(alpha > 0, alpha < 1, N >= 2, N <= 100)

hazard_vector <- function(x, N) {
  stopifnot(length(x) %in% c(1L, N), all(is.finite(x)), all(x >= 0 & x <= 1))
  rep(x, length.out = N)
}

# 2. Stand age as the state: harvest or wait each year ---------------------
# f_A[A+1] is the value of an ALREADY PLANTED stand at beginning age A.
# Bare-land value is f = f_A[1] - C. Avoid charging C twice at a reset.
# Harvest: burn -> S[j]; survive -> R[j]; both reset to bare land next year.
# Wait: burn -> S[j] and reset; survive -> beginning age A+1 next year.
# All year-end net revenues and reset values are discounted by alpha. Harvest is mandatory
# in growing year N, matching the rotation search's maximum admissible k=N.
age_actions <- function(f_A, R, S, C, alpha, PI, PIH) {
  N <- length(R)
  f <- f_A[1] - C
  Q_H <- alpha * ((1 - PIH) * R + PIH * S + f)
  Q_W <- rep(-Inf, N)           # waiting not admissible at A=N-1
  jj <- seq_len(N - 1)
  Q_W[jj] <- alpha * ((1 - PI[jj]) * f_A[jj + 1] + PI[jj] * (S[jj] + f))
  list(Q_H = Q_H, Q_W = Q_W)
}
solve_age <- function(R, S, C, alpha, PI, PIH = PI, tol = 1e-10,
                      max_iter = 10000) {
  N <- length(R)
  stopifnot(N >= 2, length(S) == N, length(C) == 1, C >= 0,
            all(is.finite(c(R, S, C))), alpha > 0, alpha < 1, tol > 0)
  PI <- hazard_vector(PI, N)
  PIH <- hazard_vector(PIH, N)
  f_A <- numeric(N)
  changes <- numeric(max_iter)
  for (iter in seq_len(max_iter)) {
    Q <- age_actions(f_A, R, S, C, alpha, PI, PIH)
    f_new <- pmax(Q$Q_H, Q$Q_W)
    changes[iter] <- max(abs(f_new - f_A))
    f_A <- f_new
    if (changes[iter] < tol) break
  }
  if (changes[iter] >= tol) stop("Age-state iteration did not converge.")
  # Recover decisions from the FINAL value, not the penultimate iteration.
  Q <- age_actions(f_A, R, S, C, alpha, PI, PIH)
  u <- as.integer(Q$Q_H >= Q$Q_W)  # harvest on exact ties
  residual <- max(abs(pmax(Q$Q_H, Q$Q_W) - f_A))
  list(A = 0:(N - 1), f_A = f_A, f = f_A[1] - C, u = u,
       Q_H = Q$Q_H, Q_W = Q$Q_W,
       k_star = which(u == 1)[1], iter = iter,
       residual = residual, error_bound = residual / (1 - alpha),
       changes = changes[seq_len(iter)])
}
age <- solve_age(R, S, C, alpha, PI, PIH)
f_A <- age$f_A
u <- age$u
Q_H <- age$Q_H
Q_W <- age$Q_W
k_star <- age$k_star
f <- age$f
cat(sprintf("Age-state solution: k* = %d growing years; f = %.6f $/ha.\n", k_star, f))
cat(sprintf("DP: %d iterations; Bellman residual %.3g; value-error bound %.3g.\n",
            age$iter, age$residual, age$error_bound))
stopifnot(age$residual < 1e-10, tail(u, 1) == 1)
# PAUSE: k*=31 means choose harvest at beginning age A=30, not age 31.
# Starting from a planted stand, the first harvest state determines the planned
# rotation. Do not assume arbitrary extensions give a monotone policy at ALL ages.

# 3. Reproduce Table 1 at every fire probability --
PI_table <- c(0, .0005, .001, .0015, .002, .0025, .01, .015, .0175,
              .02, .0225, .025, .03, .035, .04, .045, .05)
Martell_f <- c(36.47, 36.07, 35.68, 35.28, 34.89, 34.51, 29.24, 26.17,
               24.76, 23.42, 22.14, 20.93, 18.73, 16.72, 14.94, 13.35, 11.89)
Martell_k <- c(rep(34, 6), 33, rep(32, 4), rep(31, 3), rep(30, 3))
# Explicit original-paper inputs: Table 1 stays a replication if students change
# their baseline parameters in section 1. Use a separate experiment for extensions.
R_paper <- 2 * GMV(seq_len(100))
S_paper <- rep(0, 100)
alpha_paper <- 1 / 1.05
table1 <- do.call(rbind, lapply(PI_table, function(PI) {
  dp <- solve_age(R_paper, S_paper, 0, alpha_paper, PI)
  data.frame(PI = PI, f = dp$f, k_star = dp$k_star)
}))
table1$Martell_f <- Martell_f
table1$Martell_k <- Martell_k
table1$difference <- table1$f - table1$Martell_f
# Two Table 1 values differ by one cent after rounding; retain the printed
# inputs and report the differences instead of tuning the yield curve.
print(table1, digits = 6, row.names = FALSE)
stopifnot(max(abs(table1$f - Martell_f)) < .01, all(table1$k_star == Martell_k))

# 4. Reproduce Table 2: annual timber benefits of fire management ----------
# B: annual fire management expenditure, dollars per unit area (here per hectare).
# f(B): soil expectation value given expenditure B and an optimal rotation policy.
# EPV(B): expected present value of fire management activities = f(B)-f(0).
# A(B): equivalent average annual return = (1-alpha)*EPV(B).
# Here f(0) means ZERO MANAGEMENT SPENDING, not necessarily zero fire risk.
# A(B) is a return FUNCTION; A alone remains stand age.
# Annual benefits/payments are at the START of each year, including today.
PI_without <- PI_table[PI_table >= .01]
PI_with <- c(0, .0005, .001, .0015, .002, .0025)
f_with <- table1$f[match(PI_with, table1$PI)]
f_without <- table1$f[match(PI_without, table1$PI)]
A_B <- (1 - alpha_paper) * outer(f_without, f_with, function(x, y) y - x)
dimnames(A_B) <- list(sprintf("%.4f", PI_without), sprintf("%.4f", PI_with))
print(round(A_B, 2))

# Keep Table 2's row/column order and show its values on a common color scale.
benefit_data <- expand.grid(PI_without = rownames(A_B), PI_with = colnames(A_B),
                            stringsAsFactors = FALSE)
benefit_data$A_B <- as.vector(A_B)
benefit_data$PI_without <- factor(benefit_data$PI_without,
                                  levels = rev(rownames(A_B)))
benefit_data$PI_with <- factor(benefit_data$PI_with, levels = colnames(A_B))
benefit_plot <- ggplot(benefit_data, aes(PI_with, PI_without, fill = A_B)) +
  geom_tile(color = "white", linewidth = .4) +
  geom_text(aes(label = sprintf("%.2f", A_B),
                color = A_B >= .75), size = 3.4, show.legend = FALSE) +
  scale_color_manual(values = c("FALSE" = "#172B4D", "TRUE" = "white")) +
  scale_fill_gradient(low = "#EFF6FC", high = "#084594",
                      name = "Annual benefit ($/ha)") +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "PI with fire management", y = "PI without fire management") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), legend.position = "right",
        axis.text = element_text(color = "#172B4D"),
        axis.ticks = element_blank())
if (make_plots) print(benefit_plot)
Martell_A_B <- rbind(
  c(.34,.33,.31,.29,.27,.25), c(.49,.47,.45,.43,.42,.40),
  c(.56,.54,.52,.50,.48,.46), c(.62,.60,.58,.56,.55,.53),
  c(.68,.66,.64,.63,.61,.59), c(.74,.72,.70,.68,.66,.65),
  c(.84,.83,.81,.79,.77,.75), c(.94,.92,.91,.89,.87,.85),
  c(1.02,1.01,.99,.97,.95,.93), c(1.10,1.08,1.06,1.05,1.03,1.01),
  c(1.17,1.15,1.13,1.11,1.09,1.08))
stopifnot(max(abs(A_B - Martell_A_B)) < .01)
B <- 0.48                      # Martell's illustrative annual fire management expenditure
EPV_B <- table1$f[match(.0015, table1$PI)] - table1$f[match(.025, table1$PI)]
protection_example <- data.frame(EPV_B = EPV_B, A_B = (1-alpha_paper)*EPV_B,
                                 B = B, annual_net_benefit = (1-alpha_paper)*EPV_B-B)
print(protection_example)

# 5. Simulate the annual harvest policy -----------------------------------
# This simulation uses the section 1 parameters and the solved annual policy.
# 'no_fire' illustrates the planned cycle, NOT a reoptimization at PI=0.
simulate_policy <- function(u, R, S, C, PI, PIH, years = 160, no_fire = FALSE) {
  N <- length(u)
  PI <- hazard_vector(PI, N)
  PIH <- hazard_vector(PIH, N)
  A <- 0
  out <- vector("list", years)
  for (t in seq_len(years)) {
    j <- A + 1
    harvest <- u[j] == 1
    probability <- if (harvest) PIH[j] else PI[j]
    burns <- if (no_fire) FALSE else runif(1) < probability
    revenue <- if (burns) S[j] else if (harvest) R[j] else 0
    reset <- harvest || burns
    A_next <- if (reset) 0 else A + 1
    out[[t]] <- data.frame(t = t, A = A, u = u[j], burns = burns,
                           revenue = revenue, A_next = A_next,
                           planting_next = if (reset) C else 0,
                           event = if (burns) "Fire" else if (harvest) "Harvest" else "Wait")
    A <- A_next
  }
  do.call(rbind, out)
}
set.seed(1980)
path <- simulate_policy(u, R, S, C, PI, PIH)
planned_path <- simulate_policy(u, R, S, C, PI, PIH, no_fire = TRUE)
print(subset(path, event != "Wait"), row.names = FALSE)
# Initial planting cost C occurs at time 0; planting_next is paid at time t,
# the same boundary as year t's revenue and the start of year t+1.

# 6. Plot the annual solution -------------------------------------------
plot_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
age_data <- data.frame(A = A, f_A = f_A, u = u, Q_H = Q_H, Q_W = Q_W)
value_plot <- ggplot(age_data, aes(A, f_A)) +
  geom_line(color = "steelblue", linewidth = .9) +
  labs(x = "Beginning-of-year stand age A", y = "Established-stand value f_A ($/ha)",
       title = "Age-state value function") + plot_theme
# Focus near the decision boundary; exclude forced-harvest N where Q_W=-Inf.
action_data <- rbind(data.frame(A = A, value = Q_H, action = "Harvest"),
                     data.frame(A = A, value = Q_W, action = "Wait"))
action_data <- subset(action_data, is.finite(value) & abs(A - (k_star - 1)) <= 10)
action_plot <- ggplot(action_data, aes(A, value, color = action)) +
  geom_line(linewidth = .9) + geom_point(size = 1.5) +
  geom_vline(xintercept = k_star - 1, linetype = "dashed") +
  labs(x = "Beginning-of-year stand age A", y = "Action value ($/ha)",
       title = "Harvest when Q_H(A) reaches or exceeds Q_W(A)", color = NULL) + plot_theme
policy_plot <- ggplot(age_data, aes(A, u)) +
  geom_step(color = "steelblue", linewidth = .9) +
  scale_y_continuous(breaks = c(0, 1), labels = c("Wait", "Harvest")) +
  labs(x = "Beginning-of-year stand age A", y = NULL,
       title = "Optimal annual harvest decision") + plot_theme
path_data <- rbind(transform(path, scenario = "With fire"),
                   transform(planned_path, scenario = "No-fire realization"))
path_plot <- ggplot(path_data, aes(t - 1, A)) +
  geom_step(color = "steelblue", linewidth = .65) +
  geom_point(data = subset(path_data, event != "Wait"), aes(color = event), size = 2) +
  facet_wrap(~scenario, ncol = 1) +
  labs(x = "Calendar time at start of year", y = "Stand age A", color = NULL,
       title = "The same harvest policy along two realized paths",
       caption = "Points mark the start of years ending in fire or harvest; age resets at the next boundary.") + plot_theme
if (make_plots) {
  print(value_plot)
  print(action_plot)
  print(policy_plot)
  print(path_plot)
}

# 7. Independent validation of the annual policy --------------------------
# Evaluate the recovered policy by solving a linear system, not by iterating.
# For planted states: f_A = alpha * (expected net revenue - reset cost + M f_A).
# Each row of M is a probability distribution over next year's planted ages.
PI_check <- hazard_vector(PI, N)
PIH_check <- hazard_vector(PIH, N)
M <- matrix(0, N, N)
g <- numeric(N)
for (j in seq_len(N)) {
  if (u[j] == 1) {
    M[j, 1] <- 1
    g[j] <- (1 - PIH_check[j]) * R[j] + PIH_check[j] * S[j] - C
  } else {
    M[j, 1] <- PI_check[j]
    M[j, j + 1] <- 1 - PI_check[j]
    g[j] <- PI_check[j] * (S[j] - C)
  }
}
f_policy <- as.vector(solve(diag(N) - alpha * M, alpha * g))
stopifnot(max(abs(rowSums(M) - 1)) < 1e-12, max(abs(f_policy - f_A)) < 1e-7)

cat("All Table 1 rotations match and table values agree within $0.01.\n")
# Investigations: change section 1 and rerun the solution and plots.
# Tables in sections 3-4 retain the original-paper inputs for replication.
# 1. Raise C or allow S[j]>0. Explain how these change the cost of a reset.
# 2. Let PI increase with j, or make PIH>PI. Does harvesting still avoid risk?
# 3. Raise i (in percent). Compare both k* and f, not just the policy.
