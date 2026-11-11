# Martell (1980): the optimal rotation of a flammable forest stand
# AREC 615. Run the numbered sections interactively in RStudio.
# Companion notes: martell.qmd. Requires ggplot2; numerical work uses base R.
# From the repository root: Rscript modules/dynamic_programming/martell_model_lab.R
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

# 2. The paper's regeneration solution: enumerate k = 1,...,N -------------
# This function retains C, S, and age-/action-dependent probabilities so the
# simplified example can later be extended. Supply scalar fire probabilities or N-vectors.
hazard_vector <- function(x, N) {
  stopifnot(length(x) %in% c(1L, N), all(is.finite(x)), all(x >= 0 & x <= 1))
  rep(x, length.out = N)
}
rotation_values <- function(R, S, C, alpha, PI, PIH = PI) {
  N <- length(R)
  stopifnot(N >= 2, length(S) == N, all(is.finite(c(R, S, C))),
            length(C) == 1, C >= 0, alpha > 0, alpha < 1)
  PI <- hazard_vector(PI, N)
  PIH <- hazard_vector(PIH, N)
  E1 <- E2 <- f_k <- numeric(N)
  for (k in seq_len(N)) {
    # Probability of entering each year without any earlier fire.
    survival_start <- c(1, cumprod(1 - PI[seq_len(k - 1)]))
    P <- survival_start * c(PI[seq_len(k - 1)], PIH[k])
    # P[j] is unconditional; in the harvest year it uses PIH[k].
    survival_harvest <- survival_start[k] * (1 - PIH[k])
    stopifnot(abs(sum(P) + survival_harvest - 1) < 1e-12)
    E1[k] <- survival_harvest * alpha^k + sum(alpha^seq_len(k) * P)
    E2[k] <- survival_harvest * alpha^k * R[k] +
      sum(alpha^seq_len(k) * S[seq_len(k)] * P)
    f_k[k] <- (E2[k] - C) / (1 - E1[k])  # Martell's equation (8)
  }
  data.frame(k = seq_len(N), E1 = E1, E2 = E2, f_k = f_k)
}
rotation <- rotation_values(R, S, C, alpha, PI, PIH)
k_star <- rotation$k[which.max(rotation$f_k)]  # earliest maximum if exact tie
f <- max(rotation$f_k)
print(rotation[rotation$k %in% (k_star + (-2:2)), ], digits = 7, row.names = FALSE)
cat(sprintf("Regeneration solution: k* = %d growing years; f = %.6f $/ha.\n", k_star, f))

# Inspect the components of the winning cycle. Future cycles contribute E1*f.
cycle_accounting <- data.frame(C = C, E2 = rotation$E2[k_star],
                               continuation = rotation$E1[k_star] * f, f = f)
print(cycle_accounting)
stopifnot(abs(-C + cycle_accounting$E2 + cycle_accounting$continuation - f) < 1e-10)
# PAUSE: Why do fire outcomes contain continuation value even when S[j]=0?

# Closed-form shortcut for the simplified model only: C=0, S=0, PIH=PI constant.
# This is derived in the notes; the general routine above does not rely on it.
simplified_values <- function(R, alpha, PI) {
  k <- seq_along(R)
  b <- alpha * (1 - PI)         # algebra helper, not a new economic parameter
  R * b^k * (1 - b) / ((1 - alpha) * (1 - b^k))
}
if (C == 0 && all(S == 0) && length(PI) == 1 && identical(PIH, PI)) {
  stopifnot(max(abs(rotation$f_k - simplified_values(R, alpha, PI))) < 1e-10)
}

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
  rot <- rotation_values(R_paper, S_paper, 0, alpha_paper, PI)
  data.frame(PI = PI, f = max(rot$f_k), k_star = which.max(rot$f_k))
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

# 5. Plot the planned-rotation values -----------------------------------
plot_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")
rotation_plot <- ggplot(rotation, aes(k, f_k)) +
  geom_line(color = "steelblue", linewidth = .9) +
  geom_vline(xintercept = k_star, linetype = "dashed") +
  geom_point(data = rotation[k_star, ], color = "darkorange", size = 3) +
  labs(x = "Planned rotation k (growing years)", y = "Soil expectation value f_k ($/ha)",
       title = "Choose k to maximize Martell's equation (8)") + plot_theme
if (make_plots) print(rotation_plot)

cat("All Table 1 rotations match and table values agree within $0.01.\n")
# Investigations: change section 1 and rerun the solution and plots.
# Tables in sections 3-4 retain the original-paper inputs for replication.
# 1. Raise C or allow S[j]>0. Explain how these change the cost of a reset.
# 2. Let PI increase with j, or make PIH>PI. Does harvesting still avoid risk?
# 3. Raise i (in percent). Compare both k* and f, not just the policy.
