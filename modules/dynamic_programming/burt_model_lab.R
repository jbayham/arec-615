# Groundwater management with value function iteration
# AREC 615: run sections interactively, then change parameters and rerun.
# Illustrative units: storage/extraction in million acre-feet; benefits and
# costs in million dollars per period. Parameters are not empirical estimates.
# Requires ggplot2. From the repository root:
#   Rscript modules/dynamic_programming/Bert_Model_Lab.R

library(ggplot2)

# 1. Parameters -----------------------------------------------------------
a <- 10             # scale of benefits from water use
c0 <- 0.5           # unit pumping cost at full storage
c1 <- 2             # sensitivity of unit pumping cost to depletion
c2 <- 0.5           # increasing marginal cost of extraction within a period
Sbar <- 10          # aquifer capacity
epsilon <- 1        # keeps pumping costs finite at zero storage
w <- 0.5            # natural recharge arriving AFTER extraction
beta <- 0.95        # discount factor
r <- 1 / beta - 1
# Baseline: interior long-run storage near 5.7, with extraction equal to
# recharge (0.5). Zero extraction at low stocks is an economically meaningful
# corner; interiority is intended at the steady state, not at every stock.

# Numerical settings: refine both grids to assess discretization error.
Ns <- 401
Nx <- 401
S <- seq(0, Sbar, length.out = Ns)
X <- seq(0, Sbar, length.out = Nx)
tol <- 1e-7         # stopping tolerance for successive value functions
max_iter <- 2000
n_periods <- 80
initial_stocks <- c(2, 8)
make_plots <- interactive()  # set TRUE to draw plots when running sections

stopifnot(a > 0, c0 >= 0, c1 > 0, c2 >= 0, Sbar > 0,
          epsilon > 0, w > 0, w < Sbar, beta > 0, beta < 1,
          Ns >= 2, Nx >= 2, tol > 0,
          all(initial_stocks >= 0 & initial_stocks <= Sbar))

# 2. Economic and hydrological functions ---------------------------------
benefit <- function(x) a * log1p(x)
unit_cost <- function(s) c0 + c1 * (Sbar - s) / (s + epsilon)
cost <- function(x, s) unit_cost(s) * x + 0.5 * c2 * x^2
profit <- function(x, s) benefit(x) - cost(x, s)
next_stock <- function(s, x) pmin(Sbar, s - x + w)
# Extraction is constrained to [0,s]. Recharge exceeding capacity spills.
# Bellman equation: V(s) = max_x {profit(x,s) + beta*V(next_stock(s,x))}.
# Bounded rewards and beta < 1 make the Bellman operator a contraction.
# Linear interpolation uses convex weights, preserving this property for
# the discretized operator. No extrapolation or rounding of next stock.

# 3. Initialize, then iterate ---------------------------------------------
V <- rep(0, Ns)
policy <- rep(0, Ns)
V_history <- matrix(NA_real_, nrow = Ns, ncol = max_iter + 1)
V_history[, 1] <- V
changes <- numeric(max_iter)
diff <- Inf
iter <- 0

while (diff > tol && iter < max_iter) {
  V_new <- numeric(Ns)
  for (i in seq_along(S)) {
    s <- S[i]
    # Include x=s even when the two grids do not coincide.
    x_feasible <- sort(unique(c(X[X <= s], s)))
    s_next <- next_stock(s, x_feasible)
    continuation <- approx(S, V, xout = s_next)$y
    objective <- profit(x_feasible, s) + beta * continuation
    best <- which.max(objective)  # ties go to the smaller extraction
    V_new[i] <- objective[best]
    policy[i] <- x_feasible[best]
  }
  diff <- max(abs(V_new - V))
  V <- V_new
  iter <- iter + 1
  changes[iter] <- diff
  V_history[, iter + 1] <- V
  if (iter == 1 || iter %% 50 == 0) {
    cat(sprintf("Iteration %3d: maximum value change = %.8f\n", iter, diff))
  }
}
if (diff > tol) stop("VFI did not converge: increase max_iter or inspect inputs.")
changes <- changes[seq_len(iter)]
V_history <- V_history[, seq_len(iter + 1), drop = FALSE]

# Recover a policy with respect to the FINAL value function, and
# check the Bellman residual (distinct from the successive-iteration change).
T_V <- numeric(Ns)
myopic_policy <- numeric(Ns)
for (i in seq_along(S)) {
  s <- S[i]
  x_feasible <- sort(unique(c(X[X <= s], s)))
  current_profit <- profit(x_feasible, s)
  objective <- current_profit + beta *
    approx(S, V, xout = next_stock(s, x_feasible))$y
  best <- which.max(objective)
  policy[i] <- x_feasible[best]
  T_V[i] <- objective[best]
  myopic_policy[i] <- x_feasible[which.max(current_profit)]
}
bellman_residual <- max(abs(T_V - V))
value_error_bound <- bellman_residual / (1 - beta)
# This bound concerns the grid Bellman equation, NOT discretization error.
stopifnot(all(is.finite(V)), all(policy >= 0), all(policy <= S),
          bellman_residual <= tol)
results <- data.frame(stock = S, value = V, extraction = policy,
                      myopic_extraction = myopic_policy,
                      next_stock = next_stock(S, policy))
cat(sprintf("Converged in %d iterations; Bellman residual = %.3g.\n",
            iter, bellman_residual))
cat(sprintf("Value error bound for grid problem = %.3g.\n", value_error_bound))

# 4. Simulate the policy from low and high initial storage -----------------
# Interpolate the policy between storage nodes. Clip only numerical roundoff
# or interpolation error to maintain feasibility. Recompute actual transitions.
simulate_path <- function(s0, extraction_policy) {
  storage <- numeric(n_periods + 1)
  extraction <- net_benefit <- spill <- numeric(n_periods)
  storage[1] <- s0
  for (t in seq_len(n_periods)) {
    s <- storage[t]
    x <- approx(S, extraction_policy, xout = s)$y
    x <- max(0, min(s, x))
    extraction[t] <- x
    net_benefit[t] <- profit(x, s)
    spill[t] <- max(0, s - x + w - Sbar)
    storage[t + 1] <- next_stock(s, x)
  }
  data.frame(period = 0:n_periods, stock = storage,
             extraction = c(extraction, NA_real_),
             net_benefit = c(net_benefit, NA_real_),
             spill = c(spill, NA_real_))
}
paths <- lapply(initial_stocks, simulate_path, extraction_policy = policy)
myopic_paths <- lapply(initial_stocks, simulate_path,
                       extraction_policy = myopic_policy)
path_summary <- data.frame(
  initial_stock = initial_stocks,
  final_stock = vapply(paths, function(p) tail(p$stock, 1), numeric(1)),
  final_extraction = vapply(paths, function(p) tail(p$extraction, 2)[1], numeric(1)),
  myopic_final_stock = vapply(myopic_paths, function(p) tail(p$stock, 1), numeric(1))
)
print(path_summary, row.names = FALSE)
# With discrete controls, x=w can hold over a small stock interval. Do not
# interpret that grid-induced interval as evidence of a unique exact steady state.
steady_grid <- S[abs(policy - w) < 1e-10 & S > 0 & S < Sbar]
if (length(steady_grid) > 0) {
  cat(sprintf("Grid stocks with extraction = recharge: %.3f to %.3f.\n",
              min(steady_grid), max(steady_grid)))
}

# 5. Plot results with ggplot2 --------------------------------------------
# Plot objects are always created so they can be inspected individually or
# saved with ggsave(). make_plots controls automatic display only.
plot_theme <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

policy_data <- rbind(
  data.frame(stock = S, extraction = policy, rule = "Dynamic"),
  data.frame(stock = S, extraction = myopic_policy, rule = "Myopic")
)
path_data <- do.call(rbind, lapply(seq_along(paths), function(i) {
  transform(paths[[i]], initial_stock = paste("Initial stock", initial_stocks[i]))
}))

value_plot <- ggplot(results, aes(stock, value)) +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(title = "Optimal value function", x = "Groundwater storage",
       y = "Present value of net benefits") + plot_theme

policy_plot <- ggplot(policy_data, aes(stock, extraction, color = rule,
                                       linetype = rule)) +
  geom_hline(yintercept = w, linetype = "dotted", color = "gray40") +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(Dynamic = "steelblue", Myopic = "gray40")) +
  scale_linetype_manual(values = c(Dynamic = "solid", Myopic = "dashed")) +
  labs(title = "Dynamic versus myopic extraction",
       subtitle = "Dotted line: natural recharge", x = "Groundwater storage",
       y = "Extraction per period", color = NULL, linetype = NULL) + plot_theme

storage_plot <- ggplot(path_data, aes(period, stock, color = initial_stock)) +
  geom_line(linewidth = 1) +
  coord_cartesian(ylim = c(0, Sbar)) +
  labs(title = "Storage paths under the dynamic policy", x = "Period",
       y = "Groundwater storage", color = NULL) + plot_theme

extraction_plot <- ggplot(subset(path_data, !is.na(extraction)),
                          aes(period, extraction, color = initial_stock)) +
  geom_hline(yintercept = w, linetype = "dotted", color = "gray40") +
  geom_line(linewidth = 1) +
  labs(title = "Extraction paths under the dynamic policy",
       subtitle = "Dotted line: natural recharge", x = "Period",
       y = "Extraction per period", color = NULL) + plot_theme

if (make_plots) {
  print(value_plot)
  print(policy_plot)
  print(storage_plot)
  print(extraction_plot)
}

# 6. Discrete-time phase diagram with cobweb paths -------------------------
# Plot s_(t+1) against s_t. Above the identity line storage rises; below
# it storage falls. Intersections are fixed points of the grid policy.
phase_data <- results
fixed_points <- subset(phase_data, abs(next_stock - stock) < 1e-10)

# Start each cobweb on the diagonal at (s_0, s_0). Each period consists of:
#   1. a vertical move from (s_t, s_t) to (s_t, s_(t+1)) on the map;
#   2. a horizontal move to (s_(t+1), s_(t+1)) on the diagonal.
# Reuse the simulated paths, including the capacity limit and interpolated
# policy, rather than constructing an unrelated illustrative trajectory.
cobweb_data <- do.call(rbind, lapply(seq_along(paths), function(i) {
  s_now <- head(paths[[i]]$stock, -1)
  s_next <- tail(paths[[i]]$stock, -1)
  label <- paste("Initial stock", initial_stocks[i])
  rbind(
    data.frame(period = seq_along(s_now) - 1, initial_stock = label,
               leg = "vertical", x = s_now, y = s_now,
               xend = s_now, yend = s_next),
    data.frame(period = seq_along(s_now) - 1, initial_stock = label,
               leg = "horizontal", x = s_now, y = s_next,
               xend = s_next, yend = s_next)
  )
}))
# Omit zero-length segments once a trajectory has reached a grid steady state.
cobweb_data <- subset(cobweb_data, abs(xend - x) + abs(yend - y) > 1e-10)

phase_plot <- ggplot(phase_data, aes(stock, next_stock)) +
  geom_abline(intercept = 0, slope = 1, color = "gray40", linetype = "dashed") +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_segment(data = cobweb_data,
               aes(x = x, y = y, xend = xend, yend = yend,
                   color = initial_stock),
               inherit.aes = FALSE, linewidth = 0.55,
               arrow = grid::arrow(length = grid::unit(0.045, "inches"))) +
  geom_point(data = fixed_points, color = "darkorange", size = 2) +
  coord_equal(xlim = c(0, Sbar), ylim = c(0, Sbar)) +
  labs(title = "Groundwater transition map and cobweb paths",
       subtitle = "Blue: optimal transition map. Dashed: next storage = current storage.",
       x = expression("Current groundwater storage, " * s[t]),
       y = expression("Next-period groundwater storage, " * s[t+1]),
       color = NULL,
       caption = paste("Arrows follow successive periods.",
                       "\nOrange points mark grid steady states at intersections with the diagonal.")) +
  plot_theme

if (make_plots) print(phase_plot)

# Try next: change beta, recharge w, or depletion sensitivity c1. Refine Ns
# and Nx together. Stochastic recharge replaces continuation by a weighted
# average over next stocks; persistent drought requires a second state.
