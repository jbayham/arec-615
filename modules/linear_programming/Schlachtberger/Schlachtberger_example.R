# Schlachtberger et al.: a small electricity-planning LP
# ====================================================
# Run this file a SECTION AT A TIME in RStudio. All intermediate objects remain
# in your Environment pane. There are no custom solution functions.
#
# Question: How do generation, batteries, and trade combine to meet demand?
# Experiment: Change the transmission-volume cap and re-optimize everything.
#
# This is a TEACHING MODEL, not a replication of the European model.
# - Two locations, North (N) and South (S).
# - Two consecutive one-hour periods: sunny and evening.
# - Wind can be built in North; solar can be built in South.
# - Both locations can build gas turbines and batteries.
# - A lossless line connects the two locations.
# - Demand, weather, and costs are known with certainty.
# - All costs are fictional cost units for THIS TWO-HOUR HORIZON.
#   They are NOT annual costs, market prices, or technology cost estimates.
#
# Notation follows the paper:
# G = generation/storage power capacity (MW); g = dispatch (MW);
# soc = stored energy (MWh); F = line capacity (MW); f = line flow (MW).
# Battery energy capacity is E = h_max * G.
# lpSolve requires nonnegative variables. We represent signed f as f_NS - f_SN,
# and signed battery g as g_discharge - g_charge. These are bookkeeping splits.

# 1. Load packages and enter the data ---------------------------------------
# Run once if needed:
# install.packages(c("lpSolve", "dplyr", "tidyr", "tibble", "ggplot2"))
library(lpSolve)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

theme_set(theme_minimal(base_size = 12))
# Use the same technology colors across figures.
technology_colors <- c(wind = "#397CA8", solar = "#E3AC36", gas = "#777777",
                       battery = "#8865A8", line = "#26998C")

# Every row is one location-hour: 4 balances, not one annual balance.
demand <- tribble(
  ~location, ~period, ~demand_MW,
  "N",       1,       4,
  "N",       2,       4,
  "S",       1,       4,
  "S",       2,       4
)

# Availability is the paper's g_bar: output available per MW of capacity.
# These are coefficients, not decision variables.
renewables <- tribble(
  ~location, ~technology, ~period, ~g_bar,
  "N",       "wind",     1,       0.4,
  "N",       "wind",     2,       1.0,
  "S",       "solar",    1,       1.0,
  "S",       "solar",    2,       0.0
)

eta_1 <- 0.9                 # Battery charging efficiency
eta_2 <- 0.9                 # Battery discharging efficiency
h_max <- 1                  # One MWh of energy capacity per MW of battery
line_length <- 1            # One normalized distance unit, NOT an actual km
CAP_LV <- 1                 # Baseline: F * line_length <= 1
CAP_CO2 <- 1                # Tonnes permitted over the two hours
emissions_per_MWh <- 0.5    # e_gas / eta_gas, already per MWh of electricity
# Thus gas can supply at most 2 MWh out of 16 MWh of demand (before losses).

# Investment cost per MW for the modeled horizon.
# Battery cost includes its fixed one-hour energy capacity.
capacity_variables <- tribble(
  ~variable,       ~location, ~technology, ~cost,
  "G_N_wind",      "N",       "wind",      2.0,
  "G_S_solar",     "S",       "solar",     1.0,
  "G_N_gas",       "N",       "gas",       0.2,
  "G_S_gas",       "S",       "gas",       0.2,
  "G_N_battery",   "N",       "battery",   0.5,
  "G_S_battery",   "S",       "battery",   0.5,
  "F",            "N-S",     "line",      0.5
)
capacity_variables

# Plot the exogenous weather inputs before solving anything.
# Prediction: Where might electricity move in each period?
plot_weather <- ggplot(renewables,
                       aes(factor(period), g_bar, fill = technology)) +
  geom_col(width = 0.6) +
  facet_wrap(~location) +
  scale_fill_manual(values = technology_colors) +
  labs(x = "Period (1 = sunny; 2 = evening)",
       y = "Available MW per MW installed", fill = NULL,
       title = "The weather coefficients are inputs to the LP")
print(plot_weather)

# 2. List the operating variables -----------------------------------------
# Build a table so each matrix column has a name and an economic meaning.
# Each generator needs a dispatch decision in each of the two periods.
gas <- crossing(location = c("N", "S"), period = 1:2) |>
  mutate(technology = "gas", g_bar = 1)

generation_variables <- bind_rows(renewables, gas) |>
  mutate(variable = paste("g", location, technology, period, sep = "_"),
         capacity = paste("G", location, technology, sep = "_"),
         cost = if_else(technology == "gas", 1, 0))

# Charge and discharge are electricity entering/leaving the battery (MW).
# soc is energy INSIDE the battery at the end of each period (MWh).
storage_variables <- crossing(location = c("N", "S"), period = 1:2,
                              operation = c("charge", "discharge", "soc")) |>
  mutate(variable = if_else(operation == "soc",
                            paste("soc", location, period, sep = "_"),
                            paste("g", location, operation, period, sep = "_")),
         cost = 0)

flow_variables <- crossing(direction = c("NS", "SN"), period = 1:2) |>
  mutate(variable = paste("f", direction, period, sep = "_"), cost = 0)

# This column order must be identical in the objective, matrix, and solution.
variables <- bind_rows(
  capacity_variables |> select(variable, cost),
  generation_variables |> select(variable, cost),
  storage_variables |> select(variable, cost),
  flow_variables |> select(variable, cost)
)
variables
nrow(variables)              # 31 nonnegative decision variables

objective <- setNames(variables$cost, variables$variable)
objective
# Capacity costs appear once. Dispatch cost appears for each hour.
# Each period lasts one hour, so MW * 1 hour = MWh in operating costs.

# 3. Write the constraint matrix, one economic restriction at a time -------
# Rows = constraints. Columns = decisions. Start with zeros, then fill entries.
# Loops below repeat transparent row-writing steps; they do not hide a solve.
balance_rows <- paste("balance", demand$location, demand$period, sep = "_")
generation_rows <- paste0("limit_", generation_variables$variable)
storage_rows <- paste0("limit_", storage_variables$variable)
stock_rows <- paste("stock", rep(c("N", "S"), each = 2), rep(1:2, 2), sep = "_")
flow_rows <- paste0("limit_", flow_variables$variable)
row_names <- c(balance_rows, generation_rows, storage_rows, stock_rows,
               flow_rows, "line_volume", "carbon")

A <- matrix(0, nrow = length(row_names), ncol = nrow(variables),
            dimnames = list(row_names, variables$variable))
constraint_direction <- setNames(rep("<=", nrow(A)), row_names)
rhs <- setNames(rep(0, nrow(A)), row_names)

# 3a. Supply + discharge - charge + imports - exports = demand.
# Write all FOUR balances explicitly so students can check the flow signs.
A["balance_N_1", c("g_N_wind_1", "g_N_gas_1", "g_N_discharge_1",
                    "g_N_charge_1", "f_NS_1", "f_SN_1")] <- c(1, 1, 1, -1, -1, 1)
A["balance_N_2", c("g_N_wind_2", "g_N_gas_2", "g_N_discharge_2",
                    "g_N_charge_2", "f_NS_2", "f_SN_2")] <- c(1, 1, 1, -1, -1, 1)
A["balance_S_1", c("g_S_solar_1", "g_S_gas_1", "g_S_discharge_1",
                    "g_S_charge_1", "f_NS_1", "f_SN_1")] <- c(1, 1, 1, -1, 1, -1)
A["balance_S_2", c("g_S_solar_2", "g_S_gas_2", "g_S_discharge_2",
                    "g_S_charge_2", "f_NS_2", "f_SN_2")] <- c(1, 1, 1, -1, 1, -1)
constraint_direction[balance_rows] <- "="
rhs[balance_rows] <- demand$demand_MW
A[balance_rows, ]

# 3b. Generator output <= g_bar * G. Rearrange as g - g_bar * G <= 0.
for (i in seq_len(nrow(generation_variables))) {
  row <- generation_rows[i]
  A[row, generation_variables$variable[i]] <- 1
  A[row, generation_variables$capacity[i]] <- -generation_variables$g_bar[i]
}
A["limit_g_N_wind_1", ]       # g_N_wind_1 - 0.4 G_N_wind <= 0
A["limit_g_S_solar_2", ]      # No sunlight: g_S_solar_2 <= 0
# No upper siting limits are imposed on installed generation in this toy model.

# 3c. Battery charge/discharge <= G_battery; soc <= h_max * G_battery.
for (i in seq_len(nrow(storage_variables))) {
  row <- storage_rows[i]
  capacity <- paste("G", storage_variables$location[i], "battery", sep = "_")
  A[row, storage_variables$variable[i]] <- 1
  A[row, capacity] <- if (storage_variables$operation[i] == "soc") -h_max else -1
}

# 3d. soc_t = soc_previous + eta_1 * charge_t - discharge_t / eta_2.
# CYCLIC boundary: soc_0 = soc_2. Thus period 1's previous stock is soc_2.
# This does not fix starting stocks to zero. It requires them to be replenished.
# Both hours are solved together, so starting stored energy is never free.
for (location in c("N", "S")) {
  for (period in 1:2) {
    previous_period <- if (period == 1) 2 else 1
    row <- paste("stock", location, period, sep = "_")
    A[row, paste("soc", location, period, sep = "_")] <- 1
    A[row, paste("soc", location, previous_period, sep = "_")] <- -1
    A[row, paste("g", location, "charge", period, sep = "_")] <- -eta_1
    A[row, paste("g", location, "discharge", period, sep = "_")] <- 1 / eta_2
  }
}
constraint_direction[stock_rows] <- "="
A[stock_rows, ]

# 3e. Each directional flow <= F.
# The physically meaningful signed flow is f_NS - f_SN.
# Counterflows can be canceled without changing any balance or objective value.
for (i in seq_len(nrow(flow_variables))) {
  A[flow_rows[i], flow_variables$variable[i]] <- 1
  A[flow_rows[i], "F"] <- -1
}

# 3f. These are the paper's two policy constraints.
A["line_volume", "F"] <- line_length
rhs["line_volume"] <- CAP_LV

gas_columns <- generation_variables |>
  filter(technology == "gas") |>
  pull(variable)
A["carbon", gas_columns] <- emissions_per_MWh
rhs["carbon"] <- CAP_CO2

# Inspect a readable version of A: show only the nonzero coefficients.
# In RStudio, View(constraint_terms) is useful here.
constraint_terms <- as_tibble(A, rownames = "constraint") |>
  pivot_longer(-constraint, names_to = "variable", values_to = "coefficient") |>
  filter(coefficient != 0)
constraint_terms
constraint_terms |> filter(constraint == "balance_N_1")
constraint_terms |> filter(constraint == "stock_S_1")

# 4. Solve ONCE and inspect what the solver returned ------------------------
fit <- lp(direction = "min", objective.in = objective,
          const.mat = A, const.dir = constraint_direction, const.rhs = rhs,
          compute.sens = TRUE)
fit$status                    # 0 means an optimal solution was found
stopifnot(fit$status == 0)
fit$objval                    # Minimum cost over the two-hour horizon

solution <- variables |>
  mutate(value = fit$solution, cost_contribution = cost * value)
solution
solution |> filter(value > 1e-7)
sum(solution$cost_contribution)  # Reconstruct the objective independently

# Check EVERY original constraint, not just the solver's status code.
lhs <- drop(A %*% fit$solution)
constraint_check <- tibble(
  constraint = row_names, direction = unname(constraint_direction),
  lhs = lhs, rhs = unname(rhs),
  # For <= rows, positive slack is unused room. Equalities should have zero gap.
  slack = rhs - lhs,
  dual = fit$duals[seq_len(nrow(A))]
)
constraint_check
stopifnot(all(fit$solution >= -1e-7),
          all(abs(constraint_check$slack[constraint_direction == "="]) < 1e-7),
          all(constraint_check$slack[constraint_direction == "<="] >= -1e-7),
          abs(sum(solution$cost_contribution) - fit$objval) < 1e-7)

# For this minimization problem a <= cap's dual is d(cost*)/d(cap), usually <= 0.
# The positive value of relaxing the cap is its NEGATIVE.
line_dual <- constraint_check |> filter(constraint == "line_volume") |> pull(dual)
mu_LV <- -line_dual
mu_LV

# 5. Plot the baseline: where does each MW come from and go? ---------------
installed <- capacity_variables |>
  left_join(solution |> select(variable, value), by = "variable")
installed

plot_capacity <- ggplot(installed, aes(technology, value, fill = technology)) +
  geom_col(width = 0.65) + facet_wrap(~location) +
  scale_fill_manual(values = technology_colors) +
  labs(x = NULL, y = "Installed power capacity (MW)",
       title = "Investment decisions at the baseline transmission cap") +
  guides(fill = "none")
print(plot_capacity)

# Generator output and battery operations, with charging below zero.
generation <- generation_variables |>
  left_join(solution |> select(variable, value), by = "variable")
battery <- storage_variables |>
  left_join(solution |> select(variable, value), by = "variable")
flows <- flow_variables |>
  left_join(solution |> select(variable, value), by = "variable") |>
  select(period, direction, value) |>
  pivot_wider(names_from = direction, values_from = value) |>
  mutate(net_NS = NS - SN)  # Positive means North -> South

# The direction can reverse: the same line shares solar in period 1 and
# wind in period 2. The dashed bounds show the installed line capacity.
line_capacity <- installed |> filter(variable == "F") |> pull(value)
plot_flow <- ggplot(flows, aes(factor(period), net_NS)) +
  geom_hline(yintercept = 0, color = "grey55") +
  geom_hline(yintercept = c(-line_capacity, line_capacity), linetype = "dashed") +
  geom_col(width = 0.6, fill = "#397CA8") +
  labs(x = "Period", y = "Signed flow (MW; positive = North to South)",
       title = "One line can carry electricity in either direction",
       subtitle = "Dashed lines = installed transfer limits")
print(plot_flow)

trade <- bind_rows(
  flows |> transmute(location = "N", period, value = -net_NS),
  flows |> transmute(location = "S", period, value = net_NS)
) |> mutate(component = if_else(value >= 0, "Imports", "Exports"))

balance_plot_data <- bind_rows(
  generation |> transmute(location, period, component = technology, value),
  battery |> filter(operation != "soc") |>
    transmute(location, period,
              component = if_else(operation == "charge", "Charging", "Discharging"),
              value = if_else(operation == "charge", -value, value)),
  trade |> select(location, period, component, value)
)

# Bars above zero are supplies; bars below zero are charging and exports.
# Positive stack minus negative stack magnitude must equal the demand marker.
plot_balance <- ggplot(balance_plot_data,
                       aes(factor(period), value, fill = component)) +
  geom_hline(yintercept = 0, color = "grey55") +
  geom_col(width = 0.65) +
  geom_point(data = demand, aes(factor(period), demand_MW),
             inherit.aes = FALSE, shape = 18, size = 3) +
  facet_wrap(~location) +
  scale_fill_manual(values = c(technology_colors, Charging = "#8865A8",
                               Discharging = "#B6A0CD", Imports = "#26998C",
                               Exports = "#C65A48")) +
  labs(x = "Period", y = "Power supplied (+) or used (-), MW", fill = NULL,
       title = "Read each country's electricity balance",
       subtitle = "Black diamond = demand; net of all bars = demand")
print(plot_balance)

# Actual output can be below availability: this difference is curtailment.
renewable_output <- generation |> filter(technology != "gas") |>
  left_join(installed |> select(capacity = variable, G = value), by = "capacity") |>
  mutate(available = g_bar * G, curtailed = available - value)
renewable_output |> select(location, period, available, value, curtailed)

plot_curtailment <- renewable_output |>
  select(location, period, Used = value, Curtailed = curtailed) |>
  pivot_longer(c(Used, Curtailed), names_to = "component", values_to = "MW") |>
  ggplot(aes(factor(period), MW, fill = component)) +
  geom_col(width = 0.65) + facet_wrap(~location) +
  scale_fill_manual(values = c(Used = "#397CA8", Curtailed = "grey75")) +
  labs(x = "Period", y = "Renewable power (MW)", fill = NULL,
       title = "Available renewable output need not all be used")
print(plot_curtailment)

# Include time zero explicitly so students can see the cyclic boundary.
stocks <- battery |> filter(operation == "soc") |>
  select(location, period, soc = value)
stocks <- bind_rows(stocks, stocks |> filter(period == 2) |> mutate(period = 0)) |>
  arrange(location, period)
energy_capacity <- installed |> filter(technology == "battery") |>
  transmute(location, E = h_max * value)

plot_storage <- ggplot(stocks, aes(period, soc)) +
  geom_line(color = "#397CA8", linewidth = 1) + geom_point(size = 2) +
  geom_hline(data = energy_capacity, aes(yintercept = E), linetype = "dashed") +
  facet_wrap(~location) + scale_x_continuous(breaks = 0:2) +
  labs(x = "End of period (0 = starting stock)", y = "Stored energy (MWh)",
       title = "Batteries shift energy between periods",
       subtitle = "Dashed line = energy capacity; starting stock = final stock")
print(plot_storage)

# Storage must use more electricity in charging than it returns in discharge.
# This independent energy accounting also checks that imports cancel globally.
energy_totals <- battery |> filter(operation != "soc") |>
  group_by(operation) |> summarise(MWh = sum(value), .groups = "drop")
energy_totals
storage_loss <- sum(battery$value[battery$operation == "charge"]) -
  sum(battery$value[battery$operation == "discharge"])
stopifnot(abs(sum(generation$value) - sum(demand$demand_MW) - storage_loss) < 1e-7)

# 6. Change ONE right-hand side: the transmission-volume cap ---------------
# Stop first: predict the sign of the cost change. What can the model rebuild?
# Every capacity and dispatch variable is re-optimized, just as in the paper.
# A visible loop replaces a custom solve_model() function.
cap_grid <- seq(0, 3, by = 0.05)
scenario_rows <- list()
scenario_choices <- list()

for (i in seq_along(cap_grid)) {
  scenario_rhs <- rhs
  scenario_rhs["line_volume"] <- cap_grid[i]

  scenario_fit <- lp(direction = "min", objective.in = objective,
                     const.mat = A, const.dir = constraint_direction,
                     const.rhs = scenario_rhs, compute.sens = TRUE)
  stopifnot(scenario_fit$status == 0)

  # Check feasibility using the SAME A and the new right-hand side.
  scenario_gap <- scenario_rhs - drop(A %*% scenario_fit$solution)
  stopifnot(all(abs(scenario_gap[constraint_direction == "="]) < 1e-7),
            all(scenario_gap[constraint_direction == "<="] >= -1e-7))

  scenario_rows[[i]] <- tibble(
    CAP_LV = cap_grid[i], total_cost = scenario_fit$objval,
    mu_LV = -scenario_fit$duals[match("line_volume", row_names)]
  )
  scenario_choices[[i]] <- tibble(CAP_LV = cap_grid[i],
                                 variable = variables$variable,
                                 value = scenario_fit$solution)
}
scenarios <- bind_rows(scenario_rows)
choices <- bind_rows(scenario_choices)
scenarios |> filter(CAP_LV %in% c(0, 1, 2, 3))

# A relaxed UPPER bound cannot increase minimum cost: the old plan is feasible.
stopifnot(all(diff(scenarios$total_cost) <= 1e-7))

plot_cost <- ggplot(scenarios, aes(CAP_LV, total_cost)) +
  geom_line(color = "#397CA8", linewidth = 1) +
  geom_vline(xintercept = CAP_LV, linetype = "dashed") +
  labs(x = "Allowed line volume (MW x normalized distance)",
       y = "Minimum two-hour cost (teaching cost units)",
       title = "Relaxing the transmission cap expands the feasible set",
       subtitle = "Dashed line = baseline cap; every point is a new LP solve")
print(plot_cost)

capacity_path <- choices |>
  inner_join(capacity_variables |> select(variable, location, technology), by = "variable")
plot_investment <- ggplot(capacity_path, aes(CAP_LV, value, color = technology)) +
  geom_line(linewidth = 0.9) + facet_wrap(~location) +
  scale_color_manual(values = technology_colors) +
  labs(x = "Allowed line volume", y = "Installed power capacity (MW)", color = NULL,
       title = "The generation and storage portfolio adjusts with the grid")
print(plot_investment)

plot_shadow <- ggplot(scenarios, aes(CAP_LV, mu_LV)) +
  geom_point(size = 1.4, color = "#397CA8") +
  labs(x = "Allowed line volume", y = "Marginal cost saving per extra line-volume unit",
       title = "A shadow price is a local value",
       subtitle = "At LP corners, the marginal value may not be unique")
print(plot_shadow)

# 7. Verify one shadow price with a small, explicit re-solve ----------------
# Choose a point away from a kink after looking at plot_cost/plot_shadow.
# Keep this separate from the scenario loop so every step is visible.
check_cap <- 0.55
small_change <- 0.0001
rhs_before <- rhs
rhs_before["line_volume"] <- check_cap
fit_before <- lp("min", objective, A, constraint_direction, rhs_before,
                 compute.sens = TRUE)

rhs_after <- rhs_before
rhs_after["line_volume"] <- check_cap + small_change
fit_after <- lp("min", objective, A, constraint_direction, rhs_after)
stopifnot(fit_before$status == 0, fit_after$status == 0)

marginal_saving_from_dual <- -fit_before$duals[match("line_volume", row_names)]
marginal_saving_from_resolve <- (fit_before$objval - fit_after$objval) / small_change
shadow_check <- tibble(
  method = c("Negative of cap dual", "Small relaxation and re-solve"),
  marginal_saving = c(marginal_saving_from_dual, marginal_saving_from_resolve)
)
shadow_check
stopifnot(abs(marginal_saving_from_dual - marginal_saving_from_resolve) < 1e-5)

# Compare total savings with a single marginal value. They are different objects.
scenarios |> summarise(no_trade_cost = first(total_cost),
                       expanded_grid_cost = last(total_cost),
                       total_saving = first(total_cost) - last(total_cost))
# Do NOT multiply one shadow price by a large change in the cap unless you
# have checked that the same slope remains valid throughout that interval.

# 8. Suggested interactive exercises --------------------------------------
# A. Set CAP_LV = 0 in Section 1 and rerun Sections 3-5. Explain each balance.
# B. Raise eta_1 and eta_2 to 1. Rebuild A, then re-solve. Where do losses vanish?
# C. Set CAP_CO2 = 0. Rebuild rhs and re-solve. Can both hours still be served?
# D. Give solar positive evening availability. Rebuild the generation table,
#    objective and A. Predict the effect on South's storage and imports.
# E. Change line cost from 0.5 to 2 in capacity_variables. Rerun from Section 2.
#    Explain the difference between changing a COST and changing a CAP.
# F. Inspect constraint_check. Which limits bind? Does binding always mean
#    that the associated shadow price must be strictly different from zero?
#
# Different optimal dispatches can tie. Focus on costs, feasibility, and the
# economic mechanism; do not treat every plotted operating choice as unique.

# 9. Optional: save the plots you have already displayed --------------------
# Run from this Schlachtberger directory to place these alongside the script.
# Set save_plots <- FALSE to skip saving. Plot objects always remain available.
save_plots <- TRUE
if (save_plots) {
  dir.create("Schlachtberger_example_output", showWarnings = FALSE)
  ggsave("Schlachtberger_example_output/01_weather.png", plot_weather, width = 8, height = 4)
  ggsave("Schlachtberger_example_output/02_capacity.png", plot_capacity, width = 8, height = 4)
  ggsave("Schlachtberger_example_output/03_balance.png", plot_balance, width = 8, height = 5)
  ggsave("Schlachtberger_example_output/04_curtailment.png", plot_curtailment, width = 8, height = 4)
  ggsave("Schlachtberger_example_output/05_storage.png", plot_storage, width = 8, height = 4)
  ggsave("Schlachtberger_example_output/06_cost.png", plot_cost, width = 8, height = 4)
  ggsave("Schlachtberger_example_output/07_investment.png", plot_investment, width = 8, height = 4)
  ggsave("Schlachtberger_example_output/08_shadow.png", plot_shadow, width = 8, height = 4)
  ggsave("Schlachtberger_example_output/09_flow.png", plot_flow, width = 8, height = 4)
}
