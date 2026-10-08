# Helfand and House (1995): an interactive model walkthrough
# Run these sections in order, selecting a few lines at a time in RStudio.
# This file is self-contained: it does not source or change model.R.
# Dependency: nloptr (already used by the original model).
# Coefficients follow the EXISTING CLASSROOM RECONSTRUCTION:
# Mocho's linear N yield coefficient is negative; Pacheco's -1.11*sqrt(N)
# leaching term is omitted. These are not author-confirmed corrections.

# 1. Prices, areas, and biological responses -------------------------------

library(nloptr)

lettuce_price <- 1350                   # dollars per ton, net of harvest cost
nitrogen_price <- 0.70                  # dollars per kg
water_price <- 0.23                     # dollars per mm-ha
area_share <- c(Mocho = 0.5, Pacheco = 0.5)

# N is kg/ha, W is irrigation depth in mm.
# Yield is tons of dry lettuce biomass/ha; leaching is kg/ha.
yield_mocho <- function(N, W) {
  0.9117 - 0.00363*N - 0.0029*W + 0.00000193*N*W +
    0.0506*sqrt(N) + 0.156*sqrt(W)
}

yield_pacheco <- function(N, W) {
  2.296 - 0.00195*N - 0.00158*W + 0.00000153*N*W +
    0.0249*sqrt(N) + 0.0732*sqrt(W)
}

leaching_mocho <- function(N, W) {
  -26.06 - 0.152*N + 0.158*W + 0.000363*N*W
}

leaching_pacheco <- function(N, W) {
  43.82 - 0.17*N + 0.313*W + 0.000466*N*W - 7.28*sqrt(W)
}

# Try one point before optimizing. These are the published Mocho inputs.
yield_mocho(N = 85, W = 723)
leaching_mocho(N = 85, W = 723)

# x always contains FOUR choices in this order: N_M, W_M, N_P, W_P.
starting_inputs <- c(N_M = 85, W_M = 723, N_P = 52, W_P = 478)

# Translate four input choices into a two-row table of economic outcomes.
farm_results <- function(x) {
  N <- c(x[1], x[3])
  W <- c(x[2], x[4])
  yield <- c(yield_mocho(N[1], W[1]), yield_pacheco(N[2], W[2]))
  leaching <- c(leaching_mocho(N[1], W[1]), leaching_pacheco(N[2], W[2]))
  rent <- lettuce_price*yield - nitrogen_price*N - water_price*W
  #output
  data.frame(soil = c("Mocho", "Pacheco"), N, W, yield, leaching, rent,
             row.names = NULL)
}

farm_results(starting_inputs)

# Quasi-rent excludes costs other than fertilizer and irrigation.
# Area shares convert soil-specific outcomes to landscape averages per ha.
mean_rent <- function(x) sum(area_share * farm_results(x)$rent)
mean_leaching <- function(x) sum(area_share * farm_results(x)$leaching)

mean_rent(starting_inputs)
mean_leaching(starting_inputs)

# 2. Optimize the unregulated baseline ------------------------------------

# SLSQP is sequential quadratic programming: approximate the objective
# locally by a quadratic and constraints by linear functions, solve that
# subproblem, update the input choices, and repeat.

# Keep the original model's numerical domain and scaling.
# These are computational choices, not additional policies from the paper.
input_lower <- rep(1e-6, 4)
input_upper <- c(150, 1000, 100, 700)
input_scale <- c(100, 1000, 100, 1000)

# SLSQP minimizes. Negate rent to maximize it.
# The optimizer chooses z; x = z*input_scale converts back to physical units.
# SLSQP uses first derivatives. These are the four marginal quasi-rents.
# Writing them explicitly keeps square-root derivatives inside their domain.
rent_gradient <- function(x) {
  N_M <- x[1]; W_M <- x[2]; N_P <- x[3]; W_P <- x[4]
  yield_slopes <- c(
    -0.00363 + 0.00000193*W_M + 0.0506/(2*sqrt(N_M)),
    -0.0029 + 0.00000193*N_M + 0.156/(2*sqrt(W_M)),
    -0.00195 + 0.00000153*W_P + 0.0249/(2*sqrt(N_P)),
    -0.00158 + 0.00000153*N_P + 0.0732/(2*sqrt(W_P))
  )
  rep(area_share, each = 2) *
    (lettuce_price*yield_slopes - rep(c(nitrogen_price, water_price), 2))
}
baseline_objective <- function(z) -mean_rent(z * input_scale) / 1000

baseline_fit <- nloptr(
  x0 = starting_inputs / input_scale,
  eval_f = baseline_objective,
  eval_grad_f = function(z) -rent_gradient(z * input_scale) * input_scale / 1000,
  lb = input_lower / input_scale,
  ub = input_upper / input_scale,
  opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-10,
              ftol_abs = 1e-12, maxeval = 2000)
)

# 3. Look at the baseline --------------------------------------------------

baseline_inputs <- baseline_fit$solution * input_scale
baseline_inputs
farm_results(baseline_inputs)
baseline_rent <- mean_rent(baseline_inputs)
baseline_leaching <- mean_leaching(baseline_inputs)
baseline_rent
baseline_leaching

# Every policy below targets the same 20% reduction.
leaching_target <- 0.8 * baseline_leaching
leaching_target

# One small reporting function lets us compare all the policies consistently.
# Taxes are transfers; efficiency cost is lost PRE-TAX quasi-rent.
policy_result <- function(policy, x, taxes = rep(0, 4)) {
  tax_by_soil <- c(sum(taxes[1:2] * x[1:2]), sum(taxes[3:4] * x[3:4]))
  transfers <- sum(area_share * tax_by_soil)
  data.frame(policy,
             efficiency_cost = baseline_rent - mean_rent(x),
             leaching = mean_leaching(x),
             tax_payments = transfers,
             farmer_net_rent = mean_rent(x) - transfers)
}

baseline_result <- policy_result("Baseline", baseline_inputs)
baseline_result

# 4. Efficient allocation: choose inputs subject to the leaching target ----

# The planner chooses all four inputs. nloptr expects g(z) <= 0.
efficient_objective <- function(z) -mean_rent(z * input_scale) / 1000
pollution_constraint <- function(z) {
  (mean_leaching(z * input_scale) - leaching_target) / 100
}

efficient_fit <- nloptr(
  x0 = baseline_inputs / input_scale,
  eval_f = efficient_objective,
  eval_grad_f = function(z) -rent_gradient(z * input_scale) * input_scale / 1000,
  eval_g_ineq = pollution_constraint,
  eval_jac_g_ineq = function(z) nl.grad(z, pollution_constraint),
  lb = input_lower / input_scale,
  ub = input_upper / input_scale,
  opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-10, maxeval = 2000)
)

efficient_inputs <- efficient_fit$solution * input_scale
farm_results(efficient_inputs)
mean_leaching(efficient_inputs)

# Recover lambda from the nitrogen FOC on Mocho: q_N = lambda * L_N.
# Reuse the rent derivatives; nl.grad estimates the leaching derivatives.
rent_slopes <- rent_gradient(efficient_inputs)
leaching_slopes <- nl.grad(efficient_inputs, mean_leaching)
shadow_value <- rent_slopes[1] / leaching_slopes[1]
shadow_value                          # dollars per kg of allowed leaching

# Differentiated taxes: lambda times each soil's marginal leaching.
# Remove area weights from the slopes of LANDSCAPE-average leaching.
differentiated_taxes <- shadow_value * leaching_slopes /
  rep(area_share, each = 2)
names(differentiated_taxes) <- names(starting_inputs)
differentiated_taxes

efficient_result <- policy_result("Differentiated taxes", efficient_inputs,
                                  differentiated_taxes)
efficient_result

# 5. Farmer responses to a tax or input ceiling ----------------------------

# Reuse the baseline optimization, now allowing taxes and policy ceilings.
# Maximizing aggregate farmer receipts here also solves each farmer's problem:
# farmers have separate technologies and no shared constraint in this step.
choose_inputs <- function(taxes = rep(0, 4), ceilings = input_upper) {
  after_tax_objective <- function(z) {
    x <- z * input_scale
    tax_by_soil <- c(sum(taxes[1:2]*x[1:2]), sum(taxes[3:4]*x[3:4]))
    -(mean_rent(x) - sum(area_share * tax_by_soil)) / 1000
  }
  after_tax_gradient <- function(z) {
    -(rent_gradient(z * input_scale) - rep(area_share, each = 2) * taxes) *
      input_scale / 1000
  }
  fit <- nloptr(
    x0 = pmin(baseline_inputs, ceilings) / input_scale,
    eval_f = after_tax_objective,
    eval_grad_f = after_tax_gradient,
    lb = input_lower / input_scale,
    ub = ceilings / input_scale,
    opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-10,
                ftol_abs = 1e-12, maxeval = 2000)
  )
  fit$solution * input_scale
}

# Example: farmers facing the differentiated taxes choose the efficient inputs.
choose_inputs(taxes = differentiated_taxes)
efficient_inputs

# 6. Uniform taxes on BOTH inputs -----------------------------------------

# Regulator chooses TWO rates, repeated across soils: (t_N, t_W, t_N, t_W).
# Each trial pair requires farmers to re-optimize all four input choices.
tax_scale <- c(1, 0.2)
uniform_response <- function(z) {
  choose_inputs(taxes = rep(z * tax_scale, 2))
}
uniform_objective <- function(z) -mean_rent(uniform_response(z)) / 1000
uniform_constraint <- function(z) {
  (mean_leaching(uniform_response(z)) - leaching_target) / 100
}

# SLSQP again; nl.grad estimates how the regulator's objective and constraint
# change when the tax rates change and farmers re-optimize.
uniform_fit <- nloptr(
  x0 = c(0.06, 0.9),
  eval_f = uniform_objective,
  eval_grad_f = function(z) nl.grad(z, uniform_objective, heps = 1e-4),
  eval_g_ineq = uniform_constraint,
  eval_jac_g_ineq = function(z) nl.grad(z, uniform_constraint, heps = 1e-4),
  lb = c(0, 0),
  opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-9,
              maxeval = 5000, tol_constraints_ineq = 1e-9)
)

uniform_rates <- uniform_fit$solution * tax_scale
names(uniform_rates) <- c("nitrogen", "water")
uniform_rates
uniform_inputs <- choose_inputs(taxes = rep(uniform_rates, 2))
farm_results(uniform_inputs)
uniform_result <- policy_result("Uniform taxes on both inputs", uniform_inputs,
                                rep(uniform_rates, 2))
uniform_result

# 7. Uniform nitrogen-only tax --------------------------------------------

# With one instrument, find the rate that makes leaching equal the target.
# The intervals in these examples bracket the target for THIS specification.
# They are search intervals, not economic limits on the instruments.
nitrogen_tax_gap <- function(t) {
  x <- choose_inputs(taxes = c(t, 0, t, 0))
  mean_leaching(x) - leaching_target
}
nitrogen_rate <- uniroot(nitrogen_tax_gap, interval = c(0, 15), tol = 1e-8)$root
nitrogen_rate
nitrogen_tax_inputs <- choose_inputs(taxes = c(nitrogen_rate, 0, nitrogen_rate, 0))
farm_results(nitrogen_tax_inputs)
nitrogen_tax_result <- policy_result("Uniform nitrogen-only tax", nitrogen_tax_inputs,
                                     c(nitrogen_rate, 0, nitrogen_rate, 0))

# 8. Uniform water-only tax -----------------------------------------------

water_tax_gap <- function(t) {
  x <- choose_inputs(taxes = c(0, t, 0, t))
  mean_leaching(x) - leaching_target
}
water_rate <- uniroot(water_tax_gap, interval = c(0, 1), tol = 1e-8)$root
water_rate
water_tax_inputs <- choose_inputs(taxes = c(0, water_rate, 0, water_rate))
farm_results(water_tax_inputs)
water_tax_result <- policy_result("Uniform water-only tax", water_tax_inputs,
                                  c(0, water_rate, 0, water_rate))

# 9. Common nitrogen ceiling ----------------------------------------------

nitrogen_cap_gap <- function(cap) {
  x <- choose_inputs(ceilings = c(cap, 1000, cap, 700))
  mean_leaching(x) - leaching_target
}
nitrogen_cap <- uniroot(nitrogen_cap_gap, interval = c(5, 85), tol = 1e-8)$root
nitrogen_cap
nitrogen_cap_inputs <- choose_inputs(ceilings = c(nitrogen_cap, 1000, nitrogen_cap, 700))
farm_results(nitrogen_cap_inputs)
nitrogen_cap_result <- policy_result("Nitrogen cap", nitrogen_cap_inputs)

# 10. Common water ceiling -------------------------------------------------

water_cap_gap <- function(cap) {
  x <- choose_inputs(ceilings = pmin(input_upper, c(150, cap, 100, cap)))
  mean_leaching(x) - leaching_target
}
water_cap <- uniroot(water_cap_gap, interval = c(500, 723), tol = 1e-8)$root
water_cap
water_cap_inputs <- choose_inputs(ceilings = pmin(input_upper, c(150, water_cap, 100, water_cap)))
farm_results(water_cap_inputs)           # Pacheco can use less than the ceiling
water_cap_result <- policy_result("Water cap", water_cap_inputs)

# 11. Water AND nitrogen caps (% of each soil's baseline) ------------------

# A common percentage r creates four soil-specific ceilings.
both_caps_gap <- function(r) {
  x <- choose_inputs(ceilings = (1-r) * baseline_inputs)
  mean_leaching(x) - leaching_target
}
reduction_fraction <- uniroot(both_caps_gap, interval = c(0, 0.3), tol = 1e-9)$root
reduction_fraction
both_ceilings <- (1-reduction_fraction) * baseline_inputs
both_ceilings
both_caps_inputs <- choose_inputs(ceilings = both_ceilings)
farm_results(both_caps_inputs)
both_caps_result <- policy_result("Water and nitrogen caps (% of baseline)", both_caps_inputs)

# 12. Compare policies ----------------------------------------------------

policy_comparison <- rbind(baseline_result, efficient_result, uniform_result,
                           both_caps_result, nitrogen_tax_result, water_tax_result,
                           nitrogen_cap_result, water_cap_result)
print(policy_comparison, digits = 6, row.names = FALSE)

# Inspect any soil-level allocation using farm_results(), for example:
farm_results(water_tax_inputs)
farm_results(water_cap_inputs)

# This walkthrough intentionally omits the original program's checks,
# full derivative machinery, multi-start searches, and automatic root bracketing.
# Keep model.R and validate_model.R as the reference for research/extensions.
