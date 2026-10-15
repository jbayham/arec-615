# Ando and Mallory (2012): conservation portfolios under climate uncertainty
# AREC 615. Run numbered sections in order in RStudio, or source this file.
# From the repository root: Rscript modules/nonlinear_programming/Ando/AM_model.R
# Dependencies: install.packages(c("nloptr", "ggplot2"))
# No downloads or file writes. Results and ggplot objects remain in memory.
#
# SOURCE: references/Ando-portfolio_design.pdf, Tables 1-2, equations 1-5.
# DOI: https://doi.org/10.1073/pnas.1114653109
# STATUS: reconstruction from rounded Table 1 inputs, NOT exact replication.
# Do not calibrate inputs to match Table 2: publication discrepancies are
# exposed in paper_audit and budget_audit below. reproduce_SI.R audits the SI
# and reproduces Figure 3 using inferred two-decimal moment rounding.
#
# Reproducibility search (2026-10-08): the paper used MATLAB R2011a frontcon.
# Publisher supplements: pnas.201114653SI.pdf (Table S1), sd01.xlsx (Dataset S1).
# https://www.pnas.org/doi/suppl/10.1073/pnas.1114653109
# No author-issued code package located. Seong Yun's later portn package
# includes an Ando-Mallory example: https://github.com/ysd2004/portn
# Its rounded moment inputs are not used here. We derive moments from outcomes.
# The supplied SI contains result tables rather than MATLAB source code.

library(nloptr)
library(ggplot2)

# 1. Read the scenario table -----------------------------------------------
# Rows are possible JOINT climate futures, not repeated annual observations.
# Columns are regions. A single climate scenario applies to all three regions.
regions <- c("Western", "Central", "Eastern")
scenarios <- c("Historic", "+2 C", "+4 C", "+4 C, wetter")
CCI <- matrix(c(
  0.290, 0.718, 0.317,
  0.178, 0.587, 0.561,
  0.124, 0.251, 0.584,
  0.168, 0.503, 0.654
), nrow = 4, byrow = TRUE, dimnames = list(scenarios, regions))

# Table 1 units: THOUSAND dollars per acre. Keep these units for Figure 3.
cost <- matrix(c(
  0.601, 0.697, 1.210,
  0.631, 0.720, 1.230,
  0.631, 0.720, 1.230,
  0.536, 0.659, 1.200
), nrow = 4, byrow = TRUE, dimnames = list(scenarios, regions))
probabilities <- list(
  "No change likely" = c(0.80, 0.10, 0.05, 0.05),
  "Uniform" = rep(0.25, 4)
)
returns <- list("CCI" = CCI, "CCI/cost" = CCI / cost)
# E[CCI/cost] generally differs from E[CCI]/E[cost]. Divide FIRST.

# 2. Calculate means and covariance ----------------------------------------
portfolio_moments <- function(R, p) {
  stopifnot(is.matrix(R), all(is.finite(R)), length(p) == nrow(R),
            all(is.finite(p)), all(p >= 0), abs(sum(p) - 1) < 1e-10)
  mu <- drop(crossprod(p, R))
  names(mu) <- colnames(R)
  deviations <- sweep(R, 2, mu, "-")
  # Population covariance under the assumed probabilities: no n-1 correction.
  Sigma <- crossprod(deviations, sweep(deviations, 1, p, "*"))
  list(mu = mu, Sigma = Sigma, R = R, p = p)
}

moments_example <- portfolio_moments(CCI, probabilities$Uniform)
moments_example$mu
moments_example$Sigma
cov2cor(moments_example$Sigma)

portfolio_outcomes <- function(w, moments) {
  mean_return <- sum(w * moments$mu)
  variance <- drop(crossprod(w, moments$Sigma %*% w))
  c(mean = mean_return, variance = variance, sd = sqrt(max(0, variance)))
}

equal_weights <- setNames(rep(1/3, 3), regions)
fws_weights <- setNames(c(0.14, 0.76, 0.10), regions) # Table 2, Fig. 3 H/I
portfolio_outcomes(equal_weights, moments_example)

# 3. Solve one constrained portfolio problem -------------------------------
# min w' Sigma w subject to sum(w)=1, mu'w=target, w>=0.
# target=NULL removes the return equality and finds global minimum variance.
# The paper's Eq. 1 prints wi>0, but its text and zero-weight results require
# wi>=0. We follow the economically meaningful, closed feasible set.
solve_portfolio <- function(moments, target = NULL) {
  mu <- moments$mu
  Sigma <- moments$Sigma
  n <- length(mu)
  # SQP supports positive semidefinite covariance, including the singular SI case.
  if (min(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values) < -1e-10)
    stop("Covariance must be positive semidefinite.")
  if (!is.null(target)) {
    stopifnot(length(target) == 1, is.finite(target))
    if (target < min(mu) - 1e-10 || target > max(mu) + 1e-10)
      stop("Expected-return target is infeasible.")
    # At either extreme, a unique best/worst region fixes the weights exactly.
    edge <- which(abs(mu - target) < 1e-12)
    if (length(edge) == 1 &&
        (abs(target - min(mu)) < 1e-12 || abs(target - max(mu)) < 1e-12)) {
      w <- setNames(rep(0, n), names(mu))
      w[edge] <- 1
      return(list(w = w, outcomes = portfolio_outcomes(w, moments),
                  target = target, fit = NULL, endpoint = TRUE))
    }
  }
  # Scale objective and return equality to make tolerances insensitive to units.
  # This leaves the allocation unchanged; reported outcomes use original units.
  variance_scale <- max(abs(Sigma))
  if (variance_scale == 0) variance_scale <- 1
  return_scale <- max(abs(mu))
  if (return_scale == 0) return_scale <- 1
  equalities <- function(w) {
    if (is.null(target)) return(sum(w) - 1)
    c(sum(w) - 1, (sum(mu * w) - target) / return_scale)
  }
  equality_jacobian <- function(w) {
    if (is.null(target)) return(matrix(1, nrow = 1, ncol = n))
    rbind(rep(1, n), mu / return_scale)
  }
  fit <- nloptr(
    x0 = rep(1/n, n), lb = rep(0, n), ub = rep(1, n),
    eval_f = function(w) drop(crossprod(w, Sigma %*% w)) / variance_scale,
    eval_grad_f = function(w) drop(2 * Sigma %*% w) / variance_scale,
    eval_g_eq = equalities, eval_jac_g_eq = equality_jacobian,
    opts = list(algorithm = "NLOPT_LD_SLSQP", xtol_rel = 1e-12,
                ftol_abs = 1e-15, tol_constraints_eq = rep(1e-10, if(is.null(target)) 1 else 2),
                maxeval = 2000)
  )
  if (!(fit$status %in% 1:4)) stop("SQP did not converge: ", fit$message)
  w <- setNames(fit$solution, names(mu))
  stopifnot(abs(sum(w) - 1) < 1e-8, min(w) > -1e-8)
  if (!is.null(target)) stopifnot(abs(sum(mu * w) - target) < 1e-8)
  list(w = w, outcomes = portfolio_outcomes(w, moments), target = target,
       fit = fit, endpoint = FALSE)
}

example_fit <- solve_portfolio(moments_example, target = 0.51)
example_fit$w
example_fit$outcomes

# 4. Trace the efficient frontier ------------------------------------------
# Only targets at/above the global minimum-variance portfolio are efficient.
# The lower branch of the equality-constrained locus is dominated.
trace_frontier <- function(moments, npoints = 100) {
  minimum <- solve_portfolio(moments)
  targets <- seq(minimum$outcomes["mean"], max(moments$mu), length.out = npoints)
  do.call(rbind, lapply(targets, function(target) {
    fit <- solve_portfolio(moments, target)
    data.frame(target = target, mean = unname(fit$outcomes["mean"]),
               sd = unname(fit$outcomes["sd"]), t(fit$w), row.names = NULL)
  }))
}

models <- list()
frontiers <- data.frame()
benchmarks <- data.frame()
for (metric in names(returns)) {
  for (belief in names(probabilities)) {
    key <- paste(metric, belief, sep = ": ")
    m <- portfolio_moments(returns[[metric]], probabilities[[belief]])
    models[[key]] <- m
    f <- trace_frontier(m)
    f$metric <- metric
    f$belief <- belief
    frontiers <- rbind(frontiers, f)
    for (name in c("Equal thirds", "FWS holdings")) {
      w <- if (name == "Equal thirds") equal_weights else fws_weights
      out <- portfolio_outcomes(w, m)
      benchmarks <- rbind(benchmarks, data.frame(metric, belief, portfolio = name,
        mean = unname(out["mean"]), sd = unname(out["sd"])))
    }
  }
}

# 5. Reconstruct Figures 2 and 3 -------------------------------------------
# These plots use Table 1 calculations, not copied published coordinates.
belief_colors <- c("No change likely" = "#237468", "Uniform" = "#b97739")
region_colors <- c(Western = "#6d83a6", Central = "#237468", Eastern = "#b97739")
ando_theme <- theme_minimal(base_size = 18) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        plot.background = element_rect(fill = "#faf9f5", color = NA),
        panel.background = element_rect(fill = "#faf9f5", color = NA),
        text = element_text(color = "#20352f"),
        plot.caption = element_text(size = 11, hjust = 0))
frontier_plot <- function(metric_name) {
  f <- subset(frontiers, metric == metric_name)
  b <- subset(benchmarks, metric == metric_name)
  if (metric_name == "CCI") b <- subset(b, portfolio == "Equal thirds")
  ggplot(f, aes(sd, mean, color = belief)) +
    geom_path(linewidth = 1.1) +
    geom_point(data = b, aes(shape = portfolio), size = 3.5) +
    scale_color_manual(values = belief_colors) +
    scale_shape_manual(values = c("Equal thirds" = 17, "FWS holdings" = 15)) +
    labs(x = paste("Standard deviation of", metric_name),
         y = paste("Expected", metric_name), color = NULL, shape = NULL,
         caption = "Reconstruction from rounded Table 1 inputs. Risk is standard deviation.") +
    ando_theme
}
figure2 <- frontier_plot("CCI")
figure3 <- frontier_plot("CCI/cost")

weight_data <- do.call(rbind, lapply(regions, function(region) {
  data.frame(frontiers[c("target", "metric", "belief")], region,
             weight = frontiers[[region]])
}))
weights_plot <- ggplot(subset(weight_data, metric == "CCI"),
                       aes(target, weight, color = region)) +
  geom_line(linewidth = 1.1) + facet_wrap(~belief, nrow = 1) +
  scale_color_manual(values = region_colors) +
  labs(x = "Target expected CCI", y = "Share of conservation land", color = NULL) +
  ando_theme

scenario_data <- data.frame(scenario = rep(scenarios, 3),
                            region = rep(regions, each = 4), CCI = as.vector(CCI))
scenario_data$scenario <- factor(scenario_data$scenario, levels = scenarios)
scenario_plot <- ggplot(scenario_data, aes(scenario, CCI, color = region, group = region)) +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  scale_color_manual(values = region_colors) +
  labs(x = NULL, y = "Wetland habitat quality (CCI)", color = NULL) + ando_theme

# 6. Quantify diversification gains ----------------------------------------
# At a benchmark's expected return, find the least possible SD.
# At its SD, find the highest feasible mean on the efficient branch.
compare_benchmark <- function(m, w, solver = solve_portfolio) {
  out <- portfolio_outcomes(w, m)
  low <- solver(m)
  # >= benchmark return: use GMV if the benchmark lies below the efficient branch.
  same_mean <- solver(m, max(out["mean"], low$outcomes["mean"]))
  top <- solver(m, max(m$mu))
  if (out["sd"] >= top$outcomes["sd"]) {
    same_risk <- top
  } else {
    target <- uniroot(function(t) solver(m, t)$outcomes["sd"] - out["sd"],
                      c(low$outcomes["mean"], max(m$mu)), tol = 1e-10)$root
    same_risk <- solver(m, target)
  }
  data.frame(benchmark_mean = out["mean"], benchmark_sd = out["sd"],
    efficient_sd = same_mean$outcomes["sd"], efficient_mean = same_risk$outcomes["mean"],
    risk_reduction_pct = 100 * (1 - same_mean$outcomes["sd"] / out["sd"]),
    benefit_gain_pct = 100 * (same_risk$outcomes["mean"] / out["mean"] - 1),
    row.names = NULL)
}
diversification <- do.call(rbind, lapply(names(models), function(key) {
  rbind(data.frame(model = key, benchmark = "Equal thirds",
                   compare_benchmark(models[[key]], equal_weights)),
        data.frame(model = key, benchmark = "FWS holdings",
                   compare_benchmark(models[[key]], fws_weights)))
}))

