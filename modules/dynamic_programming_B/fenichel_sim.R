################################################################################
# Natural Capital Accounting for Gulf of Mexico Reef Fish
# Reference: Fenichel & Abbott (2014), JAERE 1(1/2): 1–27
#
# This script reproduces Figure 1 from the paper using the {capn} package.
# Original code by Eli Fenichel (2013, Mathematica)
# Updated by Seong Do Yun and Eli Fenichel (R, 2017)
# Refactored and commented for teaching purposes (AREC 615)
################################################################################

# Load required package ---------------------------------------------------------
library(capn)

# Clean workspace to avoid conflicts
rm(list = ls())
devAskNewPage(ask = FALSE)
#------------------------------------------------------------------------------
# 1. Set Model Parameters (from Fenichel & Abbott 2014)
#------------------------------------------------------------------------------

param <- list(
  # Ecological parameters
  r      = 0.3847,         # intrinsic growth rate
  k      = 359016000,      # carrying capacity (lbs)
  q      = 0.0003172934,   # catchability coefficient
  
  # Economic parameters
  price  = 2.70,           # price per lb of fish ($)
  cost   = 153.0,          # cost per unit effort ($)
  alpha  = 0.5436459,      # elasticity of catch wrt effort
  gamma  = 0.7882,         # elasticity of effort wrt stock
  y      = 0.1574557,      # scaling parameter for effort
  delta  = 0.02,           # social discount rate
  
  # Approximation parameters
  order   = 50,            # order of Chebyshev polynomial expansion
  upperK  = 359016000,     # upper bound of state space
  lowerK  = 5e6,           # lower bound of state space
  nodes   = 500            # number of collocation nodes
)

#------------------------------------------------------------------------------
# 2. Define Model Functions
#------------------------------------------------------------------------------

# (a) Fishing effort as a function of stock
effort <- function(s, Z) {
  Z$y * s^Z$gamma
}

# (b) Harvest (catch) function
catch <- function(s, Z) {
  Z$q * effort(s, Z)^Z$alpha * s
}

# (c) Stock dynamics: logistic growth minus harvest
sdot <- function(s, Z) {
  Z$r * s * (1 - s / Z$k) - catch(s, Z)
}

# (d) Profit function: revenue minus cost
profit <- function(s, Z) {
  Z$price * catch(s, Z) - Z$cost * effort(s, Z)
}

#------------------------------------------------------------------------------
# 3. Analytical Derivatives for Collocation Approximation
#------------------------------------------------------------------------------

# ∂W/∂s : marginal net benefit wrt stock
dwds <- function(s, Z) {
  (Z$gamma * Z$alpha + 1) * Z$price * Z$q * (Z$y^Z$alpha) * s^(Z$gamma * Z$alpha) -
    Z$gamma * Z$cost * Z$y * s^(Z$gamma - 1)
}

# ∂²W/∂s² : second derivative of net benefit wrt stock
dwdss <- function(s, Z) {
  (Z$gamma * Z$alpha + 1) * Z$gamma * Z$alpha * Z$price * Z$q * (Z$y^Z$alpha) * s^(Z$gamma * Z$alpha - 1) -
    Z$gamma * (Z$gamma - 1) * Z$cost * Z$y * s^(Z$gamma - 2)
}

# ∂(ṡ)/∂s : derivative of stock dynamics wrt stock
dsdotds <- function(s, Z) {
  Z$r - 2 * Z$r * s / Z$k - (Z$gamma * Z$alpha + 1) * Z$q * (Z$y^Z$alpha) * s^(Z$gamma * Z$alpha)
}

# ∂²(ṡ)/∂s² : second derivative of stock dynamics wrt stock
dsdotdss <- function(s, Z) {
  -2 * Z$r / Z$k -
    (Z$gamma * Z$alpha + 1) * Z$gamma * Z$alpha * Z$q * (Z$y^Z$alpha) * s^(Z$gamma * Z$alpha - 1)
}

#------------------------------------------------------------------------------
# 4. Define Approximation Space and Nodes
#------------------------------------------------------------------------------

# Define the approximation space for value and price functions
Aspace <- aproxdef(param$order, param$lowerK, param$upperK, param$delta)

# Generate Chebyshev nodes across the state space
nodes <- chebnodegen(param$nodes, param$lowerK, param$upperK)

#------------------------------------------------------------------------------
# 5. Prepare Simulation Data for Approximation
#------------------------------------------------------------------------------

# Data for value function approximation (V)
simuDataV <- cbind(nodes, sdot(nodes, param), profit(nodes, param))

# Data for shadow price approximation (p)
simuDataP <- cbind(nodes,
                   sdot(nodes, param),
                   dsdotds(nodes, param),
                   dwds(nodes, param))

# Data for shadow price derivative approximation (ṗ)
simuDataPdot <- cbind(nodes,
                      sdot(nodes, param),
                      dsdotds(nodes, param),
                      dsdotdss(nodes, param),
                      dwds(nodes, param),
                      dwdss(nodes, param))

#------------------------------------------------------------------------------
# 6. Recover Approximation Coefficients
#------------------------------------------------------------------------------

# (a) Coefficients for value function V(s)
vC <- vaprox(Aspace, simuDataV)

# (b) Coefficients for price function p(s)
pC <- paprox(Aspace,
             stock = simuDataP[, 1],
             sdot = simuDataP[, 2],
             dsdotds = simuDataP[, 3],
             dwds = simuDataP[, 4])

# (c) Coefficients for price derivative ṗ(s)
pdotC <- pdotaprox(Aspace,
                   stock  = simuDataPdot[, 1],
                   sdot  = simuDataPdot[, 2],
                   dsdotds = simuDataPdot[, 3],
                   dsdotdss = simuDataPdot[, 4],
                   dwds  = simuDataPdot[, 5],
                   dwdss = simuDataPdot[, 6])

#------------------------------------------------------------------------------
# 7. Simulate Value, Price, and Price Derivative Functions
#------------------------------------------------------------------------------

GOMSimV <- vsim(vC, as.matrix(simuDataV[, 1], ncol = 1), profit(nodes, param))
GOMSimP <- psim(pC, simuDataP[, 1], profit(nodes, param), simuDataP[, 2])
GOMSimPdot <- pdotsim(pdotC, simuDataPdot[, 1], simuDataPdot[, 2],
                      simuDataPdot[, 3], profit(nodes, param),
                      simuDataPdot[, 5])

#------------------------------------------------------------------------------
# 8. Plot Shadow Prices from the Three Approximations
#------------------------------------------------------------------------------
library(tidyverse)
library(tibble)
tibble(nodes = nodes,
          shadow_price_value_based       = as.numeric(GOMSimV$shadowp),
          shadow_price_price_based       = as.numeric(GOMSimP$shadowp),
          shadow_price_price_derivative  = as.numeric(GOMSimPdot$shadowp)) %>%
  pivot_longer(cols = starts_with("shadow_price"),
               names_to = "method",
               values_to = "shadow_price") %>%
  ggplot(aes(x = nodes / 1e6,y = shadow_price, color = method)) +
  geom_line(size = 1,alpha=.4) +
  labs(x = "Stock size (million lbs)",
       y = "Shadow price (USD/lb)",
       color = "Approximation Method") +
  theme_minimal() +
  theme(legend.position = "top")


plot(nodes, GOMSimV$shadowp, type = "l", lwd = 2, col = "blue",
     ylim = c(0, 15),
     xlab = "Stock size, s",
     ylab = "Shadow price (USD/lb)")
lines(nodes, GOMSimP$shadowp,   lwd = 2, col = "red")
lines(nodes, GOMSimPdot$shadowp, lwd = 2, col = "green")
legend("topright", legend = c("Value-based", "Price-based", "Price-derivative-based"),
       col = c("blue", "red", "green"), lwd = 2, bty = "n")

#------------------------------------------------------------------------------
# 9. Replicate Fenichel & Abbott (2014) Figure (ggplot version)
#------------------------------------------------------------------------------

# Load required visualization packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

# Prepare data for plotting
gom.out <- tibble(
  stock = as.numeric(GOMSimV$stock / 1e6),  # convert to millions of lbs
  sp    = as.numeric(GOMSimV$shadowp),       # shadow price
  vfun  = as.numeric(GOMSimV$vfun)           # value function
)

# Compute "annuity" approximation for comparison (marginal benefit / δ)
annuity <- tibble(
  stock = as.numeric(gom.out$stock),
  sp = as.numeric(sapply(gom.out$stock * 1e6, dwds, Z = param) / (10 * param$delta))
)

# Reference lines: MSY and calibration stock levels (millions of lbs)
msy.data <- tibble(stock = c(0.5 * param$k / 1e6, 0.5 * param$k / 1e6),
                       sp = c(0, 14))
cal.data <- tibble(stock = c(8.633e7 / 1e6, 8.633e7 / 1e6),
                       sp = c(0, 14))

# Plot
ggplot() +
  geom_line(data = gom.out, aes(x = stock, y = sp), color = "black") +
  geom_line(data = annuity, aes(x = stock, y = sp),
            color = "red", linetype = "dashed") +
  geom_line(data = msy.data, aes(x = stock, y = sp),
            color = "black", linetype = "dotted") +
  geom_line(data = cal.data, aes(x = stock, y = sp),
            color = "black", linetype = "dotted") +
  annotate("text", label = "Accounting price", x = 300, y = 2.7, size = 3) +
  annotate("text", label = "Annuity-based\naccounting price\n(in $10 units)",
           x = 300, y = 9.6, size = 3) +
  annotate("text", label = "MSY stock level", x = 225, y = 12, size = 3) +
  annotate("text", label = "Calibration\nstock level", x = 115, y = 12, size = 3) +
  labs(x = "Fish stock (million lbs)",
       y = "Natural capital accounting price (USD)") +
  ylim(0, 15) +
  theme_minimal(base_size = 12) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

################################################################################
# End of Script
################################################################################
