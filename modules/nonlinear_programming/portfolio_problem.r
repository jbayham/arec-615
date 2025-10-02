library(ggplot2)

# Parameters
mu <- c(0.10, 0.15)   # expected returns
Omega <- matrix(c(0.04, 0.02,
                  0.02, 0.09), nrow=2)  # covariance
lambda <- 3           # risk aversion

# Utility function
U <- function(x) {
  mu %*% x - 0.5*lambda * t(x) %*% Omega %*% x
}

# Feasible grid of allocations
grid <- expand.grid(x1 = seq(0,1,length=101),
                    x2 = seq(0,1,length=101))
grid <- subset(grid, abs(x1+x2-1)<1e-6)  # enforce budget

grid$U <- apply(grid, 1, function(z) U(c(z[1],z[2])) )

# Find optimum
opt_idx <- which.max(grid$U)
x_star <- grid[opt_idx,]

# Plot
ggplot(grid, aes(x=x1, y=U)) +
  geom_line(color="blue") +
  geom_point(aes(x=x_star$x1, y=x_star$U), color="red", size=3) +
  labs(title="Two-Asset Portfolio Problem",
       subtitle="Utility along feasible allocations (x1+x2=1)",
       x="Allocation to Asset 1", y="Utility")

# solve for optimal allocation across all possible risk aversion levels
lambda_seq <- seq(0.1, 10, length=100)
opt_allocations <- sapply(lambda_seq, function(lam) {
  U_lam <- function(x) {
    mu %*% x - 0.5*lam * t(x) %*% Omega %*% x
  }
  grid$U_lam <- apply(grid, 1, function(z) U_lam(c(z[1],z[2])) )
  opt_idx_lam <- which.max(grid$U_lam)
  return(c(grid[opt_idx_lam, "x1"], grid[opt_idx_lam, "x2"]))
})

opt_allocations <- t(opt_allocations)
colnames(opt_allocations) <- c("x1", "x2")
opt_allocations <- as.data.frame(opt_allocations)
opt_allocations$lambda <- lambda_seq
# Plot optimal allocations vs risk aversion
ggplot(opt_allocations, aes(x=lambda)) +
  geom_line(aes(y=x1, color="Asset 1")) +
  geom_line(aes(y=x2, color="Asset 2")) +
  labs(title="Optimal Asset Allocations vs Risk Aversion",
       x="Risk Aversion (lambda)", y="Optimal Allocation") +
  scale_color_manual(name="Assets", values=c("Asset 1"="blue", "Asset 2"="green")) +
  theme_minimal()


#####################


library(nloptr)

set.seed(123)
n <- 5
mu <- runif(n, 0.05, 0.15)
A <- matrix(rnorm(n^2), n)
Omega <- crossprod(A)/n
lambda <- 8

# --- container to store trace ---
trace_list <- list()

# --- Objective with gradient and tracing ---
obj <- function(x) {
  val <- -( t(mu) %*% x - 0.5*lambda * t(x) %*% Omega %*% x )
  grad <- -mu + lambda * (Omega %*% x)
  
  # store iteration
  trace_list[[length(trace_list) + 1]] <<- as.numeric(x)
  
  list(objective = as.numeric(val), gradient = as.numeric(grad))
}

# --- Equality constraint with jacobian ---
budget_constraint <- function(x) {
  constr <- sum(x) - 1
  jac <- rep(1, length(x))
  list(constraints = constr, jacobian = jac)
}

# --- Bounds ---
lb <- rep(0, n)
ub <- rep(1, n)

# --- Solve ---
res <- nloptr(
  x0 = rep(1/n, n),
  eval_f = obj,
  eval_g_eq = budget_constraint,
  lb = lb, ub = ub,
  opts = list(
    algorithm   = "NLOPT_LD_SLSQP",
    xtol_rel    = 1e-8,
    print_level = 2,
    maxeval     = 50
  )
)

# --- Convert trace to dataframe for plotting ---
trace_mat <- do.call(rbind, trace_list)
trace_df <- data.frame(iter = 1:nrow(trace_mat), trace_mat)
colnames(trace_df)[-1] <- paste0("x", 1:n)

# Plot with ggplot
library(ggplot2)
trace_long <- reshape2::melt(trace_df, id.vars = "iter",
                             variable.name = "Asset", value.name = "Weight")

ggplot(trace_long, aes(x = iter, y = Weight, color = Asset)) +
  geom_line(linewidth = 1,alpha=.6) +
  geom_point(size = 2,alpha=.6) +
  labs(title = "Evolution of Portfolio Weights During Optimization",
       x = "Iteration", y = "Weight")

