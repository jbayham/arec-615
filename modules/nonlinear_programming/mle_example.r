

library(pacman)
p_load(tidyverse)

set.seed(123)

# Simulate data for binary logit
n <- 500
x <- rnorm(n)
beta0 <- 1
beta1 <- -2
xb <- beta0 + beta1 * x
p <- 1 / (1 + exp(-xb))
y <- rbinom(n, 1, p)

# Log-likelihood, gradient (score), Hessian
loglik <- function(theta) {
  xb <- theta[1] + theta[2]*x
  sum(y*xb - log(1 + exp(xb)))
}

score <- function(theta) {
  xb <- theta[1] + theta[2]*x
  p <- 1 / (1 + exp(-xb))
  g1 <- sum(y - p)
  g2 <- sum((y - p)*x)
  c(g1, g2)
}

hessian <- function(theta) {
  xb <- theta[1] + theta[2]*x
  p <- 1 / (1 + exp(-xb))
  W <- p*(1-p)
  H11 <- -sum(W)
  H12 <- -sum(W * x)
  H22 <- -sum(W * x^2)
  matrix(c(H11,H12,H12,H22),2,2)
}


# -----------------------
# 1. Newton-Raphson (manual)
# -----------------------
icount=10 #iteration count
path_NR = vector("list",icount+1) #list to hold series of intermediate results
theta <- c(-1,.5)  # initial guess
path_NR[[1]] <- data.frame(iter=0,
                      theta0=theta[1],
                      theta1=theta[2],
                      ll=loglik(theta),
                      gradnorm=norm(score(theta),"2"))

#Loop over iterations
for (i in 1:icount) {
  g <- score(theta)
  H <- hessian(theta)
  theta <- theta - solve(H, g)
  path_NR[[i+1]] <- data.frame(iter=i,
                               theta0=theta[1],
                               theta1=theta[2],
                               ll=loglik(theta),
                               gradnorm=norm(score(theta),"2"))
}

#Combine results into dataframe (long for plotting)
out_NR <- bind_rows(path_NR) %>%
  pivot_longer(-iter) %>%
  mutate(model="NR")

#plot the intermediate values
ggplot(out_NR,aes(iter,value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1:icount)) +
  facet_wrap(~name,scales = "free") +
  theme_minimal()



# -----------------------
# 2. BFGS via optim
# -----------------------
res_BFGS <- optim(par=c(0,0), 
                  fn=function(th) -loglik(th),
                  gr=function(th) -score(th),
                  method="BFGS", 
                  control=list(maxit=1000,trace=1,REPORT=1)
                  )
theta_BFGS <- res_BFGS$par

# -----------------------
# 3. BHHH via maxLik
# -----------------------
icount=10
path_BHHH = vector("list",icount+1)
theta <- c(-1,.5)  # initial guess
path_BHHH[[1]] <- data.frame(iter=0,
                           theta0=theta[1],
                           theta1=theta[2],
                           ll=loglik(theta),
                           gradnorm=norm(score(theta),"2"))

for (i in 1:icount){
  xb <- theta[1]+theta[2]*x
  p <- 1/(1+exp(-xb))
  s <- cbind(y-p,(y-p)*x)  # scores per obs
  G <- t(s)%*%s
  g <- colSums(s)
  step <- solve(G,g)
  theta <- theta + step
  path_BHHH[[i+1]] <- data.frame(iter=i,
                               theta0=theta[1],
                               theta1=theta[2],
                               ll=loglik(theta),
                               gradnorm=norm(score(theta),"2"))
}

out_BHHH <- bind_rows(path_BHHH) %>%
  pivot_longer(-iter) %>%
  mutate(model="BHHH")

ggplot(out_BHHH,aes(iter,value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(1:icount)) +
  facet_wrap(~name,scales = "free") +
  theme_minimal()


# Compare convergence
bind_rows(out_NR,out_BHHH) %>%
  filter(name=="ll") %>%
  ggplot(aes(x=iter,y=value,color=model)) +
  geom_line() +
  geom_point()
  
bind_rows(out_NR,out_BHHH) %>%
  filter(str_detect(name,"theta")) %>%
  ggplot(aes(x=iter,y=value,color=model)) +
  geom_line() +
  geom_point() +
  facet_wrap(~name,scales="free")

library(plotly)
# Plot the likelihood function
t0=seq(-3,2,by=.05)
t1=seq(-3,2,by=.05)
df <- expand_grid(t0,t1) 
  
# Evaluate loglik at each pair
df$ll <- apply(df, 1, function(row) loglik(c(row["t0"], row["t1"])))

# Reshape into matrix form for plotly surface
zmat <- matrix(df$ll, nrow=length(t0), ncol=length(t1), byrow=FALSE)


ll_surf <- plot_ly(x=~t0, y=~t1, z=~zmat) %>%
  add_surface(opacity=0.7, colorscale="Viridis") %>%
  layout(
    title="Log-Likelihood Surface",
    scene=list(
      xaxis=list(title="theta0"),
      yaxis=list(title="theta1"),
      zaxis=list(title="Log-Likelihood")
    )
  )

ll_surf

ll_NR <- ll_surf %>%
  # Add NR path
  add_trace(data=bind_rows(path_NR), x=~theta0, y=~theta1, z=~ll,
          type="scatter3d", mode="lines+markers",
          line=list(color="blue", width=4),
          marker=list(size=3, color="blue"),
          name="Newton-Raphson")

ll_NR

ll_NR_BHHH <- ll_NR %>%
  # Add NR path
  add_trace(data=bind_rows(path_BHHH), x=~theta0, y=~theta1, z=~ll,
            type="scatter3d", mode="lines+markers",
            line=list(color="red", width=4),
            marker=list(size=3, color="red"),
            name="BHHH")

ll_NR_BHHH
