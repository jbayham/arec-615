library(plotly)

# Define function
f <- function(y, x) {
  100*x + 150*y - x^2 - y^2 - x*y
}

# Create grid
x <- seq(0, 100, length.out = 100)
y <- seq(0, 100, length.out = 100)
z <- outer(x, y, f)

# Create surface plot
fig <- plot_ly(x = ~x, y = ~y, z = ~z) %>%
  add_surface() %>%
  layout(
    title = "Unconstrained Maximization: f(x,y)",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x,y)")
    )
  )

# Add optimum point
x_star <- 50/3
y_star <- 100 - 2*x_star
z_star <- f(y_star, x_star)

fig <- fig %>%
  add_markers(x = x_star, y = y_star, z = z_star,
              marker = list(size = 5, color = "red"),
              name = "Optimum")

fig



#############################

library(plotly)

# Saddle function
f <- function(x, y) x^2 - y^2

# Grid
x <- seq(-3, 3, length.out = 150)
y <- seq(-3, 3, length.out = 150)
z <- outer(x, y, f)

# Surface with contours and saddle marker
plot_ly(x = ~x, y = ~y, z = ~z) %>%
  add_surface(contours = list(
    z = list(show = TRUE, usecolormap = TRUE,
             highlightcolor = "white", project = list(z = TRUE))
  )) %>%
  add_markers(x = 0, y = 0, z = 0,
              marker = list(size = 5, color = "red"),
              name = "Saddle point (0,0,0)") %>%
  layout(
    title = "Saddle Surface: f(x, y) = x^2 - y^2",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x, y)")
    )
  )


