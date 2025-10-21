library(tidyverse)
library(igraph)
library(ggraph)

# parameters
beta <- 0.9
pi <- function(x) 8*x - x^2   # per-period profit
V2 <- c(`0`=0, `1`=7, `2`=12, `3`=15, `4`=16)   # terminal values at t=2

# compute V1(s)
V1 <- tibble(s = 0:4) %>%
  mutate(V1 = map_dbl(s, \(s)
                      max(sapply(0:s, \(x) pi(x) + beta*V2[as.character(s - x)]))
  ))

# helper for discounted total value
edge_value <- function(x, s_next, t) {
  cont <- if (t == 0) V1[["V1"]][s_next + 1] else V2[as.character(s_next)]
  val <- pi(x) + beta * cont
  round(val, 2)
}

# -------------------------------
# Build only feasible paths from s0 = 4
# -------------------------------

# t0 -> t1 edges (s0=4)
edges_01 <- tibble(
  from = "t0_s4",
  to   = paste0("t1_s", 4 - 0:4),
  x    = 0:4,
  t    = 0,
  s_next = 4 - 0:4
)

# t1 -> t2 edges (from all s1 reachable from s0=4)
edges_12 <- map_dfr(0:4, \(s1) {
  tibble(
    from = paste0("t1_s", s1),
    to   = paste0("t2_s", s1 - 0:s1),
    x    = 0:s1,
    t    = 1,
    s_next = s1 - 0:s1
  )
})

edges <- bind_rows(edges_01, edges_12) %>%
  mutate(payoff = pi(x),
         total_value = mapply(edge_value, x, s_next, t),
         label = paste0("x=", x, "\nπ=", payoff))

# -------------------------------
# Filter to nodes reachable from t0_s4
# -------------------------------
# Collect only edges reachable from s0=4
reachable <- function(edges, start = "t0_s4") {
  # BFS to find reachable nodes
  visited <- start
  repeat {
    new_nodes <- edges %>% filter(from %in% visited) %>% pull(to)
    new_nodes <- setdiff(new_nodes, visited)
    if (length(new_nodes) == 0) break
    visited <- union(visited, new_nodes)
  }
  edges %>% filter(from %in% visited & to %in% visited)
}

edges <- reachable(edges, start = "t0_s4")

# -------------------------------
# Mark optimal path
# -------------------------------
edges <- edges %>%
  mutate(optimal = case_when(
    from == "t0_s4" & x == 2 ~ TRUE,   # x0=2
    from == "t1_s2" & x == 1 ~ TRUE,   # x1=1
    from == "t2_s1" & x == 1 ~ TRUE,   # x2=1
    TRUE ~ FALSE
  ))

# Node values
node_values <- tribble(
  ~name,      ~V,
  "t0_s4", 23.97,
  "t1_s4", 22.8,
  "t1_s3", 18.3,
  "t1_s2", 13.3,
  "t1_s1", 7,
  "t1_s0", 0,
  "t2_s4", 16,
  "t2_s3", 15,
  "t2_s2", 12,
  "t2_s1", 7,
  "t2_s0", 0
)

# vertices used
vertex_names <- sort(unique(c(edges$from, edges$to)))
nodes <- tibble(name = vertex_names) %>%
  left_join(node_values, by = "name") %>%
  mutate(optimal = name %in% c("t0_s4", "t1_s2", "t2_s1", "t3_s0"))

# -------------------------------
# Build and plot graph
# -------------------------------
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

p <- ggraph(g, layout = "tree") +
  geom_edge_link(aes(label = label, color = optimal),
                 angle_calc = "along",
                 label_dodge = unit(3, "mm"),
                 label_size = 2.8,
                 end_cap = circle(3, "mm"),
                 arrow = arrow(length = unit(2, "mm")),
                 show.legend = FALSE) +
  geom_node_label(aes(label = paste0(name, "\nV=", round(V,1)),
                      fill = optimal),
                  color = "black",
                  size = 3.5,
                  label.padding = unit(2, "mm")) +
  scale_fill_manual(values = c("TRUE" = "gold", "FALSE" = "lightyellow")) +
  scale_color_manual(values = c("TRUE" = "darkorange", "FALSE" = "grey70")) +
  theme_void() +
  coord_flip(xlim = NULL, ylim = NULL) +   # flip layout: t0 on left, t2 on right
  ggtitle("Mine Management Decision Tree (s=4, Left→Right, Payoffs Shown)")

p

ggsave("mine_tree_s4_left_right.pdf", p, width = 9, height = 5)