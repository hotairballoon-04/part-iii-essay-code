source("code/run_grid.R")
source("code/data_generation_scenarios.R")
source("code/fit_learners.R")
source("code/base_learners.R")
library(dplyr)
library(ggplot2)
library(patchwork)



param_grid <- expand.grid(
  n = c(200, 500, 1000, 2000, 4000, 8000),
  d = 20,
  sigma = 1
)

scenario <- case_7
t_learner <- fit_t_learner
x_learner <- fit_x_learner2

base_learner <- rf_base
base_learner_mu <- rf_base
base_learner_tau <- lasso_base

summary_results_t <- run_grid(param_grid, scenario, t_learner, base_learner, REPS=10)
summary_results_x <- run_grid(param_grid, scenario, x_learner, base_learner_mu, REPS=10, base_learner_tau)


t_data <- aggregate(mse ~ n + d + sigma, data = summary_results_t, mean)
x_data <- aggregate(mse ~ n + d + sigma, data = summary_results_x, mean)


# add method labels
t_data$method  <- "T"
x_data$method  <- "X"


# combine data frames
all_data <- bind_rows(t_data, x_data)

all_data$setting <- with(all_data, paste0("n=", n, ", d=", d, ", σ=", sigma))

# plot
p1 <- ggplot(all_data, aes(x = n, y = log(mse), color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  theme_bw(base_family="serif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "n",
    y = "log(MSE)",
    color = "Method",
  ) + 
  scale_color_manual(values = c(
    "T" = "#460956",
    "X" = "#468E8B"
  )) + 
  theme(legend.position = "none")


p2 <- ggplot(all_data, aes(x = n, y = log(mse), color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  theme_bw(base_family="serif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_log10() +
  labs(
    x = "n",
    y = "log(MSE)",
    color = "Method",
  ) + 
  scale_color_manual(values = c(
    "T" = "#460956",
    "X" = "#468E8B"
  ))

(p1 + p2) + plot_annotation(tag_levels = "a", tag_suffix=")")
