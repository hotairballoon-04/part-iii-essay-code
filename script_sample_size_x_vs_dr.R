source("code/run_grid.R")
source("code/data_generation_scenarios.R")
source("code/fit_learners.R")
source("code/base_learners.R")
library(dplyr)
library(ggplot2)



param_grid <- expand.grid(
  n = c(200, 500, 1000, 2000, 4000, 8000, 16000),
  d = 20,
  sigma = 1
)

scenario <- case_8
x_learner <- fit_x_learner
dr_learner <- fit_dr_learner
base_learner <- lasso_base

summary_results_x <- run_grid(param_grid, scenario, x_learner, base_learner, REPS=50)
summary_results_dr <- run_grid(param_grid, scenario, dr_learner, base_learner, REPS=50)


x_data <- aggregate(mse ~ n + d + sigma, data = summary_results_x, mean)
dr_data <- aggregate(mse ~ n + d + sigma, data = summary_results_dr, mean)


# add method labels
x_data$method  <- "X"
dr_data$method <- "DR"

# combine data frames
all_data <- bind_rows(x_data, dr_data)

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
    "X" = "#468E8B",
    "DR" = "#F9E855"
  )) + 
  theme(legend.position = "none")

p2 <- ggplot(all_data, aes(x = n, y = log(mse), color = method, group = method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_log10()+
  theme_bw(base_family="serif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "n",
    y = "log(MSE)",
    color = "Method",
  ) + 
  scale_color_manual(values = c(
    "X" = "#468E8B",
    "DR" = "#F9E855"
  ))

library(patchwork)
(p1 + p2) + plot_annotation(tag_levels = "a", tag_suffix=")")
