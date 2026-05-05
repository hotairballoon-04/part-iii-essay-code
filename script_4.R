source("code/run_grid.R")
source("code/data_generation_scenarios.R")
source("code/fit_learners.R")
source("code/base_learners.R")
library(dplyr)
library(ggplot2)

param_grid <- expand.grid(
  n = c(100, 300, 1000),
  d = c(5, 20),
  sigma = c(0.5, 1, 2)
)

scenario <- case_4
t_learner <- fit_t_learner
x_learner <- fit_x_learner
dr_learner <- fit_dr_learner
base_learner <- lasso_base

summary_results_t <- run_grid(param_grid, scenario, t_learner, base_learner)
summary_results_x <- run_grid(param_grid, scenario, x_learner, base_learner)
summary_results_dr <- run_grid(param_grid, scenario, dr_learner, base_learner)

# label and combine data
summary_results_t$method <- 'T'
summary_results_x$method <- 'X'
summary_results_dr$method <- 'DR'

all_raw <- bind_rows(summary_results_t,
                     summary_results_x,
                     summary_results_dr)

all_raw$setting <- with(all_raw,
                        paste0("n=", n, ", d=", d, ", σ²=", sigma)
)
# plot
ordering <- c(
  "n=1000, d=5, σ²=0.5",
  "n=1000, d=20, σ²=0.5",
  "n=1000, d=5, σ²=1",
  "n=1000, d=20, σ²=1",
  "n=1000, d=5, σ²=2",
  "n=1000, d=20, σ²=2",
  
  "n=300, d=5, σ²=0.5",
  "n=300, d=20, σ²=0.5",
  "n=300, d=5, σ²=1",
  "n=300, d=20, σ²=1",
  "n=300, d=5, σ²=2",
  "n=300, d=20, σ²=2",
  
  "n=100, d=5, σ²=0.5",
  "n=100, d=20, σ²=0.5",
  "n=100, d=5, σ²=1",
  "n=100, d=20, σ²=1",
  "n=100, d=5, σ²=2",
  "n=100, d=20, σ²=2"
)

all_raw$setting <- factor(all_raw$setting, levels = ordering)
all_raw$method <- factor(all_raw$method, levels = c("T", "X", "DR"))

all_raw_4 <- all_raw

ggplot(all_raw_4, aes(x = method, y = log(mse), fill = method)) +
  geom_boxplot() +
  facet_wrap(~ setting, nrow = 3, ncol = 6) +
  scale_fill_viridis_d(option = "D") +
  theme_bw(base_family = "serif") +
  labs(
    x = "Method",
    y = "log(MSE)",
    fill = "Method"
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
