case_1 <- list(
  name = "everything_nice",
  X_generator = function(n, d){
    X <- matrix(rnorm(n * d), nrow = n)
    colnames(X) <- paste0("x", 1:d)
    X
  },
  pi_fun = function(X,d,n) {
    0.5
  },
  mu0_fun = function(X) {
    X[,1]
  },
  mu1_fun = function(X) {
    X[,2]
  }
)

case_2 <- list(
  name = "complicated_responses",
  X_generator = function(n, d){
    X <- matrix(rnorm(n * d), nrow = n)
    colnames(X) <- paste0("x", 1:d)
    X
  },
  pi_fun = function(X,d,n) {
    0.4 + 0.2 * plogis(X[,1] + X[,2])
  },
  mu0_fun = function(X) {
    X[,1] - 3 * X[,2] + 0.3 * X[,3] - 0.8 *X[,4] + 4 * X[,5]
  },
  mu1_fun = function(X) {
    X[,1] - 3 * X[,2] + 0.3 * X[,3] - 0.1 *X[,4] + 4 * X[,5] + 2
  }
)

case_3 <- list(
  name = "weak overlap",
  X_generator = function(n, d){
    X <- matrix(rnorm(n * d), nrow = n)
    colnames(X) <- paste0("x", 1:d)
    X
  },
  pi_fun = function(X,d,n) {
    0.02 + 0.96 * plogis(2 * X[,1] + X[,2] - 3 * X[,4])
  },
  mu0_fun = function(X) {
    2 * X[,1]
  },
  mu1_fun = function(X) {
    2 * X[,1] + 0.5 * X[,2] - 1
  }
)

case_4 <- list(
  name = "class_imbalance",
  X_generator = function(n, d){
    X <- matrix(rnorm(n * d), nrow = n)
    colnames(X) <- paste0("x", 1:d)
    X
  },
  pi_fun = function(X,d,n) {
    0.13
  },
  mu0_fun = function(X) {
    X[,1] - 3 * X[,2] + 0.3 * X[,3] - 0.8 *X[,4] + 4 * X[,5]
  },
  mu1_fun = function(X) {
    X[,1] - 3 * X[,2] + 0.3 * X[,3] - 0.1 *X[,4] + 4 * X[,5] + 2
  }
)

case_5 <- list(
  name = "misspecified_nuisances",
  X_generator = function(n, d){
    X <- matrix(rnorm(n * d), nrow = n)
    colnames(X) <- paste0("x", 1:d)
    X
  },
  pi_fun = function(X,d,n) {
    0.5
  },
  mu0_fun = function(X) {
    X[,1] - 10 * X[,2]^2 + 0.2 * X[,3] + 12 * X[,1] * X[,4] + 50 * X[,5]
  },
  mu1_fun = function(X) {
    2 * X[,1] - 10 * X[,2]^2 + 0.2 * X[,3] + 12 * X[,1] * X[,4] + 50 * X[,5]
  }
)


case_6 <- list(
  name = "convergence_t_vs_x",
  X_generator = function(n, d){
    X <- matrix(rnorm(n * d), nrow = n)
    colnames(X) <- paste0("x", 1:d)
    X
  },
  pi_fun = function(X,d,n) {
    0.02
  },
  mu0_fun = function(X) {
    3 * X[,1] - 2 * X[,2] + 1.5 * X[,3] + 2 * X[,4] - X[,5] + X[,6] + 4 * X[,7]
  },
  mu1_fun = function(X) {
    3.5 * X[,1] - 2 * X[,2] + 1.5 * X[,3] + 2 * X[,4] - X[,5] + X[,6] + 4 * X[,7] + 1
  }
)


case_7 <- list(
  name = "convergence_t_vs_x_different_base_learners",
  X_generator = function(n, d){
    X <- matrix(rnorm(n * d), nrow = n)
    colnames(X) <- paste0("x", 1:d)
    X
  },
  pi_fun = function(X,d,n) {
    0.1
  },
  mu0_fun = function(X) {
    3 * X[,1] - 2 * sin(X[,2]*X[,4]) + 1.5 * X[,3]^2 + 2 * X[,2]*X[,4] - X[,5]
  },
  mu1_fun = function(X) {
    3.5 * X[,1] - 2 * sin(X[,2]*X[,4]) + 1.5 * X[,3]^2 + 2 * X[,2]*X[,4] - X[,5] + 1
  }
)

case_8 <- list(
  name = "convergence_x_vs_dr",
  X_generator = function(n, d){
    X <- matrix(rnorm(n * d), nrow = n)
    colnames(X) <- paste0("x", 1:d)
    X
  },
  pi_fun = function(X,d,n) {
    plogis(2 * X[,1] + X[,2] - 3 * X[,4])
  },
  mu0_fun = function(X) {
    X[,1] - X[,2]^2 + sin(X[,1]*X[,4]) + 4 * X[,3]
  },
  mu1_fun = function(X) {
    2*X[,1] - X[,2]^2 + sin(X[,1]*X[,4]) - 2 * X[,3] + 1
  }
)

