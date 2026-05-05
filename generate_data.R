generate_data <- function(n, d, sigma, scenario, seed = NULL) {
  # generate data in a scenario specified by scenario and with sample size n, feature space dimension d and outcome std dev sigma  
  if (!is.null(seed)) set.seed(seed)
  
  # generate data from scenario
  X <- scenario$X_generator(n, d)
  pi <- scenario$pi_fun(X,d,n)
  T <- rbinom(n, 1, pi)
  
  mu0 <- scenario$mu0_fun(X)
  mu1 <- scenario$mu1_fun(X)
  
  # compute CATE tau
  tau <- mu1 - mu0
  
  # simulate observations
  eps <- rnorm(n, 0, sigma)
  
  Y <- T * mu1 + (1 - T) * mu0 + eps
  
  data.frame(
    Y = Y,
    T = T,
    tau_true = tau,
    pi_true = pi,
    mu0_true = mu0,
    mu1_true = mu1,
    X = X
  )
}