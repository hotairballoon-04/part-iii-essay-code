source("code/generate_data.R")

run_one_replication <- function(n, d, sigma, scenario, learner, base_learner, seed = NULL, save_predictions=FALSE, second_base_learner=NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # generate train and test data on different seeds -> independent data
  train_data <- generate_data(n, d, sigma, scenario, seed=seed)
  test_data <- generate_data(10000, d, sigma, scenario, seed=seed+1)
  
  # train our model and get predictions on test data
  if (!is.null(second_base_learner)){
    res <- learner(train_data, test_data, base_learner, second_base_learner)
  }else{
    res <- learner(train_data, test_data, base_learner)
  }
  
  # evaluate mse
  mse <- mean((res$tau_hat - test_data$tau_true)^2)
  
  
  # return summary and if wanted (save_predictions==TRUE) also return the predictions and true values
  if (save_predictions){
    list(
      summary= data.frame(
        scenario = scenario$name,
        n=n,
        d=d,
        sigma=sigma,
        mse=mse
      ),
      predictions=data.frame(
        tau_hat = res$tau_hat,
        tau_true = test_data$tau_true,
        test_data = test_data
      )
    )
  }else{
    list(
      summary= data.frame(
        scenario = scenario$name,
        n=n,
        d=d,
        sigma=sigma,
        mse=mse
      )
    )
  }
  
}

