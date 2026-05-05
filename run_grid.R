source("code/run_one_replication.R")

run_grid <- function(param_grid, scenario, learner, base_learner, REPS = 50, second_base_learner=NULL){
  
  no_of_replications <- REPS
  
  all_summaries <- list()
  counter <- 1
  
  for (config in seq_len(nrow(param_grid))) {
    
    n <- param_grid$n[config]
    d <- param_grid$d[config]
    sigma <- param_grid$sigma[config]
    
    for (rep in seq_len(no_of_replications)) {
      
      out <- run_one_replication(
        n = n,
        d = d,
        sigma = sigma,
        scenario = scenario,
        learner = learner,
        base_learner = base_learner,
        seed = 10000 * config + rep,
        save_predictions = FALSE,
        second_base_learner = second_base_learner
      )
      
      all_summaries[[counter]] <- out$summary
      counter <- counter + 1
    }
  }
  
  summary_results <- do.call(rbind, all_summaries)
  
  summary_results
  
}

