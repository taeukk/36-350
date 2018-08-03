generate_data <- function(n, p) {
return(list(covariates = matrix(rnorm(n * p), n, p), responses = rnorm(n)))
}

model_select <- function(covariates, responses, cutoff) {
  model <- lm(responses ~ covariates)                             
  retained <- summary(model)$coefficients[-1,4] <= cutoff         
  
  if (!any(retained)) {
return(c())                                                 
  }
  
  model <- lm(responses ~ covariates[, retained])                   
  return(as.vector(summary(model)$coefficients[-1, 4]))

}

run_simulation <- function(n_trials, n, p, cutoff) {                       
  replicates <- replicate(n_trials, {                              
    data <- generate_data(n, p)                                           
    return(model_select(data$covariates, data$responses, cutoff))         
    })                                                                     
  return(c, replicates)                                  
}                                                            
                                                                         
combination <- expand.grid(n = c(100, 1000, 10000),                            
                      p = c(10, 20, 50),                                  
                      n_trials = 100,                                   
                      cutoff = 0.05)    
