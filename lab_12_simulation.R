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
