
# Representación de individuos --------------------------------------------

ind <- list(x = 1:5,
            sigma = 0.5)

tau <- 2

representacion_individuo_continua <- function(n_x, 
                                              n_sigma = 1,
                                              min, 
                                              max,
                                              tau1,
                                              tau2){
  x <- runif(n, min = min, max = max)
  sigma <- 
}

mutacion_es_no_correladas <- function(individuo, 
                                      tau1,
                                      tau2,
                                      epsilon){
  
  sigma <- individuo$sigma*exp(tau1*rnorm(1) + tau2*rnorm(1))
  
  sigma <- ifelse(sigma < epsilon, epsilon, sigma)
  
  x <- individuo$x + individuo$sigma * rnorm(length(individuo$x))
  
  return(list(x, sigma))
}

mutacion_es_no_correladas(ind, 1, 0, 0.3)

recombinacion <- function(ind, tipo = "discreta", lambda, mu){
  if (! tipo %in% c("discreta", "intermedia")) 
    stop("El tipo de recombinación debe ser discreta o intermedia.")
  if (tipo == "discreta"){
    
  } else {
    
  }
}