
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

ind1 <- list(x = 1:5,
            sigma = 0.5)

ind2 <- list(x = 2:6,
             sigma = 0.75)

ind3 <- list(x = 3:7,
             sigma = 0.05)

poblacion <- list(ind1, ind2, ind3)

recombinacion <- function(poblacion, 
                          tipo.x = "discreta",
                          tipo.param = "intermedia", 
                          global = TRUE,
                          lambda, mu){
  
  if (!all(c(tipo.x, tipo.param) %in% c("discreta", "intermedia"))) 
    stop("El tipo de recombinación debe ser discreta o intermedia.")
  
  dim.x <- length(poblacion[[1]]$x)
  poblacion <- do.call(rbind, lapply(poblacion, unlist))
  dim.poblacion <- nrow(poblacion)
  dim.ind <- ncol(poblacion)
   
  
  if (global){
    muestra_tam2 <- function(dim.poblacion) sample(1:dim.poblacion, 2)
    padres <- replicate(dim.ind, muestra_tam2(dim.poblacion), simplify = F)
  } else {
    padres <- list(muestra_tam2(dim.poblacion))
    padres <- rep(padres, dim.ind)
  }
  
  hijo <- numeric(dim.ind)
  for (n in 1:dim.x){
    if (tipo.x == "discreta"){
      hijo[n] <- sample(poblacion[padres[[n]], n], 1)
    } else {
      hijo[n] <- mean(poblacion[padres[[n]], n], 1)
    }
  }
  
  for (n in (dim.x + 1):dim.ind){
    if (tipo.param == "discreta"){
      hijo[n] <- sample(poblacion[padres[[n]], n], 1)
    } else {
      hijo[n] <- mean(poblacion[padres[[n]], n], 1)
    }
  }
  return(list(x = hijo[1:dim.x],
              sigma = hijo[(dim.x + 1):dim.ind]))
}

