
# Representación de individuos --------------------------------------------

mutacion <- function(individuo, 
                     param){
  
  sigma <- individuo$sigma*exp(param$tau1*rnorm(1) + param$tau2*rnorm(1))
  
  sigma <- ifelse(sigma < param$epsilon, param$epsilon, sigma)
  
  x <- individuo$x + individuo$sigma * rnorm(length(individuo$x))
  
  return(list(x = x, sigma = sigma))
}

recombinacion <- function(poblacion, 
                          param){
  
  if (!all(c(tipo.x, tipo.param) %in% c("discreta", "intermedia"))) 
    stop("El tipo de recombinación debe ser discreta o intermedia.")
  poblacion <- lapply(poblacion, function(x){ 
    x$fitness <- NULL
    return(x)
    }
  )
  dim.x <- length(poblacion[[1]]$x)
  poblacion <- do.call(rbind, lapply(poblacion, unlist))
  dim.poblacion <- nrow(poblacion)
  dim.ind <- ncol(poblacion)
   
  if (param$global){
    muestra_tam2 <- function(dim.poblacion) sample(1:dim.poblacion, 2)
    padres <- replicate(dim.ind, muestra_tam2(dim.poblacion), simplify = F)
  } else {
    padres <- list(muestra_tam2(dim.poblacion))
    padres <- rep(padres, dim.ind)
  }
  
  hijo <- numeric(dim.ind)
  for (n in 1:dim.x){
    if (param$recomb.x == "discreta"){
      hijo[n] <- sample(poblacion[padres[[n]], n], 1)
    } else {
      hijo[n] <- mean(poblacion[padres[[n]], n], 1)
    }
  }
  
  for (n in (dim.x + 1):dim.ind){
    if (param$recomb.sigma == "discreta"){
      hijo[n] <- sample(poblacion[padres[[n]], n], 1)
    } else {
      hijo[n] <- mean(poblacion[padres[[n]], n], 1)
    }
  }
  return(list(x = hijo[1:dim.x],
              sigma = hijo[(dim.x + 1):dim.ind]))
}

seleccion_supervivientes <- function(poblacion,
                                     nueva.poblacion,
                                     param){

  if (param$estrategia == "+") nueva.poblacion <- c(poblacion, nueva.poblacion)
  
  fitness <- sapply(nueva.poblacion, `[[`, "fitness")
  
  x.mu <- rank(fitness) <= param$mu
  if (!param$min) x.mu <- !x.mu
  return(nueva.poblacion[x.mu])
}
  

# Algoritmo principal -----------------------------------------------------

# Inicialización de la población

generacion_individuo <- function(param) {
  individuo <- list()
  individuo$x <- runif(param$n.x, param$x.min, param$x.max)
  individuo$sigma <- rnorm(param$n.sigma, mean = 0, sd = 1)
  individuo
}

generacion_poblacion <- function(param) {
  replicate(n = param$tam.poblacion,
            generacion_individuo(param),
            simplify = F)
}

# Lista de parámetros
param <- list(n.x = 5,
              n.sigma = 5,
              tam.poblacion = 10,
              
              x.min = -1,
              x.max = 1,
              num.iter = 5,
              
              lambda = 100,
              mu = 10, # ¿== tam.poblacion?
              estrategia = "",
              min = TRUE,
              funcion = suma_powell,
              
              global = TRUE,
              recomb.x = "discreta",
              recomb.sigma = "discreta",
              
              tau1 = 0.7,
              tau2 = 0.8,
              epsilon = 0.01
              )


# Población inicial
poblacion <- generacion_poblacion(param)

poblacion <- lapply(poblacion, function(x){
  x$fitness <- param$funcion(x$x)
  return(x)
  }
  )


num.iter <- 100
for (i in 1:num.iter){
  ## DUDA: primero mutación o recombinación?
  nueva.poblacion <- replicate(param$lambda, recombinacion(poblacion, param), simplify = FALSE)
  
  nueva.poblacion <- lapply(nueva.poblacion, mutacion, param = param)
  
  nueva.poblacion <- lapply(nueva.poblacion, function(x){
    x$fitness <- param$funcion(x$x)
    return(x)
  }
  )
  
  poblacion <- seleccion_supervivientes(poblacion = poblacion,
                                        nueva.poblacion = nueva.poblacion,
                                        param)
  
  print(min(sapply(poblacion, `[[`, "fitness")))
}




