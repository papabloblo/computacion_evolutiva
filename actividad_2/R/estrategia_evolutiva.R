
#' Mutación para estrategia evolutiva
#'
#' @param individuo list. Lista con dos elementos: x y sigma.
#' @param param list. Lista con los parámetros de la estrategia evolutiva.
#'
#' @return
#' @export
#'
#' @examples
mutacion <- function(individuo, 
                     param){
  
  sigma <- individuo$sigma*exp(param$tau1*rnorm(1) + param$tau2*rnorm(1))
  
  sigma <- ifelse(sigma < param$epsilon, param$epsilon, sigma)
  
  x <- individuo$x + individuo$sigma * rnorm(length(individuo$x))
  
  x <- ifelse(x > param$x.max, 
              param$x.max, 
              ifelse(x < param$x.min,
                     param$x.min,
                     x))
  
  return(list(x = x, sigma = sigma))
}

#' Title
#'
#' @param poblacion 
#' @param param 
#'
#' @return
#' @export
#'
#' @examples
recombinacion <- function(poblacion, 
                          param){
  
  poblacion <- lapply(poblacion, 
                      function(x){ 
                        x$fitness <- NULL
                        return(x)
                        }
                      )
  
  dim.x <- length(poblacion[[1]]$x)
  poblacion <- do.call(rbind, lapply(poblacion, unlist))
  dim.poblacion <- nrow(poblacion)
  dim.ind <- ncol(poblacion)
   
  muestra_tam2 <- function(dim.poblacion) sample(1:dim.poblacion, 2)
  if (param$global){
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
  individuo$sigma <- runif(param$n.sigma, param$sigma.min, param$sigma.max)
  return(individuo)
}

generacion_poblacion <- function(param) {
  replicate(n = param$tam.poblacion,
            generacion_individuo(param),
            simplify = F)
}


estrategia_evolutiva <- function(param) {
  # Población inicial
  poblacion <- generacion_poblacion(param)
  
  poblacion <- lapply(poblacion, function(x){
                                    x$fitness <- param$funcion(x$x)
                                    return(x)
                                  }
                      )
  
  traza.fitness <- vector("list", length = param$num.iter)
  for (i in seq(param$num.iter)){
    nueva.poblacion <- replicate(param$lambda, recombinacion(poblacion, param), simplify = FALSE)
    
    nueva.poblacion <- lapply(nueva.poblacion, mutacion, param = param)
    
    nueva.poblacion <- lapply(nueva.poblacion, 
                              function(x){
                                x$fitness <- param$funcion(x$x)
                                return(x)
                                }
                              )
    
    poblacion <- seleccion_supervivientes(poblacion = poblacion,
                                          nueva.poblacion = nueva.poblacion,
                                          param)
    
    
    traza.fitness[[i]] <- sapply(poblacion, `[[`, "fitness")
    print(min(traza.fitness[[i]]))
  }
  return(list(traza.fitness = traza.fitness, 
              poblacion.final = poblacion))
}





