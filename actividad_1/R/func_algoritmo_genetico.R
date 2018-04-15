#' Random integer representation
#'
#' Generación aleatoria de un individuo cuyo genotipo está representado 
#' por una cadena de enteros.
#'
#' @param valores vector. Valores posibles que puede tomar cada gen.
#' @param tam integer. Tamaño del genotipo.
#'
#' @return
#' @export
#'
#' @examples
random_integer_representation <- function(valores, tam){
  sample(valores, size = tam, replace = TRUE)
}

random_floating_point_representation <- function(min = 0, max = 1, tam){
  return(
    runif(n = tam, min, max)
    )
}

#' Generación de población.
#' 
#' Genera la población 
#' 
#'
#' @param valores_posibles Vector. Los distintos valores que puede tomar un gen.
#' @param num_genes Integer. Número de genes que contiene el genotipo
#' @param tam_poblacion Integer. Número de individuos que contendrá la población resultante
#'
#' @return 
#' @export
#'
#' @examples
generacion_poblacion <- function(valores_posibles,
                                 num_genes,
                                 tam_poblacion,
                                 num_genes_fijo = TRUE){
  
  if (num_genes_fijo){
    num_genes <- rep(num_genes, tam_poblacion)
  } else {
    num_genes <- sample(num_genes, size = tam_poblacion, replace = T)
  }
  
  lapply(num_genes,
         random_integer_representation,
         valores = valores_posibles
         )
  
}


generacion_param_auto_adaptativo <- function(num_genes,
                                             tam_poblacion,
                                             min = 0,
                                             max = 1,
                                             num_genes_fijo = TRUE){
  
  if (num_genes_fijo){
    num_genes <- rep(num_genes, tam_poblacion)
  } else {
    num_genes <- sample(num_genes, size = tam_poblacion, replace = T)
  }
  
  lapply(num_genes,
         random_floating_point_representation,
         min = min,
         max = max
  )
  
}



#' Torneo
#'
#' @param poblacion list. Lista con los genotipos de la población.
#' @param fitness_poblacion vector. Valor de fitness para cada genotipo de la población.
#' @param k integer. Número de competidores en el torneo.
#'
#' @return 
#' @export
#'
#' @examples
torneo <- function(poblacion, 
                   fitness_poblacion, 
                   k){
  competidores <- sample(seq_along(poblacion), k)
  mejor_competidor <- competidores[which.min(fitness_poblacion[competidores])]
  return(poblacion[[mejor_competidor]])
}

#' Title
#'
#' @param num_padres 
#' @param poblacion 
#' @param fitness_poblacion 
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
seleccion_padres <- function(num_padres,
                             poblacion, 
                             fitness_poblacion,
                             k = 10){
  replicate(n = num_padres,
            torneo(
              poblacion = poblacion, 
              fitness_poblacion = fitness_poblacion, 
              k = k
            ),
            simplify = FALSE
  )
}

prob_mutacion_adaptativa <- function(t, max_iter){
  return(1 - (t/max_iter))
}

#' Cruce por un punto
#' 
#' Cruce entre dos individuos (padres) para generar dos descendientes
#' por medio de cruce por un punto.
#'
#' @param padres list. Lista con los genotipos de los dos padres.
#' @param prob_cruce float. Probabilidad de que se produzca cruce entre los padres.
#'
#' @return
#' @export
#'
#' @examples
one_point_crossover <- function(padres, prob_cruce){
 
  # Solo preparado para 2 PADRES
  punto <- sample(min(sapply(padres, length)), 1)
  
  hijos <- list()
  if (runif(1) < prob_cruce){
    
    hijos[[1]] <- c(padres[[1]][1:punto],
                    padres[[2]][min(length(padres[[2]]), (punto + 1)):length(padres[[2]])])
    
    hijos[[2]] <- c(padres[[2]][1:punto],
                    padres[[1]][min(length(padres[[1]]), (punto + 1)):length(padres[[1]])])
    
  } else {
    
    hijos[[1]] <- padres[[1]]
    hijos[[2]] <- padres[[2]]
    
  }
  return(hijos) 
}

whole_arithmetic_recombination <- function(padres, alpha = 1/2){
  hijos <- list()
  
  hijos[[1]] <- alpha*padres[[1]] + (1-alpha)*padres[[2]]
  hijos[[2]] <- alpha*padres[[2]] + (1-alpha)*padres[[1]]
  return(hijos)
}

#' Title
#'
#' @param x 
#' @param x_fitness 
#' @param num_padres 
#' @param num_emparejamientos 
#' @param prob_cruce 
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
nuevos_hijos <- function(x, 
                         x_fitness,
                         num_padres, 
                         num_emparejamientos = length(x)/num_padres,
                         prob_cruce, 
                         k){
  
  # Falta el caso en el que el tamaño de la población no sea divisible por num_padres.
  
  emparejamientos <- replicate(n = num_emparejamientos, 
                               seleccion_padres(num_padres, 
                                                poblacion = x,
                                                fitness_poblacion = x_fitness,
                                                k), 
                               simplify = FALSE)
  return(
    unlist(
      lapply(emparejamientos,
             one_point_crossover,
             prob_cruce = prob_cruce
      ),
      recursive = FALSE
    )
  )
}

# Mutación ----------------------------------------------------------------

#' Mutación random resetting.
#' 
#' 
#' 
#'
#' @param x vector. Genotipo sobre el que provocar mutaciones.
#' @param prob float. Probabilidad de que se aplique mutación en cada gen.
#' @param valores_posibles vector. Valores posibles para la mutación de cada gen.
#'
#' @return
#' @export
#'
#' @examples
random_resetting <- function(x, prob, valores_posibles){
  genes_mutantes <- runif(length(x)) < prob
  x[genes_mutantes] <- sample(valores_posibles,
                              size = sum(genes_mutantes),
                              replace = TRUE)
  return(x)
}

#' Aplicar mutaciones sobre los individuos de una población
#'
#' @param x list. Lista con los genotipos de los individuos de la población.
#' @param prob_mutacion float. Probabilidad de que se aplique mutación en cada gen.
#' @param valores_posibles vector. Valores posibles de mutaciones en cada gen.
#'
#' @return
#' @export
#'
#' @examples
mutacion_poblacion <- function(x, prob_mutacion, valores_posibles){
  return(
    lapply(x, 
           FUN = random_resetting, 
           prob = prob_mutacion,
           valores_posibles = valores_posibles
    )
  )
}




stochastic_hill_climbing <- function(x, valores_posibles){
  mutaciones <- vector("list", length = length(x))
  for (i in seq_along(x)){
    x_mutado <- x
    x_mutado[i] <- sample(valores_posibles, size = 1)
    mutaciones[[i]] <- x_mutado
  }
  return(mutaciones)
}

local_search <- function(x, funcion_fitness, valores_posibles, valor_fitness_x){
  
  mutaciones_x <- stochastic_hill_climbing(x, valores_posibles)
  
  valor_fitness <- funcion_fitness(mutaciones_x)
  
  id_min <- which.min(valor_fitness)
  mejor_fitness <- valor_fitness[[id_min]]
  if (valor_fitness_x < mejor_fitness){
    return(list(x, fitness = valor_fitness_x))
  } else {
    return(list(mutaciones_x[[id_min]], fitness = mejor_fitness))
  }
}

