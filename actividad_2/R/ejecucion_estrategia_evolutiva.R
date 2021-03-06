
# Suma de powell ----------------------------------------------------------

source("actividad_2/R/estrategia_evolutiva.R")

# Apartado 3 --------------------------------------------------------------

# Lista de parámetros
param <- list(n.x = 5,
              n.sigma = 1,
              tam.poblacion = 30,
              
              x.min = -1,
              x.max = 1,
              
              sigma.min = 1,
              sigma.max = 10,
              
              num.iter = 1000,
              
              lambda = 200,
              mu = 30,
              estrategia = ",",
              min = TRUE,
              funcion = suma_powell,
              
              global = TRUE,
              recomb.x = "discreta",
              recomb.sigma = "intermedia",
              
              tau1 = 1/sqrt(2*5),
              tau2 = 0,
              epsilon = 0.01
)

set.seed(123)
system.time(
  resultados <- replicate(20, estrategia_evolutiva(param), simplify = FALSE)  
)

resultados$param <- param

id <- "powell_3_,_disc_interm_eps_001"

saveRDS(resultados, file = paste0("actividad_2/resultados/", id))


# Apartado 3 bis ----------------------------------------------------------

param <- list(n.x = 5,
              n.sigma = 1,
              tam.poblacion = 30,
              
              x.min = -1,
              x.max = 1,
              
              sigma.min = 1,
              sigma.max = 10,
              
              num.iter = 1000,
              
              lambda = 200,
              mu = 30, # ¿== tam.poblacion?
              estrategia = "+",
              min = TRUE,
              funcion = suma_powell,
              
              global = TRUE,
              recomb.x = "discreta",
              recomb.sigma = "intermedia",
              
              tau1 = 1/sqrt(2*5),
              tau2 = 0,
              epsilon = 0.01
)

resultados <- replicate(20, estrategia_evolutiva(param), simplify = FALSE)

resultados$param <- param

id <- "powell_3_+_disc_interm_eps_001"

saveRDS(resultados, file = paste0("actividad_2/resultados/", id))


# Apartado 4 --------------------------------------------------------------

param <- list(n.x = 5,
              n.sigma = 5,
              tam.poblacion = 30,
              
              x.min = -1,
              x.max = 1,
              
              sigma.min = 1,
              sigma.max = 10,
              
              num.iter = 1000,
              
              lambda = 200,
              mu = 30, # ¿== tam.poblacion?
              estrategia = ",",
              min = TRUE,
              funcion = suma_powell,
              
              global = TRUE,
              recomb.x = "discreta",
              recomb.sigma = "intermedia",
              
              tau1 = 1/sqrt(2*5),
              tau2 = 1/sqrt(2*sqrt(5)),
              epsilon = 0.01
)

resultados <- replicate(20, estrategia_evolutiva(param), simplify = FALSE)

resultados$param <- param

id <- "powell_4_,_disc_interm_eps_001"

saveRDS(resultados, file = paste0("actividad_2/resultados/", id))


# Apartado 4 bis ----------------------------------------------------------

param <- list(n.x = 5,
              n.sigma = 5,
              tam.poblacion = 30,
              
              x.min = -1,
              x.max = 1,
              
              sigma.min = 1,
              sigma.max = 10,
              
              num.iter = 1000,
              
              lambda = 200,
              mu = 30, # ¿== tam.poblacion?
              estrategia = "+",
              min = TRUE,
              funcion = suma_powell,
              
              global = TRUE,
              recomb.x = "discreta",
              recomb.sigma = "intermedia",
              
              tau1 = 1/sqrt(2*5),
              tau2 = 1/sqrt(2*sqrt(5)),
              epsilon = 0.01
)

resultados <- replicate(20, estrategia_evolutiva(param), simplify = FALSE)

resultados$param <- param

id <- "powell_4_+_disc_interm_eps_001"

saveRDS(resultados, file = paste0("actividad_2/resultados/", id))



# xin_she_yang_4 ----------------------------------------------------------


# Apartado 3 --------------------------------------------------------------

# Lista de parámetros
param <- list(n.x = 5,
              n.sigma = 1,
              tam.poblacion = 30,
              
              x.min = -10,
              x.max = 10,
              
              sigma.min = 1,
              sigma.max = 10,
              
              num.iter = 1000,
              
              lambda = 200,
              mu = 30, # ¿== tam.poblacion?
              estrategia = ",",
              min = TRUE,
              funcion = xin_she_yang_4,
              
              global = TRUE,
              recomb.x = "discreta",
              recomb.sigma = "intermedia",
              
              tau1 = 1/sqrt(2*5),
              tau2 = 0,
              epsilon = 0.01
)

resultados <- replicate(20, estrategia_evolutiva(param), simplify = FALSE)

resultados$param <- param

id <- "xin_she_yang_4_3_,_disc_interm_eps_001"

saveRDS(resultados, file = paste0("actividad_2/resultados/", id))


# Apartado 3 bis ----------------------------------------------------------

param <- list(n.x = 5,
              n.sigma = 1,
              tam.poblacion = 30,
              
              x.min = -10,
              x.max = 10,
              
              sigma.min = 1,
              sigma.max = 10,
              
              num.iter = 1000,
              
              lambda = 200,
              mu = 30, # ¿== tam.poblacion?
              estrategia = "+",
              min = TRUE,
              funcion = xin_she_yang_4,
              
              global = TRUE,
              recomb.x = "discreta",
              recomb.sigma = "intermedia",
              
              tau1 = 1/sqrt(2*5),
              tau2 = 0,
              epsilon = 0.01
)

resultados <- replicate(20, estrategia_evolutiva(param), simplify = FALSE)

resultados$param <- param

id <- "xin_she_yang_4_3_+_disc_interm_eps_001"

saveRDS(resultados, file = paste0("actividad_2/resultados/", id))


# Apartado 4 --------------------------------------------------------------

param <- list(n.x = 5,
              n.sigma = 5,
              tam.poblacion = 30,
              
              x.min = -10,
              x.max = 10,
              
              sigma.min = 1,
              sigma.max = 10,
              
              num.iter = 1000,
              
              lambda = 200,
              mu = 30, # ¿== tam.poblacion?
              estrategia = ",",
              min = TRUE,
              funcion = xin_she_yang_4,
              
              global = TRUE,
              recomb.x = "discreta",
              recomb.sigma = "intermedia",
              
              tau1 = 1/sqrt(2*5),
              tau2 = 1/sqrt(2*sqrt(5)),
              epsilon = 0.01
)

resultados <- replicate(20, estrategia_evolutiva(param), simplify = FALSE)

resultados$param <- param

id <- "xin_she_yang_4_4_,_disc_interm_eps_001"

saveRDS(resultados, file = paste0("actividad_2/resultados/", id))


# Apartado 4 bis ----------------------------------------------------------

param <- list(n.x = 5,
              n.sigma = 5,
              tam.poblacion = 30,
              
              x.min = -10,
              x.max = 10,
              sigma.min = 1,
              sigma.max = 10,
              
              num.iter = 1000,
              
              lambda = 200,
              mu = 30, # ¿== tam.poblacion?
              estrategia = "+",
              min = TRUE,
              funcion = xin_she_yang_4,
              
              global = TRUE,
              recomb.x = "discreta",
              recomb.sigma = "intermedia",
              
              tau1 = 1/sqrt(2*5),
              tau2 = 1/sqrt(2*sqrt(5)),
              epsilon = 0.01
)

resultados <- replicate(20, estrategia_evolutiva(param), simplify = FALSE)

resultados$param <- param

id <- "xin_she_yang_4_4_+_disc_interm_eps_001"

saveRDS(resultados, file = paste0("actividad_2/resultados/", id))








