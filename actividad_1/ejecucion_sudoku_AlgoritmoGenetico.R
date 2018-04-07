# Datos de entrada --------------------------------------------------------
source("sudoku_prueba.R")
source("R/algoritmo_genetico.R")
source("R/func_algoritmo_genetico.R")
source("R/fitness.R")

library(parallel)


# Aproximación 1: instancia sencilla --------------------------------------

# Determinación de parámetros

set.seed(1234)

grid <- data.frame(tam_poblacion = 10,
                   prob_cruce= 0.9,
                   tam_torneo= 2,
                   prob_mutacion = c(0, 0.05, 0.1, 0.15, 0.2, 0.25)
)

# Ejecución de pruebas

resultados_dificil <- parallel::mcmapply(pruebas_ga,
                                         num_pruebas              = 50,
                                         
                                         
                                         num_padres               = 2,
                                         
                                         tam_poblacion            = grid$tam_poblacion,
                                         prob_cruce               = grid$prob_cruce,
                                         tam_torneo               = grid$tam_torneo,
                                         prob_mutacion            = grid$prob_mutacion,
                                         
                                         max_iter                 = 5000,
                                         print_each               = 250,
                                         
                                         
                                         MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                         genes_fijos              = sudoku_facil,
                                                         funcion_fitness          = fitness_sudoku,
                                                         
                                                         valores_posibles         = 1:9,
                                                         valores_mutacion         = 1:9
                                         )
                                         ,
                                         SIMPLIFY = FALSE
)



save(resultados_dificil, file = "data/sudoku_facil_aprox1.RData")


# Aproximación 1: instancia compleja --------------------------------------

# Determinación de parámetros

set.seed(1234)

grid <- data.frame(tam_poblacion = 100,
                   prob_cruce= 0.9,
                   tam_torneo= 5,
                   prob_mutacion = c(0, 0.05, 0.1, 0.15, 0.2, 0.25)
)

# Ejecución de pruebas

resultados_dificil <- parallel::mcmapply(pruebas_ga,
                                         num_pruebas              = 50,
                                         
                                         
                                         num_padres               = 2,
                                         
                                         tam_poblacion            = grid$tam_poblacion,
                                         prob_cruce               = grid$prob_cruce,
                                         tam_torneo               = grid$tam_torneo,
                                         prob_mutacion            = grid$prob_mutacion,
                                         
                                         max_iter                 = 5000,
                                         print_each               = 250,
                                         
                                         
                                         MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                         genes_fijos              = sudoku_dificil,
                                                         funcion_fitness          = fitness_sudoku,
                                                         
                                                         valores_posibles         = 1:9,
                                                         valores_mutacion         = 1:9
                                         )
                                         ,
                                         SIMPLIFY = FALSE
)



save(resultados_dificil, file = "data/sudoku_dificil_aprox1.RData")




# Aproximación 2: instancia sencilla --------------------------------------

# Determinación de parámetros


set.seed(1234)
n <- 50

tam_poblacion <- rep(10, n)
prob_cruce <- runif(n = n, min = 0.5, max = 1)
tam_torneo <- sample(2:10, size = n, replace =TRUE)
prob_mutacion <- seq(from = 0, to = 5/81, length.out = n)

grid <- data.frame(tam_poblacion,
                   prob_cruce,
                   tam_torneo,
                   prob_mutacion
                   )

# Ejecución de pruebas

resultados_dificil <- parallel::mcmapply(pruebas_ga,
                                         num_pruebas              = 10,
                                         
                                         
                                         num_padres               = 2,
                                         
                                         tam_poblacion            = grid$tam_poblacion,
                                         prob_cruce               = grid$prob_cruce,
                                         tam_torneo               = grid$tam_torneo,
                                         prob_mutacion            = grid$prob_mutacion,
                                         
                                         max_iter                 = 10000,
                                         print_each               = 250,
                                         
                                         
                                         MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                         genes_fijos              = sudoku_facil,
                                                         funcion_fitness          = fitness_sudoku,
                                                         
                                                         valores_posibles         = 1:9,
                                                         valores_mutacion         = 1:9
                                         )
                                         ,
                                         SIMPLIFY = FALSE
)



save(resultados_dificil, file = "data/sudoku_facil_aprox2.RData")


# Aproximación 2: instancia compleja --------------------------------------

# Determinación de parámetros

set.seed(1234)
n <- 50

tam_poblacion <- rep(100, n)
prob_cruce <- runif(n = n, min = 0.5, max = 1)
tam_torneo <- sample(2:10, size = n, replace =TRUE)
prob_mutacion <- seq(from = 0, to = 5/81, length.out = n)

grid <- data.frame(tam_poblacion,
                   prob_cruce,
                   tam_torneo,
                   prob_mutacion
                   )

# Ejecución de pruebas

resultados_dificil <- parallel::mcmapply(pruebas_ga,
                                         num_pruebas              = 10,
                                         
                                         
                                         num_padres               = 2,
                                         
                                         tam_poblacion            = grid$tam_poblacion,
                                         prob_cruce               = grid$prob_cruce,
                                         tam_torneo               = grid$tam_torneo,
                                         prob_mutacion            = grid$prob_mutacion,
                                         
                                         max_iter                 = 10000,
                                         print_each               = 250,
                                         
                                         
                                         MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                         genes_fijos              = sudoku_dificil,
                                                         funcion_fitness          = fitness_sudoku,
                                                         
                                                         valores_posibles         = 1:9,
                                                         valores_mutacion         = 1:9
                                         )
                                         ,
                                         SIMPLIFY = FALSE
)



save(resultados_dificil, file = "data/sudoku_dificil_aprox2.RData")


