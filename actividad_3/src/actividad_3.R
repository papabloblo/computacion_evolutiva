
library(gramEvol)
library(purrr)
library(parallel)
library(tidyverse)

source("actividad_1/R/func_algoritmo_genetico.R")
source("actividad_1/R/algoritmo_genetico.R")
source("actividad_3/src/utils.R")

# Definición de gramática -------------------------------------------------

reglas_def <- list(expr = gsrule("<expr><op><expr>",
                                 "(<expr><op><expr>)",
                                 "<pre_op>(<expr>)",
                                 "<var>"),
                   
                   op = gsrule("+", "-", "*", "/"),
                   
                   pre_op = gsrule("sin",
                                   "cos",
                                   "exp",
                                   "log"),
                   
                   var = gsrule("x", "1.0")) 

grammar_def <- CreateGrammar(ruleDef = reglas_def,
                             startSymb = gsrule("<expr>"))


# Generación de la población inicial --------------------------------------
# 
# set.seed(1234)
# poblacion_inicial <- generacion_poblacion(valores_posibles = 0:3,
#                                           num_genes = 50,
#                                           tam_poblacion = 100,
#                                           num_genes_fijo = T)



# Función a integrar ------------------------------------------------------

f <- function(x){
  6*x^2
}


F_integral <- function(x) eval(as.expression(expr))
f_a_integrar <- function(x) eval(expression(6*x^2))

# Ejecución algoritmo -----------------------------------------------------
# 
# z <- algoritmo_genetico(poblacion_inicial = poblacion_inicial,
#                         
#                         funcion_fitness = function(x) 
#                           funcion_fitness(poblacion = x,
#                                           a = 0,
#                                           b = 5,
#                                           
#                                           N = 50,
#                                           f = function(x) 6*x^2,
#                                           f_reparacion = function(expr) 
#                                             funcion_reparacion(expr, x = 0, f_x = 5)
#                                           ),
#                         
#                         valores_mutacion = 0:3,
#                         num_padres = 2,
#                         prob_cruce =  0.5,
#                         tam_torneo = 4,
#                         prob_mutacion = 0.5,
#                         tam_poblacion = length(poblacion_inicial),
#                    
#                         max_iter = 100,
#                         print_each = 5
#                         )



set.seed(1234)
n <- 50
tam_poblacion <- sample(10:100, size =  n, replace = TRUE)
prob_cruce <- runif(n = n, min = 0, max = 1)
prob_mutacion <- runif(n = n, min = 0, max = 1)
num_genes_fijos <- sample(c(TRUE, FALSE), size = n, replace = TRUE)

grid <- data_frame(tam_poblacion = sample(10:50, size =  n, replace = TRUE),
                   tam_torneo = sample(2:10, size = n, replace = TRUE),
                   prob_cruce = runif(n = n, min = 0, max = 1),
                   prob_mutacion = runif(n = n, min = 0, max = 1),
                   num_genes_fijo = sample(c(TRUE, FALSE), size = n, replace = TRUE),
                   num_genes = sample(10:100, size = n, replace = TRUE)
                   )


# F1 ----------------------------------------------------------------------

ini <- Sys.time()
resultados_f1 <- mapply(pruebas_ga,
                                         num_pruebas              = 10,
                                         
                                         
                                         num_padres               = 2,
                                         
                                         tam_poblacion            = grid$tam_poblacion,
                                         prob_cruce               = grid$prob_cruce,
                                         tam_torneo               = grid$tam_torneo,
                                         prob_mutacion            = grid$prob_mutacion,
                                         
                                         num_genes               = grid$num_genes,
                                         num_genes_fijo          = grid$num_genes_fijo,
                                         
                                         max_iter                 = 500,
                                         print_each               = 5,
                                         
                                         
                                         MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                         funcion_fitness          = function(x) 
                                                           funcion_fitness(poblacion = x,
                                                                           
                                                                           a = 0,
                                                                           b = 5,
                                                                           
                                                                           N = 50,
                                                                           
                                                                           f = function(x) 6*x^2,
                                                                           
                                                                           f_reparacion = function(expr) 
                                                                             f_reparacion(expr, 
                                                                                                x = 0,
                                                                                                f_x = 5),
                                                                           wrappings = 5
                                                           ),
                                                         
                                                         valores_posibles         = 0:3,
                                                         valores_mutacion         = 0:3
                                         )
                                         ,
                                         SIMPLIFY = FALSE
                                         )

fin <- Sys.time()
fin - ini

write_rds(resultados_f1, "actividad_3/resultados/f1.RDS")

# F2 ----------------------------------------------------------------------
ini <- Sys.time()
resultados_f2 <- parallel::mcmapply(pruebas_ga,
                                    num_pruebas              = 10,
                                    
                                    
                                    num_padres               = 2,
                                    
                                    tam_poblacion            = grid$tam_poblacion,
                                    prob_cruce               = grid$prob_cruce,
                                    tam_torneo               = grid$tam_torneo,
                                    prob_mutacion            = grid$prob_mutacion,
                                    
                                    num_genes               = grid$num_genes,
                                    num_genes_fijo          = grid$num_genes_fijo,
                                    
                                    max_iter                 = 500,
                                    print_each               = 5,
                                    
                                    
                                    MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                    funcion_fitness          = function(x) 
                                                      funcion_fitness(poblacion = x,
                                                                      
                                                                      a = 0,
                                                                      b = 5,
                                                                      
                                                                      N = 50,
                                                                      
                                                                      f = function(x) 2/(x + 1)^2,
                                                                      
                                                                      f_reparacion = function(expr) 
                                                                        f_reparacion(expr, 
                                                                                           x = 0,
                                                                                           f_x = -1)
                                                      ),
                                                    
                                                    valores_posibles         = 0:3,
                                                    valores_mutacion         = 0:3
                                    )
                                    ,
                                    SIMPLIFY = FALSE,
                                    mc.cores = 10
)

fin <- Sys.time()
fin - ini

write_rds(resultados_f2, "actividad_3/resultados/f2.RDS")


# F3 ----------------------------------------------------------------------
ini <- Sys.time()
resultados_f3 <- parallel::mcmapply(pruebas_ga,
                                    num_pruebas              = 10,
                                    
                                    
                                    num_padres               = 2,
                                    
                                    tam_poblacion            = grid$tam_poblacion,
                                    prob_cruce               = grid$prob_cruce,
                                    tam_torneo               = grid$tam_torneo,
                                    prob_mutacion            = grid$prob_mutacion,
                                    
                                    num_genes               = grid$num_genes,
                                    num_genes_fijo          = grid$num_genes_fijo,
                                    
                                    max_iter                 = 500,
                                    print_each               = 5,
                                    
                                    
                                    MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                    funcion_fitness          = function(x) 
                                                      funcion_fitness(poblacion = x,
                                                                      
                                                                      a = -2,
                                                                      b = 2,
                                                                      
                                                                      N = 40,
                                                                      
                                                                      f = function(x) (1/4)*(3*x^2 - 2*x + 1),
                                                                      
                                                                      f_reparacion = function(expr) 
                                                                        f_reparacion(expr, 
                                                                                           x = 0,
                                                                                           f_x = -1/4)
                                                      ),
                                                    
                                                    valores_posibles         = 0:3,
                                                    valores_mutacion         = 0:3
                                    )
                                    ,
                                    SIMPLIFY = FALSE,
                                    mc.cores = 10
)

fin <- Sys.time()
fin - ini

write_rds(resultados_f3, "actividad_3/resultados/f3.RDS")


# F4 ----------------------------------------------------------------------
ini <- Sys.time()
resultados_f4 <- parallel::mcmapply(pruebas_ga,
                                    num_pruebas              = 10,
                                    
                                    
                                    num_padres               = 2,
                                    
                                    tam_poblacion            = grid$tam_poblacion,
                                    prob_cruce               = grid$prob_cruce,
                                    tam_torneo               = grid$tam_torneo,
                                    prob_mutacion            = grid$prob_mutacion,
                                    
                                    num_genes               = grid$num_genes,
                                    num_genes_fijo          = grid$num_genes_fijo,
                                    
                                    max_iter                 = 500,
                                    print_each               = 5,
                                    
                                    
                                    MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                    funcion_fitness          = function(x) 
                                                      funcion_fitness(poblacion = x,
                                                                      
                                                                      a = 0,
                                                                      b = 2,
                                                                      
                                                                      N = 20,
                                                                      
                                                                      f = function(x) (1/3)*exp(2*x) - exp(-6*x),
                                                                      
                                                                      f_reparacion = function(expr) 
                                                                        f_reparacion(expr, 
                                                                                           x = 0,
                                                                                           f_x = 1/3)
                                                      ),
                                                    
                                                    valores_posibles         = 0:3,
                                                    valores_mutacion         = 0:3
                                    )
                                    ,
                                    SIMPLIFY = FALSE,
                                    mc.cores = 10
)

fin <- Sys.time()
fin - ini

write_rds(resultados_f4, "actividad_3/resultados/f4.RDS")


# F5 ----------------------------------------------------------------------
ini <- Sys.time()
resultados_f5 <- parallel::mcmapply(pruebas_ga,
                                    num_pruebas              = 10,
                                    
                                    
                                    num_padres               = 2,
                                    
                                    tam_poblacion            = grid$tam_poblacion,
                                    prob_cruce               = grid$prob_cruce,
                                    tam_torneo               = grid$tam_torneo,
                                    prob_mutacion            = grid$prob_mutacion,
                                    
                                    num_genes               = grid$num_genes,
                                    num_genes_fijo          = grid$num_genes_fijo,
                                    
                                    max_iter                 = 500,
                                    print_each               = 5,
                                    
                                    
                                    MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                    funcion_fitness          = function(x) 
                                                      funcion_fitness(poblacion = x,
                                                                      
                                                                      a = 0,
                                                                      b = 5,
                                                                      
                                                                      N = 50,
                                                                      
                                                                      f = function(x) log(1 + x) + x/(1+x),
                                                                      
                                                                      f_reparacion = function(expr) 
                                                                        f_reparacion(expr, 
                                                                                           x = 0,
                                                                                           f_x = 0)
                                                      ),
                                                    
                                                    valores_posibles         = 0:3,
                                                    valores_mutacion         = 0:3
                                    )
                                    ,
                                    SIMPLIFY = FALSE,
                                    mc.cores = 10
)

fin <- Sys.time()
fin - ini

write_rds(resultados_f5, "actividad_3/resultados/f5.RDS")


# F6 ----------------------------------------------------------------------
ini <- Sys.time()
resultados_f6 <- parallel::mcmapply(pruebas_ga,
                                    num_pruebas              = 10,
                                    
                                    
                                    num_padres               = 2,
                                    
                                    tam_poblacion            = grid$tam_poblacion,
                                    prob_cruce               = grid$prob_cruce,
                                    tam_torneo               = grid$tam_torneo,
                                    prob_mutacion            = grid$prob_mutacion,
                                    
                                    num_genes               = grid$num_genes,
                                    num_genes_fijo          = grid$num_genes_fijo,
                                    
                                    max_iter                 = 500,
                                    print_each               = 5,
                                    
                                    
                                    MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                    funcion_fitness          = function(x) 
                                                      funcion_fitness(poblacion = x,
                                                                      
                                                                      a = -2,
                                                                      b = 2,
                                                                      
                                                                      N = 40,
                                                                      
                                                                      f = function(x) exp(x) * (sin(x) + cos(x)),
                                                                      
                                                                      f_reparacion = function(expr) 
                                                                        f_reparacion(expr, 
                                                                                           x = 0,
                                                                                           f_x = 0)
                                                      ),
                                                    
                                                    valores_posibles         = 0:3,
                                                    valores_mutacion         = 0:3
                                    )
                                    ,
                                    SIMPLIFY = FALSE,
                                    mc.cores = 10
)

fin <- Sys.time()
fin - ini

write_rds(resultados_f6, "actividad_3/resultados/f6.RDS")

# 
# # Extraer resultados ------------------------------------------------------
# 
# str(resultados_dificil)
# x <-resultados_dificil[[2]]
# min(x[[1]]$fitness[[500]])
# 
# a <- function(z){
#   for (i in seq_along(z)){
#     z[[i]]$id <- i
#   }
#   
#   x2 <- lapply(z,
#                function(x){
#                  aa <- sapply(x$fitness, min)
#                  data_frame(id = x$id,
#                             iter = x$iteracion,
#                             min = aa,
#                             tam_poblacion = x$parametro$tam_poblacion,
#                             
#                             prob_cruce = x$parametro$prob_cruce,
#                             tam_torneo = x$parametro$tam_torneo,
#                             prob_mutacion = x$parametro$prob_mutacion)
#                }
#   )
#   
#   return(bind_rows(x2))
#   
# }
# 
# library(tidyverse)
# z <- a(resultados_f1[[1]])
# 
# z %>% 
#   ggplot(aes(x = iter,
#              y = min, group = id)) + 
#   geom_line()
