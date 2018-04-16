library(tidyverse)
library(parallel)
library(gramEvol)
source("actividad_3/src/discusion_resultados_utils.R")
source("actividad_3/src/utils.R")
source("actividad_1/R/func_algoritmo_genetico.R")

# Preparación para discusión de comentarios -------------------------------

# x <- read_rds("actividad_3/resultados/f1_comprimido.RDS")


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


# Algoritmo genético sin modificaciones -----------------------------------



                            
tabla_resultados1 <- resultados(x = read_rds("actividad_3/resultados/f1_comprimido.RDS"),
                                a = 0,
                               b = 5,

                               N = 50,

                               f = function(x) 6*x^2,

                               f_reparacion = function(expr)
                                 f_reparacion(expr,
                                                    x = 0,
                                                    f_x = 5),
                               wrappings = 5
                        )
                            
                            
tabla_resultados2 <- resultados(x = read_rds("actividad_3/resultados/f2_comprimido.RDS"),
                                a = 0,
                                b = 5,
                                
                                N = 50,
                                                   
                                f = function(x) 2/(x + 1)^2,
                                
                                f_reparacion = function(expr) 
                                  f_reparacion(expr, 
                                               x = 0,
                                               f_x = -1),
                                wrappings = 5)


tabla_resultados3 <- resultados(x = read_rds("actividad_3/resultados/f3_comprimido.RDS"),
                                a = -2,
                                b = 2,
                                
                                N = 40,
                                
                                f = function(x) (1/4)*(3*x^2 - 2*x + 1),
                                
                                f_reparacion = function(expr) 
                                  f_reparacion(expr, 
                                               x = 0,
                                               f_x = -1/4),
                                wrappings = 5)


tabla_resultados4 <- resultados(x = read_rds("actividad_3/resultados/f4_comprimido.RDS"),
                                a = 0,
                                b = 2,
                                
                                N = 20,
                                
                                f = function(x) (1/3)*exp(2*x) - exp(-6*x),
                                
                                f_reparacion = function(expr) 
                                  f_reparacion(expr, 
                                               x = 0,
                                               f_x = 1/3),
                                wrappings = 5)



tabla_resultados5 <- resultados(x = read_rds("actividad_3/resultados/f5_comprimido.RDS"),
                                a = 0,
                                b = 5,
                                
                                N = 50,
                                
                                f = function(x) log(1 + x) + x/(1+x),
                                
                                f_reparacion = function(expr) 
                                  f_reparacion(expr, 
                                               x = 0,
                                               f_x = 0),
                                wrappings = 5)



tabla_resultados6 <- resultados(x = read_rds("actividad_3/resultados/f6_comprimido.RDS"),
                                a = -2,
                                b = 2,
                                
                                N = 40,
                                
                                f = function(x) exp(x) * (sin(x) + cos(x)),
                                
                                f_reparacion = function(expr) 
                                  f_reparacion(expr, 
                                               x = 0,
                                               f_x = 0),
                                wrappings = 5)


tabla_resultados1$progreso$funcion <- 1
tabla_resultados2$progreso$funcion <- 2
tabla_resultados3$progreso$funcion <- 3
tabla_resultados4$progreso$funcion <- 4
tabla_resultados5$progreso$funcion <- 5
tabla_resultados6$progreso$funcion <- 6

tabla_resultados1$ind_fitness_iter$funcion <- 1
tabla_resultados2$ind_fitness_iter$funcion <- 2
tabla_resultados3$ind_fitness_iter$funcion <- 3
tabla_resultados4$ind_fitness_iter$funcion <- 4
tabla_resultados5$ind_fitness_iter$funcion <- 5
tabla_resultados6$ind_fitness_iter$funcion <- 6

progreso <- bind_rows(tabla_resultados1$progreso,
                      tabla_resultados2$progreso,
                      tabla_resultados3$progreso,
                      tabla_resultados4$progreso,
                      tabla_resultados5$progreso,
                      tabla_resultados6$progreso)

ind_fitness_iter <- bind_rows(tabla_resultados1$ind_fitness_iter,
                              tabla_resultados2$ind_fitness_iter,
                              tabla_resultados3$ind_fitness_iter,
                              tabla_resultados4$ind_fitness_iter,
                              tabla_resultados5$ind_fitness_iter,
                              tabla_resultados6$ind_fitness_iter)


write_rds(progreso, "actividad_3/resultados/resumen/progreso_1.RDS")
write_rds(ind_fitness_iter, "actividad_3/resultados/resumen/ind_fitness_iter_1.RDS")


# Adaptativo --------------------------------------------------------------


tabla_resultados1 <- resultados(x = read_rds("actividad_3/resultados/f1_adaptativo.RDS"),
                                a = 0,
                                b = 5,
                                
                                N = 50,
                                
                                f = function(x) 6*x^2,
                                
                                f_reparacion = function(expr)
                                  f_reparacion(expr,
                                               x = 0,
                                               f_x = 5),
                                wrappings = 5
)


tabla_resultados2 <- resultados(x = read_rds("actividad_3/resultados/f2_adaptativo.RDS"),
                                a = 0,
                                b = 5,
                                
                                N = 50,
                                
                                f = function(x) 2/(x + 1)^2,
                                
                                f_reparacion = function(expr) 
                                  f_reparacion(expr, 
                                               x = 0,
                                               f_x = -1),
                                wrappings = 5)


tabla_resultados3 <- resultados(x = read_rds("actividad_3/resultados/f3_adaptativo.RDS"),
                                a = -2,
                                b = 2,
                                
                                N = 40,
                                
                                f = function(x) (1/4)*(3*x^2 - 2*x + 1),
                                
                                f_reparacion = function(expr) 
                                  f_reparacion(expr, 
                                               x = 0,
                                               f_x = -1/4),
                                wrappings = 5)


tabla_resultados4 <- resultados(x = read_rds("actividad_3/resultados/f4_adaptativo.RDS"),
                                a = 0,
                                b = 2,
                                
                                N = 20,
                                
                                f = function(x) (1/3)*exp(2*x) - exp(-6*x),
                                
                                f_reparacion = function(expr) 
                                  f_reparacion(expr, 
                                               x = 0,
                                               f_x = 1/3),
                                wrappings = 5)


# 
# tabla_resultados5 <- resultados(x = read_rds("actividad_3/resultados/f5_adaptativo.RDS"),
#                                 a = 0,
#                                 b = 5,
#                                 
#                                 N = 50,
#                                 
#                                 f = function(x) log(1 + x) + x/(1+x),
#                                 
#                                 f_reparacion = function(expr) 
#                                   f_reparacion(expr, 
#                                                x = 0,
#                                                f_x = 0),
#                                 wrappings = 5)



tabla_resultados6 <- resultados(x = read_rds("actividad_3/resultados/f6_adaptativo.RDS"),
                                a = -2,
                                b = 2,
                                
                                N = 40,
                                
                                f = function(x) exp(x) * (sin(x) + cos(x)),
                                
                                f_reparacion = function(expr) 
                                  f_reparacion(expr, 
                                               x = 0,
                                               f_x = 0),
                                wrappings = 5)


tabla_resultados1$progreso$funcion <- 1
tabla_resultados2$progreso$funcion <- 2
tabla_resultados3$progreso$funcion <- 3
tabla_resultados4$progreso$funcion <- 4
# tabla_resultados5$progreso$funcion <- 5
tabla_resultados6$progreso$funcion <- 6

tabla_resultados1$ind_fitness_iter$funcion <- 1
tabla_resultados2$ind_fitness_iter$funcion <- 2
tabla_resultados3$ind_fitness_iter$funcion <- 3
tabla_resultados4$ind_fitness_iter$funcion <- 4
# tabla_resultados5$ind_fitness_iter$funcion <- 5
tabla_resultados6$ind_fitness_iter$funcion <- 6

progreso <- bind_rows(tabla_resultados1$progreso,
                      tabla_resultados2$progreso,
                      tabla_resultados3$progreso,
                      tabla_resultados4$progreso,
                      # tabla_resultados5$progreso,
                      tabla_resultados6$progreso)

ind_fitness_iter <- bind_rows(tabla_resultados1$ind_fitness_iter,
                              tabla_resultados2$ind_fitness_iter,
                              tabla_resultados3$ind_fitness_iter,
                              tabla_resultados4$ind_fitness_iter,
                              # tabla_resultados5$ind_fitness_iter,
                              tabla_resultados6$ind_fitness_iter)


write_rds(progreso, "actividad_3/resultados/resumen/progreso_adpatativo.RDS")
write_rds(ind_fitness_iter, "actividad_3/resultados/resumen/ind_fitness_iter_adaptativo.RDS")




# 
# 
# tabla_resultados %>%
#   ggplot(aes(x = iter,
#              y = min, group = paste(id, comb_parametros))) +
#   geom_line(alpha = 0.5)
# 
# tabla_resultados %>%
#   filter(comb_parametros == 45) %>% 
#   ggplot(aes(x = iter,
#              y = min, group = paste(id, comb_parametros))) +
#   geom_line(alpha = 0.5)
# 
# 
# 
# 
#   ggplot(aes(x = iter,
#              y = min, group =  comb_parametros)) +
#   geom_line(alpha = 0.5)
# 
# tabla_resultados %>%
#   ggplot(aes(x = iter,
#              y = min, group = paste(id, comb_parametros))) +
#   geom_line(alpha = 0.5) + 
#   scale_x_log10()
# 
# tabla_resultados %>%
#   ggplot(aes(x = iter,
#              y = min, group = comb_parametros)) +
#   geom_smooth()
# 
# tabla_resultados %>% 
#   group_by(comb_parametros
#            ) %>% 
#   summarise(min_fitness = min(min),
#             prob_cruce = mean(prob_cruce)) %>% 
#   ggplot(aes(x = prob_cruce,
#              y = min_fitness)) + 
#   geom_point()
# 
# tabla_resultados %>% 
#   group_by(comb_parametros
#   ) %>% 
#   summarise(min_fitness = min(min),
#             tam_poblacion = mean(tam_poblacion)) %>% 
#   ggplot(aes(x = tam_poblacion,
#              y = min_fitness)) + 
#   geom_point()
# 
# 
# tabla_resultados %>% 
#   group_by(comb_parametros
#   ) %>% 
#   summarise(min_fitness = min(min),
#             prob_mutacion = mean(prob_mutacion)) %>% 
#   ggplot(aes(x = prob_mutacion,
#              y = min_fitness)) + 
#   geom_point()
# 
# 
# tabla_resultados %>% 
#   group_by(comb_parametros,
#            id) %>% 
#   top_n(1, wt = iter) %>% 
#   ungroup() %>% 
#   group_by(comb_parametros) %>% 
#   summarise(min_fitness = mean(min),
#             prob_mutacion = mean(prob_mutacion)) %>% 
#   ggplot(aes(x = prob_mutacion,
#              y = min_fitness)) + 
#   geom_point()
# 
# 
# tabla_resultados %>% 
#   group_by(comb_parametros,
#            id) %>% 
#   top_n(1, wt = iter) %>% 
#   ungroup() %>% 
#   group_by(comb_parametros) %>% 
#   summarise(min_fitness = mean(min),
#             prob_cruce = mean(prob_cruce)) %>% 
#   ggplot(aes(x = prob_cruce,
#              y = min_fitness)) + 
#   geom_point()
# 
# 
# tabla_resultados %>% 
#   group_by(comb_parametros,
#            id) %>% 
#   top_n(1, wt = iter) %>% 
#   ungroup() %>% 
#   group_by(comb_parametros) %>% 
#   summarise(min_fitness = mean(min),
#             prob_cruce = mean(prob_cruce)) %>% 
#   ggplot(aes(x = prob_cruce,
#              y = min_fitness)) + 
#   geom_point() +
#   geom_smooth()
# 
# 
# 
# tabla_resultados %>% 
#   group_by(comb_parametros,
#            id) %>% 
#   top_n(1, wt = iter) %>% 
#   ungroup() %>% 
#   group_by(comb_parametros) %>% 
#   summarise(min_fitness = mean(min),
#             num_genes_fijo = mean(num_genes_fijo)) %>% 
#   ggplot(aes(y = min_fitness,
#              x = as.character(num_genes_fijo))) + 
#   geom_jitter(height = 0)
# 
# 
# tabla_resultados %>% 
#   group_by(comb_parametros,
#            id) %>% 
#   top_n(1, wt = iter) %>% 
#   ungroup() %>% 
#   group_by(comb_parametros) %>% 
#   summarise(min_fitness = mean(min),
#             num_genes_fijo = mean(num_genes_fijo)) %>% 
#   ggplot(aes(x = min_fitness,
#              fill = as.character(num_genes_fijo))) +
#   geom_density(alpha = 0.5)
# 
# 
# tabla_resultados %>% 
#   group_by(comb_parametros,
#            id) %>% 
#   top_n(-1, wt = min) %>% 
#   top_n(-1, wt = iter) %>% 
#   ungroup() %>% 
#   group_by(comb_parametros) %>% 
#   summarise(iter = mean(iter),
#             prob_mutacion = mean(prob_mutacion)) %>% 
#   ggplot(aes(y = iter,
#              x = prob_mutacion)) +
#   geom_point(alpha = 0.5)
# 
# best_iter <- tabla_resultados %>% 
#   group_by(comb_parametros,
#            id) %>% 
#   top_n(-1, wt = min) %>% 
#   top_n(-1, wt = iter) %>% 
#   ungroup() %>% 
#   group_by(comb_parametros) %>% 
#   summarise_all(funs(min, mean, max, sd))
# 
# m <- which.min(y[[4]]$fitness[[500]])
# 
# ind <- y[[4]]$poblacion_final[[m]]
# 
# ind_expr <- GrammarMap(ind,
#          grammar = grammar_def,
#          wrappings = 10)
# 
# ind_reparado <- f_reparacion(ind_expr, x = 0, f_x = 5)
# f <- ind_reparado
# 
# fitness_act_3(f = function(x) 6*x^2,
#               ind = ind_reparado,
#               a = 0,
#               b = 5,
#               N = 50)
# 
# 
# funcion_fitness(poblacion = y[[4]]$poblacion_final,
#                 a = 0, b = 5, N=50, f = function(x)6*x^2, f_reparacion = f_reparacion, wrappings = 10
#                 )
# 
# 
# # Representación de funciones ---------------------------------------------
# 
# funcion_fitness          = function(x)
#   funcion_fitness2(poblacion = x,
#                    
#                    a = 0,
#                    b = 5,
#                    
#                    N = 50,
#                    
#                    f = function(x) 6*x^2,
#                    
#                    f_reparacion = function(expr)
#                      f_reparacion(expr,
#                                   x = 0,
#                                   f_x = 5),
#                    wrappings = 5
#   )
# 
# x <- read_rds("actividad_3/resultados/f1_comprimido.RDS")
# 
# y <- x[[1]]
# funcion_mejor_ind <- function(y){
#   best_ind <- lapply(y, function(x) which.min(x$fitness[[length(x$fitness)]]))
#   best_individuos <- mapply(x = y, ind = best_ind, function(x, ind) x$poblacion_final[[ind]])
#   
#  
#   
#   poblacion_inicial_decod <- mclapply(poblacion,
#                                       GrammarMap,
#                                       grammar = grammar_def,
#                                       wrappings = wrappings,
#                                       mc.cores = 10)
#   
#   terminales <- map_lgl(poblacion_inicial_decod, function(x) x$type != "NT")
#   
#   poblacion_reparada <- map(poblacion_inicial_decod[terminales], 
#                             f_reparacion)
#   
#   GrammarMap(best_individuos[[which.min(funcion_fitness(best_individuos))]],
#              grammar = grammar_def,
#              wrappings = wrappings) %>% f_reparacion
#   
# }
# mejor_decod <- lapply(x, funcion_mejor_ind)
# 
# 
# 
# mejores <- tabla_resultados %>% 
#   group_by(comb_parametros,
#            id) %>% 
#   top_n(-1, wt = min) %>% 
#   top_n(-1, wt = iter) %>% 
#   ungroup() %>% 
#   # arrange(min)
#   # top_n(-1, wt = min) %>% 
#   filter(min <= 0.002)
# 
# a <- mapply(comb_parametros = mejores$comb_parametros,
#         id = mejores$id,
#         function(comb_parametros,
#                  id,
#                  x) {
#           
#           ind <- x[[comb_parametros]][[id]]$poblacion_final[[
#             which.min( x[[comb_parametros]][[id]]$fitness[[length(x[[comb_parametros]][[id]]$fitness)]])
#           ]]
#           
#           
#           GrammarMap(ind,
#                      grammar = grammar_def,
#                      wrappings = wrappings) %>% f_reparacion
#             
#         },MoreArgs = list(x = x)
#        )
# length(a)
# a[[2]]
# a[1:5]
