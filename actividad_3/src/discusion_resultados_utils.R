
# Tabla de resultados -----------------------------------------------------

resultados <- function(x,
                       
                       a,
                       b,
                       
                       N,
                       
                       f, 
                       f_reparacion,
                       wrappings){
  
  exitos <- map(x, 
                funcion_mejor_ind,
                a,
                b,
                
                N,
                
                f, 
                f_reparacion,
                wrappings)
  
  tabla_resultados <- map(x, res) %>% 
    bind_rows %>% 
    select(id, iter, min) %>% 
    mutate(comb_parametros = rep(1:50, each = 500*10))
  
  grid <- grid %>% mutate(comb_parametros = 1:nrow(grid))
  
  tabla_resultados <- tabla_resultados %>% 
    left_join(grid)
  
  media_fitness <- tabla_resultados %>%
    group_by(iter, comb_parametros) %>% 
    summarise(fitness_min = min(min), 
              fitness_mean = mean(min),
              fitness_sd   = sd(min),
              fitness_max  = max(min))
  
  
  ind_fitness_iter <- tabla_resultados %>% 
    group_by(comb_parametros,
             id) %>% 
    top_n(-1, wt = min) %>% 
    top_n(-1, wt = iter) %>% 
    ungroup() %>%
    group_by(comb_parametros) %>% 
    rename(fitness = min) %>% 
    summarise(  fitness_min = min(fitness), 
                fitness_mean = mean(fitness),
                fitness_sd   = sd(fitness),
                fitness_max  = max(fitness),
                
                iter_min = min(iter), 
                iter_mean = mean(iter),
                iter_sd   = sd(iter),
                iter_max  = max(iter)
    ) %>% 
    left_join(grid) %>% 
    mutate(succes_rate = map_dbl(exitos, function(x) mean(unlist(x))))
  
  return(list(progreso = media_fitness,
              ind_fitness_iter = ind_fitness_iter))
  
}



res <- function(z){
  
  z <- z[!sapply(z, is.null)]
  for (i in seq_along(z)){
    z[[i]]$id <- i
  }
  
  x2 <- lapply(z,
               function(x){
                 aa <- sapply(x$fitness, min)
                 data_frame(id = x$id,
                            iter = x$iteracion,
                            min = aa,
                            tam_poblacion = x$parametro$tam_poblacion,
                            
                            prob_cruce = x$parametro$prob_cruce,
                            tam_torneo = x$parametro$tam_torneo,
                            prob_mutacion = x$parametro$prob_mutacion)
               }
  )
  
  return(bind_rows(x2))
  
}

# Éxitos ------------------------------------------------------------------

hits <- function(f,
                 ind, 
                 
                 U = 1/10,
                 K0 = 1,
                 K1 = 10,
                 
                 a, b, N,
                 
                 h = 10e-5){
  
  
  
  delta_x <- (b - a)/N
  
  f_eval <- f(a + (0:N)*delta_x)
  
  F_prima_eval <- F_prima(a + (0:N)*delta_x, h = h, F_hat = function(x) eval(as.expression(ind)))
  
  y <- abs(F_prima_eval - f_eval)
  
  omega <- ifelse(y <= U, 
                  1,
                  0)
  
  if (is.finite(mean(omega)) && mean(omega) == 1){
    return(1)
  } else { 
    return(0)  
  }
  
}


funcion_mejor_ind <- function(y,
                              
                              a,
                              b,
                              
                              N,
                              
                              f, 
                              f_reparacion,
                              wrappings){
  best_ind <- lapply(y, function(x) which.min(x$fitness[[length(x$fitness)]]))
  best_individuos <- pmap(list(x = y, ind = best_ind), function(x, ind) x$poblacion_final[[ind]])
  
  poblacion_inicial_decod <- mclapply(best_individuos,
                                      GrammarMap,
                                      grammar = grammar_def,
                                      wrappings = wrappings,
                                      mc.cores = 10)
  
  i_no_null <- sapply(best_individuos, function(x)!is.null(x))
  
  poblacion_reparada <- map(poblacion_inicial_decod[i_no_null], 
                            f_reparacion)
  
  
  
  best_individuos[i_no_null] <- map(poblacion_reparada[i_no_null], hits, 
      a = a,
      b = b,
      
      N = N,
      
      f = f)
  
  
  return(
    lapply(best_individuos,
           function(x){
             if (is.null(x)){
               return(0)
             } else {
               return(x)
             }
           })
  )
  
  
}



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


miau <- function(x){
  aa <- sapply(x$fitness, min)
  data_frame(id = x$id,
             iter = x$iteracion,
             min = aa,
             tam_poblacion = x$parametro$tam_poblacion,
             
             prob_cruce = x$parametro$prob_cruce,
             tam_torneo = x$parametro$tam_torneo,
             prob_mutacion = x$parametro$prob_mutacion)
}

for (i in seq_along(z)){
  print(i)
  mi <- miau(z[[i]])

               

  
}
