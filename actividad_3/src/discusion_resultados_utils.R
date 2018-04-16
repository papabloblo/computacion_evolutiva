
# Tabla de resultados -----------------------------------------------------


res <- function(z){
  
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
  
  poblacion_reparada <- map(poblacion_inicial_decod, 
                            f_reparacion)
  
  map(poblacion_reparada, hits, 
      a = a,
      b = b,
      
      N = N,
      
      f = f)
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
