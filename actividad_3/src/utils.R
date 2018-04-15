

F_prima <- function(x, h = 10e-5, F_hat){
  (F_hat(x + h) - F_hat(x))/h
}


#' Title
#'
#' @param f function
#' @param poblacion list. 
#' @param x 
#' @param U 
#' @param K0 
#' @param K1 
#'
#' @return
#' @export
#'
#' @examples
fitness_act_3 <- function(f,
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
  if (sum(y <= U, na.rm = TRUE) == length(y)){
    res <- 0
  } else {
    omega <- ifelse(y <= U, 
                    K0,
                    K1)
    res <- (1/(N+1)) * sum(omega * y)
  }
  
  if (is.na(res)){
    return(Inf)
  } else { 
    return(res)  
  }
  
}

funcion_fitness <- function(poblacion, a, b, N, f, f_reparacion, wrappings){
  
  poblacion_inicial_decod <- mclapply(poblacion,
                                      GrammarMap,
                                      grammar = grammar_def,
                                      wrappings = wrappings,
                                      mc.cores = 10)
  
  terminales <- map_lgl(poblacion_inicial_decod, function(x) x$type != "NT")
  
  poblacion_reparada <- map(poblacion_inicial_decod[terminales], 
                            f_reparacion)
  
  fitness <- map(poblacion_reparada,
                 fitness_act_3,
                 a = a, b = b, N = N, f = f)
  
  poblacion_inicial_decod[!terminales] <- Inf
  poblacion_inicial_decod[terminales] <- fitness
  return(poblacion_inicial_decod)
}



F_integral <- function(x) eval(as.expression(expr))
f_a_integrar <- function(x) eval(expression(6*x^2))


f_reparacion <- function(expr, x = 0, f_x = 5){
  y <- eval(as.expression(expr))
  alpha <- f_x - y
  
  as.expression(parse(text = paste(as.character(expr), "+", alpha)))
  
}
