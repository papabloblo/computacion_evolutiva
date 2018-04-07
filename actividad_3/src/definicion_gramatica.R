library(gramEvol)


# Definición de la gramática ----------------------------------------------

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
grammar_def

expr <- GrammarMap(c(1, 7, 106), grammar_def)
x <- 10
eval(as.expression(expr))



# Función fitness ---------------------------------------------------------

f <- function(x){
  6*x^2
}
f(2)

F_eval <- function(f_a_integrar,
                   F_integral, 
                   x,
                   U = 1/10,
                   K0 = 1,
                   K1 = 10){
  
  y <- abs(F_prima(a + (0:N)*delta_x) - f_a_integrar(a + (0:N)*delta_x))
  omega <- ifelse(y <= U, 
                  K0,
                  K1)
  
  (1/(N+1)) * sum(omega * y)
}

F_prima <- function(x, h){
  (F_integral(x + h) - F_integral(x))/h
}


F_integral <- function(x) eval(as.expression(expr))
f_a_integrar <- function(x) eval(expression(6*x^2))

x <- 1:3
eval(f_a_integrar)
eval(F_integral)

F_integral(1:10)
f_a_integrar(1:10)



# Pruebas -----------------------------------------------------------------

GrammarMap(c(1, 3, 3, 0, 3, 0), grammar_def)
GrammarMap(c(3, 0, 1), grammar_def)
