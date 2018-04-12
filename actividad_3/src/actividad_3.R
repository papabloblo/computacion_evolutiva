
library(gramEvol)
library(purrr)
library(parallel)

source("actividad_1/R/func_algoritmo_genetico.R")
source("actividad_1/R/algoritmo_genetico.R")

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

set.seed(1234)
poblacion_inicial <- generacion_poblacion(valores_posibles = 0:3,
                                          num_genes = 50,
                                          tam_poblacion = 100,
                                          num_genes_fijo = T)



# Función a integrar ------------------------------------------------------

f <- function(x){
  6*x^2
}


F_integral <- function(x) eval(as.expression(expr))
f_a_integrar <- function(x) eval(expression(6*x^2))

# Ejecución algoritmo -----------------------------------------------------



num_padres    = 2
prob_cruce    = 0.75
tam_torneo    = 3
prob_mutacion = 0.5
tam_poblacion = 100

max_iter      = 100
print_each    = 1
valores_mutacion <- 0:3


z <- algoritmo_genetico(poblacion_inicial = poblacion_inicial,
                   funcion_fitness = funcion_fitness,
                   valores_mutacion = 0:3,
                   num_padres = 2,
                   prob_cruce =  0.5,
                   tam_torneo = 4,
                   prob_mutacion = 0.5,
                   tam_poblacion = length(poblacion_inicial),
                   
                   
                   max_iter = 100,
                   print_each = 1,
                   
                   a = 0,
                   b = 5,
                   
                   N = 50,
                   f = function(x) 6*x^2,
                   f_reparacion = funcion_reparacion
                   
                   )

