
# Parámetros del algoritmo ------------------------------------------------

num_pruebas = 10,
generacion_poblacion_ini,
tam_poblacion,
valores_posibles,
funcion_fitness,

genes_fijos,
num_genes,

valores_mutacion,

num_padres,
prob_cruce,
tam_torneo,
prob_mutacion,

max_iter,
print_each


poblacion <- generacion_poblacion(valores_posibles = 0:10,
                     num_genes = 5,
                     tam_poblacion = 10,
                     num_genes_fijo = F)



num_padres    = 2
prob_cruce    = 0.75
tam_torneo    = 2
prob_mutacion = 0.5
tam_poblacion = 100

max_iter      = 100
print_each    = 1
valores_mutacion <- 0:3




num_padres = num_padres,
prob_cruce = prob_cruce,
k          = tam_torneo