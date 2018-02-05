suma_powell <- function(x){
  return(sum(abs(x)^c(2:(length(x) + 1))))
}


suma_powell(c(1, -1, 1, 1, 1))


xin_she_yang_4 <- function(x){
  s <- sum(sin(x)^2)
  exp1 <- exp(-sum(x^2))
  exp2 <- exp(-sum(sin(sqrt(abs(x)))^2))
  
  return((s - exp1)*exp2)
}

xin_she_yang_4(1:2)


# 
# # Gráficos ----------------------------------------------------------------
# library(plotly)
# library(magrittr)
# library(plot3D)
# 
# set.seed(123)
# n <- 50
# x <- seq( -1, 1, length.out = n)
# y <- seq( -1, 1, length.out = n)
# a <- expand.grid(x, y)
# z <- apply(a, 1, suma_powell)
# 
# d <- data.frame(a, z)
# ggplot(d, aes(Var1, Var2, z = z)) + geom_raster(aes(fill = z)) + 
#   geom_contour(color = "white") + 
#   ggthemes::theme_fivethirtyeight()
# 
# ggplot(d, aes(Var1, Var2, z = z)) + geom_contour(binwidth = 0.01) + 
#   coord_equal()
# ggplot(d, aes(Var1, z)) + geom_point(aes(color = -z))
# 
# v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
# v + geom_contour(binwidth = 0.1)
# 
# image(x, y, z)
# x <- matrix(x, nrow = 10, ncol = 10, byrow = TRUE)
# y <- matrix(y, nrow = 10, ncol = 10, byrow = TRUE)
# z <- matrix(z, nrow = 10, ncol = 10, byrow = TRUE)
# 
# outer(x, y, "sum")
# x <- cbind(x, y)
# p <- plot_ly(x, x = ~V1, y = ~V2, z = ~y) %>% add_surface()
# 
# surf3D(x = x, y = y, z = z)
# 
# 
# X       <- seq(0, pi, length.out = 50)
# Y       <- seq(0, 2*pi, length.out = 50)
# M       <- mesh(X, Y)
# phi     <- M$x
# theta   <- M$y
# 
# # x, y and z grids
# r <- sin(4*phi)^3 + cos(2*phi)^3 + sin(6*theta)^2 + cos(6*theta)^4
# x <- r * sin(phi) * cos(theta)
# y <- r * cos(phi)
# z <- r * sin(phi) * sin(theta)
# 
# 
# 
# par(mar = c(2, 2, 2, 2))
# par(mfrow = c(1, 1))
# 
# x <- seq(-1, 1, length.out=50)
# y <- x
# M <- mesh(x, y)
# 
# alpha <- M$x
# beta <- M$y
# 
# a <- cbind(as.vector(alpha), as.vector(beta))
# 
# z <- apply(a, 1, suma_powell)
# 
# b <- matrix(z, nrow = 50, ncol = 50)
# 
# surf3D(x      = alpha,
#        y      = beta,
#        z      = b,
#        colkey = FALSE,
#        bty    = "b2",
#        main   = "Función suma de Powell ")
