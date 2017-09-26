library(lattice) # lo mismo aplica con este paquete
library(reshape2)
library(rgl)
d <- data.frame()
g <- function(x, y) {
  func2 <- (sin(x)^2 +cos(y)^2)
    return(func2)
}

low <- -2
high <- 5
step <- 0.35
replicas <- 100
x <- seq(low, high, length.out=45)
y <-  x
z <- outer(x,y,g)
for(i in x){
  for(j in y){
    d <- rbind(d,c(i,j,g(i,j)))
  }
}
names(d) <- c("x", "y", "z")

png(paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/p7_funcion.png", sep=""), width=700, height=500)
plot(levelplot(z ~ x * y, data = d))
graphics.off()

replica <- function(t) {
  curr_x <- runif(1, low, high)
  curr_y <- runif(1, low, high)
  curr <- c(curr_x, curr_y)
  bestval <- g(curr_x, curr_y)
  bestpos <- c(curr_x, curr_y)
  
  for (tiempo in 1:t) {
    dibuja(tiempo,curr_x,curr_y)
    delta <- runif(1, 0, step)
    op = c(curr_x - delta, curr_y, curr_x + delta, curr_y, curr_x, curr_y - delta, curr_x, curr_y + delta)
    posibles = numeric()
    for (i in 1:4){ 
      posibles <- c(posibles, g(op[2*i - 1], op[2*i]))
    }
    
    mejor <- which.max(posibles)
    nuevo = posibles[mejor]
    
    if (nuevo > bestval) {
      curr_x <- op[(2*mejor - 1)]
      curr_y <- op[(2*mejor)]
      curr <- c(curr_x,curr_y)
      bestval <- nuevo
    }
  }
  return(curr)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (pot in 2:5) {
    tmax <- 10^pot
    resultados <- foreach(i = 1:tmax, .combine=c) %dopar% replica()
    mejor <- which.max(resultados)
    print(paste(tmax, "iteraciones" ," Mejor Valor: ",resultados[mejor]))
}
stopImplicitCluster()
