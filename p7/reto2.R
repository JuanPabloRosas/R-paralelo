library(lattice)
library(reshape2)
library(plot3D)
library(rgl)
d <- data.frame()
mov <- data.frame()
g <- function(x, y) {
  func1 <- ((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100
  func2 <- (x^2 + y^2)^(1/3)
  func3 <- sin(x)*sin(y)^2
  func4 <- -sin(sqrt(x^2 + y^2)/(x^2 + y^2))
  return(-func1)
}

low <- -6
high <- 5
step <- 0.1
replicas <- 100
x <- seq(low, high, length.out=45)
y <-  x

dibuja <-function(t,a,b){
  library(lattice)
  z <-outer(x,y,g)
  d <- data.frame()
  for(i in x){
    for(j in y){
      d <- rbind(d,c(i,j,g(i,j)))
    }
  }
  names(d) <- c("x", "y", "z")
  if(t <= 9){
    nombre <- paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/reto2/p7_00", t, ".png", sep="")
  }
  else if(t >= 100){
    nombre <- paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/reto2/p7_", t, ".png", sep="")
  }
  else{
    nombre <- paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/reto2/p7_0", t, ".png", sep="")
  }

  png(nombre, width=700, height=500)
  plot(levelplot(z ~ x * y, data = d), main ="t")
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(a,b, pch=19, col="blue", cex=1)
  trellis.unfocus()
  graphics.off()
}

replica <- function(t) {
  curr_x <- runif(1, low, high)
  curr_y <- runif(1, low, high)
  curr <- c(curr_x, curr_y)
  T = 12
  
  for (tiempo in 1:t) {
    Delta <- runif(1, -.05, .05)
    dibuja(tiempo,curr_x,curr_y)
    if(runif(1,0,1) < .5){
      while(TRUE){
        if(curr_x + Delta > high | curr_x + Delta < low){
          Delta <- runif(1, -step, step)
        }
        else{
          nuevo <- c(curr_x + Delta, curr_y)
          break
        }
      }
    }
    else{
      while(TRUE){
        if(curr_y + Delta > high | curr_y + Delta < low){
          Delta <- runif(1, -step, step)
        }
        else{
          nuevo <- c(curr_x, curr_y + Delta)
          break
        }
      }
    }
    
    delta <- g(nuevo[1],nuevo[2]) - g(curr_x,curr_y)
    
    if(delta > 0)
    {
      curr_x <- nuevo[1]
      curr_y <- nuevo[2]
      curr <- c(curr_x,curr_y)
    }
    else
      {
        if(runif(1,0,1) < exp(-delta/T))
        {
          curr_x <- nuevo[1]
          curr_y <- nuevo[2]
          T <- T * .995
          curr <- c(curr_x,curr_y)
        }
      }
    }
  return(curr)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

#for (pot in 2:5) {
tmax <- 10^2
output <- data.frame()
resultados <- foreach(i = 1, .combine=c) %dopar% replica(tmax)
resultados <- data.frame(resultados)
colnames(resultados) <- c("x")
o <- 1
while(TRUE){
  indice <- o * 2
  output <- rbind(output, c(resultados$x[indice - 1] , resultados$x[indice]))
  o <- o + 1
  if(o == 101)
  {
    break
  }
}

colnames(output) <- c("x","y")
mejores <- data.frame()
for(z in 1:100){
  mejores <- rbind(mejores, g(output$x[z],output$y[z]))
}
colnames(mejores) <- c("z")
mejor <- which.max(mejores$z)

z <-outer(x,y,g)
d <- data.frame()
for(i in x){
  for(j in y){
    d <- rbind(d,c(i,j,g(i,j)))
  }
}
names(d) <- c("x", "y", "z")
colnames(output) <- c("x","y")


png(paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/p7_", tmax, ".png", sep=""), width=700, height=500)
plot(levelplot(z ~ x * y, data = d))
trellis.focus("panel", 1, 1, highlight=FALSE)
lpoints(output$x,output$y, pch=19, col="blue", cex=1)
trellis.unfocus()
trellis.focus("panel", 1, 1, highlight=FALSE)
lpoints(output$x[mejor],output$y[mejor], pch=19, col="red", cex=1)
trellis.unfocus()
graphics.off()
#}
stopImplicitCluster()

