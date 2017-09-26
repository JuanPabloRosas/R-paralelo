library(lattice)
library(reshape2)
library(plot3D)
library(rgl)
d <- data.frame()
mov <- data.frame()

#---------------------------------------------------------------------------------
g <- function(x, y) {
  func1 <- ((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100
  func2 <- (x^2 + y^2)^(1/3)
  func3 <- sin(x)*sin(y)^2
  func4 <- -sin(sqrt(x^2 + y^2)/(x^2 + y^2))
  return(-func1)
}
#---------------------------------------------------------------------------------

low <- -6
high <- 5
step <- 0.1
replicas <- 100
x <- seq(low, high, length.out=45)
y <-  x

#----------------------------DIBUJA CADA PUNTO-----------------------------------------------------
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
  nombre <- paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/reto1/p7_00", t, ".png", sep="")
  }
  else if(t >= 100){
    nombre <- paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/reto1/p7_", t, ".png", sep="")
  }
  else{
    nombre <- paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/reto1/p7_0", t, ".png", sep="")
  }
  png(nombre, width=700, height=500)
  plot(levelplot(z ~ x * y, data = d), main ="t")
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(a,b, pch=19, col="blue", cex=1)
  trellis.unfocus()
  graphics.off()
}
#---------------------------------------------------------------------------------

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

for (pot in 2:4) {
    tmax <- 10^pot
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
    
    png(paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/p7_densidad.png",tmax, sep=""), width=700, height=500)
    f2 <- kde2d(output$x , output$y, z ,n = 100, lims = c(-6, 6, -6, 6))
    image(f2)
    filled.contour(f2,plot.axes = {points(10, 10) },color.palette=colorRampPalette(c('white','blue','darkblue','yellow','red','darkred')))
    graphics.off()
    
    png(paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/p7_3d.png", sep=""), width=700, height=500)
    persp(x, y, z, shade=0.2, col='orange', theta=40, phi=30)
    graphics.off()
    png(paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p7/p7_", tmax, ".png", sep=""), width=700, height=500)
    plot(levelplot(z ~ x * y, data = d))
    trellis.focus("panel", 1, 1, highlight=FALSE)
    lpoints(output$x,output$y, pch=19, col="blue", cex=1)
    trellis.unfocus()
    trellis.focus("panel", 1, 1, highlight=FALSE)
    lpoints(output$x[mejor],output$y[mejor], pch=19, col="red", cex=1)
    trellis.unfocus()
    graphics.off()
}
stopImplicitCluster()

#----------------------Plot 3D manipulable---------------------------------
#persp3d(x,y,z, axes=TRUE,scale=3, box=TRUE,xlab="X-value", ylab="Y-value", zlab="Z-value", 
#        main="Función 1",nticks=5, ticktype="detailed", col = "yellow")
#browseURL(paste("file://", writeWebGL(dir=file.path(tempdir(), "webGL"), width=500), sep=""))
#-----------------------------------------------------------------------
