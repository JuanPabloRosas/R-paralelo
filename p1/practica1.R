library(parallel)
repetir <- 100
duracion <-10000
eucl <- TRUE
dimenciones <- 20
datos <-  data.frame()
x <- data.frame()

caminata <- function(dim) {
  pos <- rep(0, dim)
  mayor <- 0
  reg_orig <- 0
  for (t in 1:duracion) {
    cambiar <- sample(1:dim, 1)
    cambio <- 1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    pos[cambiar] <- pos[cambiar] + cambio
    if(all(pos == 0)){
      reg_orig = reg_orig + 1
    }
    if (eucl) {
      d <- sum(sqrt(pos**2))
    } else { # Manhattan
      d <-  sum(abs(pos))
    }
    if (d > mayor) {
      mayor <- d
    }
  }
  m <- matrix(c(reg_orig,mayor), nrow = 1, ncol = 2)
  return(m)
}

for(i in 1:dimenciones){
	for(j in 1:repetir){
	   t<-system.time(r<-caminata(i))
	   t <- matrix(c(t),nrow=1, ncol = 1)
	   d = cbind(t,r)
	   x <- rbind(x,d)
	}
	r1 <- mean(x$V1)
	r2 <- round(mean(x$V2))
	r3 <- round(mean(x$V3))
	x <- x[0,]
	r <- matrix(c(r1,r2,r3), nrow = 1,ncol = 3)
	colnames(r) <- c("Tiempo","Origen", "Distacia")
	datos <- rbind(datos,r)
}
dim <- matrix(c(1:dimenciones))
datos <- cbind(dim,datos)
print(datos)

if (eucl) {
  png("p1er_tarea_tiempo.png")
  #par(mfrow=c(1,3))
  plot(range(datos$dim),range(datos$Tiempo), type = "n", xlab = "Dimension", ylab = "Tiempo de Computo", xaxt="n", main = "Tiempo de Computo - no paralelo")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Tiempo, type = 'b')
  
  png("p1er_tarea_origen.png")
  plot(range(datos$dim),range(datos$Origen), type = "n", xlab = "Dimension", ylab = "Regresa al Origen", xaxt="n", main = "Regresa al origen - no paralelo")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Origen, type = 'b')
  
  png("p1er_tarea_distancia.png")
  plot(range(datos$dim),range(datos$Distacia), type = "n", xlab = "Dimension", ylab = "Distancia desde el Origen", xaxt="n",main = "Distancia - no paralelo")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Distacia, type = 'b')
} else {
  png("p1mr_tarea.png", width = 1200, height = 650)
  par(mfrow=c(1,3))
  plot(range(datos$dim),range(datos$Tiempo), type = "n", xlab = "Dimension", ylab = "Tiempo de Computo", xaxt="n", , main = "Tiempo de Computo - no paralelo")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Tiempo, type = 'b')
  
  plot(range(datos$dim),range(datos$Origen), type = "n", xlab = "Dimension", ylab = "Regresa al Origen", xaxt="n",, main = "Regresa al origen - no paralelo")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Origen, type = 'b')
  
  plot(range(datos$dim),range(datos$Distacia), type = "n", xlab = "Dimension", ylab = "Distancia desde el Origen", xaxt="n",main = "Distancia - no paralelo")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Distacia, type = 'b')
}
graphics.off()

