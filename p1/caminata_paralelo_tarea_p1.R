library(parallel)
repetir <- 100
duracion <-10000
eucl <- TRUE
dimenciones <- 10
datos <-  data.frame()

caminata <- function(replica) {
  pos <- rep(0, i)
  mayor <- 0
  reg_orig <- 0
  for (t in 1:duracion) {
    cambiar <- sample(1:i, 1)
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

#Paralelo
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
clusterExport(cluster, "eucl")
clusterExport(cluster, "dimenciones")
clusterExport(cluster, "caminata")


for(i in 1:dimenciones){
  clusterExport(cluster, "i")
  t <- system.time(r <- parSapply(cluster, 1:repetir, caminata))
  #t<-system.time(r<-caminata(i,dur,dist))
  t <- matrix(c(t),nrow=1, ncol = 1)
  r1 <- mean(r[1])
  r2 <- mean(r[2])
  r <- matrix(c(r1,r2), nrow = 1,ncol = 2)
  d = cbind(t,r)
  colnames(d) <- c("Tiempo","Origen", "Distacia")
  datos <- rbind(datos,d)
}
stopCluster(cluster)
dim <- matrix(c(1:dimenciones))
datos <- cbind(dim,datos)
print(datos)

if (eucl) {
  png("p1er_tarea_p_tiempo.png")
  #par(mfrow=c(1,3))
  plot(range(datos$dim),range(datos$Tiempo), type = "n", xlab = "Dimension", ylab = "Tiempo de Computo", xaxt="n")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Tiempo, type = 'b')
  
  png("p1er_tarea_p_origen.png")
  plot(range(datos$dim),range(datos$Origen), type = "n", xlab = "Dimension", ylab = "Regresa al Origen", xaxt="n")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Origen, type = 'b')
  
  png("p1er_tarea_p_distancia.png")
  plot(range(datos$dim),range(datos$Distacia), type = "n", xlab = "Dimension", ylab = "Distancia desde el Origen", xaxt="n")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Distacia, type = 'b')
} else {
  png("p1mr_tarea.png", width = 1200, height = 650)
  par(mfrow=c(1,3))
  plot(range(datos$dim),range(datos$Tiempo), type = "n", xlab = "Dimension", ylab = "Tiempo de Computo", xaxt="n")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Tiempo, type = 'b')
  
  plot(range(datos$dim),range(datos$Origen), type = "n", xlab = "Dimension", ylab = "Regresa al Origen", xaxt="n")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Origen, type = 'b')
  
  plot(range(datos$dim),range(datos$Distacia), type = "n", xlab = "Dimension", ylab = "Distancia desde el Origen", xaxt="n")
  axis(1, xaxp=c(1,dimenciones,dimenciones - 1), las=2)
  lines(datos$dim, datos$Distacia, type = 'b')
}
graphics.off()

