library(parallel)
##------------------------------------   LEE Y EXTRA DATOS PDDL --------------------------##
lee = function(path) {
  archivo <- data.frame()
  con = file(path, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    a <- line
    archivo <-c(archivo, a)
  }
  close(con)
  return(data.frame(archivo))
}
datos <-((lee("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/datos_p3.txt")))
colnames(datos)<-c(1: length(datos))
datos <-t(datos)

valor <- matrix(unlist(strsplit(datos[6]," ")), nrow= as.numeric(datos[4]))
duracion <- matrix(unlist(strsplit(datos[7]," ")), nrow= as.numeric(datos[4]))
recurso <- matrix(unlist(strsplit(datos[8]," ")), nrow= as.numeric(datos[4]))

##-------------------------------------  OBTIENE A QUE SUBTEMA PERTENECE ------------------##
subs <- matrix()
for(s in 1:as.numeric(datos[3])){ #Debe ser hasta la cantidad de subtemas
  c <- length(unlist(as.list(strsplit(datos[9 + s], " "))))
  for(i in 1:c){
    subs <- c(subs,c(unlist(as.list(strsplit(datos[9 + s], " ")))[i],s)) 
  }
}
subs <- na.omit(subs)
part_sub <- matrix(subs,ncol = 2, byrow = TRUE)

##------------------------------------   OBTIENE A QUE TEMA PERTENECE  --------------------##
tema <- matrix()
for(s in 1:as.numeric(datos[2])){ #Debe ser hasta la cantidad de temas
  c <- length(unlist(as.list(strsplit(datos[40 + s], " "))))
  for(i in 1:c){
    tema <- c(tema,c(unlist(as.list(strsplit(datos[40 + s], " ")))[i],s)) 
  }
}
tema <- na.omit(tema)
part_tema <- matrix(tema,ncol = 2, byrow = TRUE )

##-----------------------------------  OBTIENE A QUE MATERIA PERTENECE  -------------------##
mate <- matrix()
for(s in 1:datos[1]){ #Debe ser hasta la cantidad de materias
  c <- length(unlist(as.list(strsplit(datos[46 + s], " "))))
  for(i in 1:c){
    mate <- c(mate,c(unlist(as.list(strsplit(datos[46 + s], " ")))[i],s)) 
  }
}
mate <- na.omit(mate)
part_mat <- matrix(mate,ncol = 2, byrow = TRUE )

##------------------------------------  OBTIENE REQUISITOS  ------------------------------##
requiere <- matrix(c(0,0),nrow=as.numeric(datos[4]),ncol = 2)
if(as.numeric(datos[5]) != 0){
  for(s in 1:datos[4]){ 
    for(l in 1:datos[4]){#Debe ser hasta la cantidad de act con requisitos
      c <- unlist(as.list(strsplit(datos[49 + l], " ")))
      requiere[as.integer(c[1]),]<-c(c[1],c[2])
    }
  }
}

##------------------------------------  ACTIVIDADES -------------------------------------##
actividades <- cbind(valor,duracion,recurso,part_sub[,2],requiere[,2])
actividades <- as.numeric(actividades)
actividades <- matrix(actividades, nrow = as.numeric(datos[4]))
colnames(actividades) <- c("valor","duracion","recurso","sub","requiere")

##------------------------------------  MONTECARLO  -------------------------------------##

montecarlo1 <- function(i){
  cantidad <- round(as.numeric(datos[4]) * .68)  #Cantidad de subtemas como minimo
  muestra <- sample(c(1:as.numeric(datos[4])),cantidad) #Cantidad de Actividades
  return(muestra)
}

tiempos <- function(i){
  act <- actividades[muestra[i,],]
  return(colSums(act[,1:2])[2])
}

puntaje <- function(i){
  act <- actividades[muestra[i,],]
  cuantos <- length(muestra[i,])
  puntos <- matrix(0,nrow = as.numeric(datos[3]))
  for(j in 1:cuantos){ # Cantidad de subtemas
    a<-act[j,]
    puntos[a[4]] <- puntos[a[4]] + a[1]
  }
  return(puntos)
}

valida <- function(i){
  for(c in 1:as.numeric(datos[3])){
    if(puntaje[i,c] < 70){
      return(0)
    }
  }
  return(i)
}

promedio <- function(i){
  return(sum(puntaje[i,])/ as.numeric(datos[3]))
}
##----------------------------------- EXPERIMENTACION  ----------------------------------##
##////             PARALELO
repeticiones <- 1000000
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "datos")
clusterExport(cluster, "actividades")
clusterExport(cluster, "montecarlo1")
muestra <- parSapply(cluster, 1:repeticiones, montecarlo1)
muestra <- t(muestra)

clusterExport(cluster, "tiempos")
clusterExport(cluster, "muestra")
clusterExport(cluster, "actividades")
tiempo <- parSapply(cluster, 1:repeticiones, tiempos)

clusterExport(cluster, "puntaje")
puntaje <- parSapply(cluster, 1:repeticiones, puntaje)
puntaje <- t(puntaje)

png("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/puntaje_subtema.png")
boxplot(puntaje, xlab="Subtemas", ylab="Puntaje")
abline(h = 70, col="red")
graphics.off()

clusterExport(cluster, "valida")
clusterExport(cluster, "puntaje")
clusterExport(cluster, "datos")
valida <- parSapply(cluster, 1:repeticiones, valida)


clusterExport(cluster, "promedio")
clusterExport(cluster, "puntaje")
clusterExport(cluster, "datos")
promedio <- parSapply(cluster, 1:repeticiones, promedio)
stopCluster(cluster)

png("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/soluciones.png")
plot(promedio, tiempo, xlab = "Puntaje promedio", ylab="Tiempo", xlim = c(65,130))
abline(v = 100, col="red")
graphics.off()

d <- valida[valida != 0]
muestra <- muestra[d,]
tiempo <- tiempo[d]
if(is.integer(d)){
  puntaje <- puntaje[d,]
  
  promedio <- data.frame()
  for(p in 1:length(d)){
    promedio <- rbind(promedio, sum(puntaje[p,])/ as.numeric(datos[3]))
  }
  colnames(promedio) <- "promedio"

  png("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/factibles.png")
  plot(promedio$promedio, tiempo, xlab = "Puntaje promedio", ylab="Tiempo", xlim = c(65,130))
  abline(v = 100, col="red")
  graphics.off()
}
##-------------------------------------------------------



