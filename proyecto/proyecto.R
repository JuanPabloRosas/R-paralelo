library(parallel)
library(ggplot2)
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

datos <-((lee("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/instancias_pequeñas/problem1m-1t-2s-5a-2020800U-4.pddl.txt")))
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
  c <- length(unlist(as.list(strsplit(datos[9 + as.numeric(datos[3]) + 1 + s], " "))))
  for(i in 1:c){
    tema <- c(tema,c(unlist(as.list(strsplit(datos[9 + as.numeric(datos[3]) + 1 + s], " ")))[i],s)) 
  }
}
tema <- na.omit(tema)
part_tema <- matrix(tema,ncol = 2, byrow = TRUE )

##-----------------------------------  OBTIENE A QUE MATERIA PERTENECE  -------------------##
mate <- matrix()
for(s in 1:datos[1]){ #Debe ser hasta la cantidad de materias
  c <- length(unlist(as.list(strsplit(datos[9 + as.numeric(datos[3]) + as.numeric(datos[2]) + 1 + s], " "))))
  for(i in 1:c){
    mate <- c(mate,c(unlist(as.list(strsplit(datos[9 + as.numeric(datos[3]) + as.numeric(datos[2]) + 1 + s], " ")))[i],s)) 
  }
}
mate <- na.omit(mate)
part_mat <- matrix(mate,ncol = 2, byrow = TRUE )

##------------------------------------  OBTIENE REQUISITOS  ------------------------------##
requiere <- matrix(c(0,0),nrow=as.numeric(datos[4]),ncol = 2)
if(as.numeric(datos[5]) != 0){
  for(s in 1:datos[4]){ 
    for(l in 1:datos[5]){#Debe ser hasta la cantidad de act con requisitos
      c <- unlist(as.list(strsplit(datos[9 + as.numeric(datos[3]) + as.numeric(datos[2]) + as.numeric(datos[1]) + 3 + l], " ")))
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

tiempos <- function(i){
  act <- actividades[unlist(muestra[i]),]
  return(colSums(act[,1:2])[2])
}

montecarlo1 <- function(i){
  seleccion <- data.frame() # Guarda las actividades seleccionadas
  puntos <- matrix(0,nrow = as.numeric(datos[3])) #Inicializa matriz de puntos por sub
  for(n in 1:as.numeric(datos[3])){
    muestra <- matrix(actividades[actividades[,4] == n], ncol=5)
    while(puntos[n] < 70){ # Restriccion mayor a 70 por subtema
      i <- sample(length(muestra[,1]),1)
      a<-muestra[i,]
      muestra <- muestra[-i,]
      puntos[a[4]] <- puntos[a[4]] + a[1]
      cual <- which(apply(actividades, 1, function(actividades) all(actividades == a)))
      seleccion <- rbind(seleccion, cual)
    }
  }
  return(seleccion)
}
    
##----------------------------------- EXPERIMENTACION  ----------------------------------##

d <- data.frame()
mejores <- list()
cluster <- makeCluster(detectCores() - 1)
tamaños <- data.frame()
iteraciones <- 10
for(h in 1:iteraciones){
  print(h)
  ##////             PARALELO
  repeticiones <- 3628800 * .1 # Tardo 40 min para 10 iteraciones
  clusterExport(cluster, "datos")
  clusterExport(cluster, "actividades")
  clusterExport(cluster, "montecarlo1")
  muestra <- parSapply(cluster, 1:repeticiones, montecarlo1)
  
  clusterExport(cluster, "tiempos")
  clusterExport(cluster, "muestra")
  clusterExport(cluster, "actividades")
  tiempo <- parSapply(cluster, 1:repeticiones, tiempos)
    
  #nombre <- paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/soluciones/",h,"_sol.png")
  #png(nombre)
  #plot(promedios, tiempo, xlab = "Puntaje promedio", ylab="Tiempo", xlim = c(25,150), ylim = c(2300,3000), main = h)
  #abline(v = 100, col="red")
  #graphics.off()
  
  planes <- data.frame(table(unlist(muestra)))
  mejor <- data.frame(table(unlist(muestra[which.min(tiempo)])))
  #------------  CUANTAS ACTIVIDADES SE NECESITAN PARA SATISFACER ---------------------
  tamaño <- length(mejor[,1])
  tamaños <- rbind(tamaños,tamaño)
  #------------------------------------------------------------------------------------
  mejores <- c(mejores ,data.frame(mejor$Var1))
  d <- rbind(d,planes)
}
stopCluster(cluster)

dat <- data.frame()
for(i in 1:iteraciones){
  dat <- rbind(dat,mejores[i])
}

capture.output(print(mejores), file = "/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/mejores.csv")
repite <-data.frame(table(dat))
repite <- repite[order(repite$Freq, decreasing=T),]
nombre <- paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/mejores_act.png")
png(nombre)
ggplot(data=repite, aes(x=repite$dat, y=repite$Freq, group=1)) +  geom_line(color="red")+  geom_point()
graphics.off()

#________________________________________________________________________________________________________
#files <- list.files(path="/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/instancias_pequeñas", pattern=".txt", all.files=T, full.names=T)
#for(file in files){
#  print(file)
#}