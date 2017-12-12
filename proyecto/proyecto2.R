library(parallel)
library(ggplot2)

cont = 0
optimo = 0
d <- data.frame()
mejores <- list()
tiempos <- data.frame()
mejores_tiempos <- data.frame()
mejores_promedios <- data.frame()
cuantos_optimos <- data.frame()
tamaños <- data.frame()
tiempo_computo <- data.frame()
cluster <- makeCluster(detectCores() - 1)
tiempos_optimos <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/m/V.txt", sep="\n")
ruta <- "/home/juanpablo/Documentos/simulacion/instancias/m/V"
ruta2 <- "/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/V"
files <- list.files(path=ruta, pattern=".txt", all.files=T, full.names=T)

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
##----------------------------------------------------------------------------------------##
##------------------------------------  MONTECARLO ---------------------------------------##
montecarlo1 <- function(i){
  seleccion <- data.frame(0) # Guarda las actividades seleccionadas
  puntos <- matrix(0,nrow = as.numeric(datos[3])) #Inicializa matriz de puntos por sub
  copia_act <- matrix(actividades,nrow = as.numeric(datos[4]))
  copia_act <- cbind(copia_act,0)
  while(!all(puntos > 70)){ # Restriccion mayor a 70 por subtema
    while(TRUE){
      i <- sample(as.numeric(datos[4]),1)
      act<-copia_act[i,]
      if((copia_act[i,7] == 0) & (puntos[act[4]] + act[4]) < 100){
        break;
      }
    }
    if(act[5] != 0){
      req1 <- actividades[act[5],]
      if(act[6] != 0){
        req2 <- actividades[act[6],]
        if(!any(seleccion == act[5])){
          if(!any(seleccion == act[6])){
            puntos[req1[4]] <- puntos[req1[4]] + req1[1]
            seleccion <- rbind(seleccion, act[5])
            puntos[req2[4]] <- puntos[req2[4]] + req2[1]
            seleccion <- rbind(seleccion, act[6])
            puntos[act[4]] <- puntos[act[4]] + act[1]
            seleccion <- rbind(seleccion, i)
            
            copia_act[act[5],7] <- 1
            copia_act[act[6],7] <- 1
            copia_act[i,7] <- 1
            
          } else {
            puntos[req1[4]] <- puntos[req1[4]] + req1[1]
            seleccion <- rbind(seleccion, act[5])
            puntos[act[4]] <- puntos[act[4]] + act[1]
            seleccion <- rbind(seleccion, i)
            
            copia_act[act[5],7] <- 1
            copia_act[i,7] <- 1
          }
        } else {
          if(!any(seleccion == act[6])){
            puntos[req2[4]] <- puntos[req2[4]] + req2[1]
            seleccion <- rbind(seleccion, act[6])
            puntos[act[4]] <- puntos[act[4]] + act[1]
            seleccion <- rbind(seleccion, i)
            
            copia_act[act[6],7] <- 1
            copia_act[i,7] <- 1
          } else{
            puntos[act[4]] <- puntos[act[4]] + act[1]
            seleccion <- rbind(seleccion, i)
            copia_act[i,7] <- 1
          }
        }
      } else{
        if(!any(seleccion == act[5])){
          puntos[req1[4]] <- puntos[req1[4]] + req1[1]
          seleccion <- rbind(seleccion, act[5])
          puntos[act[4]] <- puntos[act[4]] + act[1]
          seleccion <- rbind(seleccion, i)
          
          copia_act[act[5],7] <- 1
          copia_act[i,7] <- 1
        } else{
          puntos[act[4]] <- puntos[act[4]] + act[1]
          seleccion <- rbind(seleccion, i)
          copia_act[i,7] <- 1
        }
      }
    } else {
      puntos[act[4]] <- puntos[act[4]] + act[1]
      seleccion <- rbind(seleccion,i)
      copia_act[i,7] <- 1
    }
  }
  seleccion <- matrix(seleccion[-1,], ncol = 1)
  return(seleccion)
}
##----------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------##
tiempos <- function(i){
  act <- actividades[unlist(muestra[i]),]
  return(colSums(act[,1:2])[2])
}
##----------------------------------------------------------------------------------------##
isEmpty <- function(x) {
  return(identical(x, numeric(0)))
}

#cant_repeticiones <- c(1000,2000,3000,4000, 5000, 6000)
cant_repeticiones <- c(10000,20000,30000,40000, 100000, 500000)
for(r in cant_repeticiones){
  for(file in files){
    inicio <- Sys.time()
    if(cont == 75){
      cont <- 1
    } else {
      cont <- cont + 1
    }
    print(paste(r,cont))
    print(file)
  
    #datos <-((lee(paste0(ruta,"/problem1m-3t-4s-5a-0000U-1.pddl.txt"))))
    datos <-((lee(file)))
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
    requiere <- matrix(c(0,0,0),nrow=as.numeric(datos[4]),ncol = 3)
    if(as.numeric(datos[5]) != 0){ #Hay actividades con requisitos
      for(l in 1:datos[5]){#Debe ser hasta la cantidad de act con requisitos
        c <- unlist(as.list(strsplit(datos[9 + as.numeric(datos[3]) + as.numeric(datos[2]) + as.numeric(datos[1]) + 3 + l], " ")))
        if(as.integer(requiere[as.integer(c[1]),2]) == 0){
          requiere[as.integer(c[1]),]<-c(c[1],c[2],0)
        }
        else{
          requiere[as.integer(c[1]),]<-c(c[1],requiere[as.integer(c[1]),2], c[2])
        }
      }
    }
    
    ##------------------------------------  ACTIVIDADES -------------------------------------##
    actividades <- cbind(valor,duracion,recurso,part_sub[,2],requiere[,2], requiere[,3])
    actividades <- as.numeric(actividades)
    actividades <- matrix(actividades, nrow = as.numeric(datos[4]))
    colnames(actividades) <- c("valor","duracion","recurso","sub","requiere1","requiere2")
    
    
    ##----------------------------------- EXPERIMENTACION  ----------------------------------##
  
    ##////             PARALELO
    repeticiones <- r
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
    temporal <- data.frame(tiempo)
    if (length(temporal[temporal == tiempos_optimos[cont,],]) > 0) {
      optimo <- length(temporal[temporal == tiempos_optimos[cont,],]) 
    } else {
      optimo <- 0
    }
    cuantos_optimos <- rbind(cuantos_optimos, optimo)
    
    planes <- data.frame(table(unlist(muestra)))
    mejor <- data.frame(muestra[which.min(tiempo)])
    act1 <- actividades[mejor[,1],]
    promedio <- colSums(act1[,1:2])[1]/as.integer(datos[3])
    mejores_promedios <-rbind(mejores_promedios,promedio)
    mejores_tiempos <- rbind(mejores_tiempos,tiempo[which.min(tiempo)])
    print(mejor[,1])
    #------------  CUANTAS ACTIVIDADES SE NECESITAN PARA SATISFACER ---------------------
    tamaño <- length(mejor[,1])
    tamaños <- rbind(tamaños,tamaño)
    #------------------------------------------------------------------------------------
    mejores <- c(mejores ,data.frame(mejor[,1]))
    d <- rbind(d,planes)
    
    fin <- Sys.time()
    t_computo <- fin - inicio
    tiempo_computo <- rbind(tiempo_computo, t_computo)
    
    write(tiempo_computo[,1],paste0(ruta2,"/tiempos_computo.txt") , sep = '\n')
    write(mejores_tiempos[,1],paste0(ruta2,"/mejores_tiempos.txt"), sep = '\n')
    write(mejores_promedios[,1],paste0(ruta2,"/mejores_promedios.txt"), sep = '\n')
    write(tamaños[,1],paste0(ruta2,"/tamaños.txt"), sep = '\n')
    write(cuantos_optimos[,1],paste0(ruta2,"/cuantos_optimos.txt") , sep = '\n')
    capture.output(print(mejores), file = paste0(ruta2,"/mejores_planes.csv"))
  }
}
stopCluster(cluster)

