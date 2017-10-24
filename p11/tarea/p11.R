library(parallel)
tiempos <- data.frame()
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

evalua <- function(i){
  val <- matrix(rep(NA, k), ncol=k)
  for (j in 1:k) { # para todos los objetivos
    val[, j] <- eval(obj[[j]], sol[i,], tc)
  }
  return(val)
}


cluster <- makeCluster(detectCores() - 1)
frentes <- data.frame()
for(u in 1:5){
  for(r in 1:50){
    tiempo <- proc.time()
    vc <- 4
    md <- 3
    tc <- 5
    k <- 2 # cuantas funciones objetivo
    obj <- list()
    for (i in 1:k) {
      obj[[i]] <- poli(md, vc, tc)
    }
    minim <- (runif(k) > 0.5)
    sign <- (1 + -2 * minim)
    n <- 200 * u # cuantas soluciones aleatorias
    sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
    val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
    
    ###########################  EVALUA  ################################################
    
    clusterExport(cluster, "evalua")
    clusterExport(cluster, "eval")
    clusterExport(cluster, "obj")
    clusterExport(cluster, "sol")
    clusterExport(cluster, "tc")
    clusterExport(cluster, "k")
    clusterExport(cluster, "n")
    
    val <- parSapply(cluster, 1:n, evalua)
    val <- t(val)
    
    ######################################################################################
    
    mejor1 <- which.max(sign[1] * val[,1])
    mejor2 <- which.max(sign[2] * val[,2])
    cual <- c("max", "min")
    xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
    yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
    
    domin.by <- function(target, challenger, total) {
      if (sum(challenger < target) > 0) {
        return(FALSE) # hay empeora
      } # si no hay empeora, vemos si hay mejora
      return(sum(challenger > target) > 0)
    }
    
    dominado <- function(i){
      d <- logical()
      for (j in 1:n) {
        d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
      }
      return(d)
    }
    
    #################################  DOMINADAS  ##################################
    no.dom <- logical()
    dominadores <- integer()
    
    clusterExport(cluster, "dominado")
    clusterExport(cluster, "domin.by")
    clusterExport(cluster, "val")
    clusterExport(cluster, "sign")
    clusterExport(cluster, "k")
    clusterExport(cluster, "n")
    
    d <- parSapply(cluster, 1:n, dominado)
    
    for(x in 1:n){
        cuantos <- sum(d[,x])
        dominadores <- c(dominadores, cuantos)
        no.dom <- c(no.dom, cuantos == 0) # nadie le domina
      }
      
      ######################################################################################
      frente <- subset(val, no.dom) # solamente las no dominadas
      seleccionados <- function(m,frente){
        d <- data.frame()
        for(n in 1:nrow(frente)){
          minimo <- m * 2
          distancias <- sqrt((val[mejor1, 1] - frente[n, 1])**2 + (val[mejor1, 2] - frente[n, 2])**2)
          temp <- abs(distancias - m)
          d <- rbind(d,temp)
        }
        return(d)
      }
      
      seleccionados2 <- function(m,frente){
        d <- data.frame()
        for(n in 1:nrow(frente)){
          minimo <- m * 2
          distancias <- sqrt((val[mejor2, 1] - frente[n, 1])**2 + (val[mejor2, 2] - frente[n, 2])**2)
          temp <- abs(distancias - m)
          d <- rbind(d,temp)
        }
        return(d)
      }
      
      mitad <- sqrt((val[mejor1, 1] - val[mejor2, 1])**2 + (val[mejor1, 2] - val[mejor2, 2])**2)/2
      dist <- seleccionados(mitad,frente)
      centro <- which.min(dist[,1])
      
      mitad2 <- sqrt((val[mejor1, 1] - frente[centro, 1])**2 + (val[mejor1, 2] - frente[centro, 2])**2)/2
      dist2 <- seleccionados(mitad2,frente)
      centro_1 <- which.min(dist2[,1])
      
      mitad3 <- sqrt((val[mejor2, 1] - frente[centro, 1])**2 + (val[mejor2, 2] - frente[centro, 2])**2)/2
      dist3 <- seleccionados2(mitad3,frente)
      centro_2 <- which.min(dist3[,1])
      
      tiempo  <- proc.time()- tiempo
      tiempos <- rbind(tiempos,tiempo[3])
  }
}
stopCluster(cluster)
write.csv(tiempos, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p11/tarea_y_reto1/tiempos_paralelo.csv") 