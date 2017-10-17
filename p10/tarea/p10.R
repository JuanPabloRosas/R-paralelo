library(testit)
library(parallel)

cluster <- makeCluster(detectCores() - 1)
for(z in 1:5){
  #i <- 1
  tiempos <- data.frame()
  pm <- 0.05
  rep <- 50
  tmax <- 50
  n <- 50
  init <- 50 * z
  mejores <- double()
  
  print(init)
  
  knapsack <- function(cap, peso, valor) {
    n <- length(peso)
    pt <- sum(peso) 
    assert(n == length(valor))
    vt <- sum(valor) 
    if (pt < cap) { 
      return(vt)
    } else {
      filas <- cap + 1 
      cols <- n + 1 
      tabla <- matrix(rep(-Inf, filas * cols),
                      nrow = filas, ncol = cols) 
      for (fila in 1:filas) {
        tabla[fila, 1] <- 0 
      }
      rownames(tabla) <- 0:cap 
      colnames(tabla) <- c(0, valor) 
      for (objeto in 1:n) { 
        for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
          anterior <- acum - peso[objeto]
          if (anterior > 0) { # si conocemos una combinacion con ese peso
            tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
          }
        }
      }
      return(max(tabla))
    }
  }
  
  factible <- function(seleccion, pesos, capacidad) {
    return(sum(seleccion * pesos) <= capacidad)
  }
  
  objetivo <- function(seleccion, valores) {
    return(sum(seleccion * valores))
  }
  
  normalizar <- function(data) {
    menor <- min(data)
    mayor <- max(data)
    rango <- mayor - menor
    data <- data - menor # > 0
    return(data / rango) # entre 0 y 1
  }
  
  generador.pesos <- function(cuantos, min, max) {
    return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
  }
  
  generador.valores <- function(pesos, min, max) {
    n <- length(pesos)
    valores <- double()
    for (i in 1:n) {
      media <- pesos[n]
      desv <- runif(1)
      valores <- c(valores, rnorm(1, media, desv))
    }
    valores <- normalizar(valores) * (max - min) + min
    return(valores)
  }
  
  poblacion.inicial <- function(n, tam) {
    pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
    for (i in 1:tam) {
      pobl[i,] <- round(runif(n))
    }
    return(as.data.frame(pobl))
  }
  
  mutacion <- function(sol, n) {
    pos <- sample(1:n, 1)
    mut <- sol
    mut[pos] <- (!sol[pos]) * 1
    return(mut)
  }
  
  reproduccion <- function(x, y, n) {
    pos <- sample(2:(n-1), 1)
    xy <- c(x[1:pos], y[(pos+1):n])
    yx <- c(y[1:pos], x[(pos+1):n])
    return(c(xy, yx))
  }
  
  muta <- function(i){
    if (runif(1) < pm) {
      return(mutacion(p[i,], n))
    }
  }
  
  factibilidad <- function(i){
    return(factible(p[i,], pesos, capacidad))
  }
  
  objet <- function(i){
    return(objetivo(p[i,], valores))
  }
  
  pesos <- generador.pesos(n, 15, 80)
  valores <- generador.valores(pesos, 10, 500)
  capacidad <- round(sum(pesos) * 0.65)
  optimo <- knapsack(capacidad, pesos, valores)
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  assert(tam == init)
  
  ##################################  INICIA GENERACIONES ######################################
  
  #cluster <- makeCluster(detectCores() - 1)
  
  for (iter in 1:tmax) {
    tiempo <- proc.time()
    p$obj <- NULL
    p$fact <- NULL
    
    ##################################   MUTA   ###################################################    
    
    clusterExport(cluster, "pm")
    clusterExport(cluster, "n")
    clusterExport(cluster, "p")
    clusterExport(cluster, "mutacion")
    clusterExport(cluster, "muta")
    
    p_test <- parSapply(cluster, 1:tam,muta)
    p_test = p_test[-which(sapply(p_test, is.null))]
    if(!is.null(nrow(p_test))){
      p_test <- unlist(p_test)
      p_test <- matrix(p_test, ncol = n, byrow = TRUE)
      
      for(x in 1:(length(p_test)/n)){
        p <- rbind(p, p_test[x,]) 
      }
    }
    
    ################################## REPRODUCE  #################################################
    for (i in 1:rep) { # una cantidad fija de reproducciones
      tam <- dim(p)[1]
      padres <- sample(1:tam, 2, replace=FALSE)
      hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
      p <- rbind(p, hijos[1:n]) # primer hijo
      p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
    }
    ################################## FACTIBLE ####################################################
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
    
    clusterExport(cluster, "p")
    clusterExport(cluster, "valores")
    clusterExport(cluster, "objetivo")
    clusterExport(cluster, "objet")
    obj <- parSapply(cluster, 1:tam, objet)
    
    clusterExport(cluster, "p")
    clusterExport(cluster, "pesos")
    clusterExport(cluster, "capacidad")
    clusterExport(cluster, "factible")
    clusterExport(cluster, "factibilidad")
    fact <- parSapply(cluster, 1:tam, factibilidad)
    
    
    ###############################################################################################    
    p <- cbind(p, obj)
    p <- cbind(p, fact)
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
    
    tiempo  <- proc.time()- tiempo
    tiempos <- rbind(tiempos,tiempo[3])
  }
  
  nombre <- paste0(init,"_tiempos_tarea2.csv")
  write.table(tiempos, nombre, sep=",")
}
stopCluster(cluster)
