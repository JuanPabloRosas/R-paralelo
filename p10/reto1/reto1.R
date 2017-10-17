library(testit)
library(parallel)
s_obj <- data.frame()
for(s in 1:20){
  sol <- data.frame()
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
  
  reproduccion <- function(x, y, n) {
    pos <- sample(2:(n-1), 1)
    xy <- c(x[1:pos], y[(pos+1):n])
    yx <- c(y[1:pos], x[(pos+1):n])
    return(c(xy, yx))
  }
  
  n <- 50
  pesos <- generador.pesos(n, 15, 80)
  valores <- generador.valores(pesos, 10, 500)
  capacidad <- round(sum(pesos) * 0.65)
  optimo <- knapsack(capacidad, pesos, valores)
  s_obj <- rbind(s_obj, optimo)
  init <- 200
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  assert(tam == init)
  pm <- 0.05
  rep <- 50
  tmax <- 50
  mejores <- double()
  
  factible <- function(seleccion, pesos, capacidad) {
    return(sum(seleccion * pesos) <= capacidad)
  }
  
  factibilidad <- function(i){
    return(factible(p[i,1:n], pesos, capacidad))
  }
  
  objetivo <- function(seleccion, valores) {
    return(sum(seleccion * valores))
  }
  
  objet <- function(i){
    return(objetivo(p[i,1:n], valores))
  }
  
  mutacion <- function(sol, n) {
    pos <- sample(1:n, 1)
    mut <- sol
    mut[pos] <- (!sol[pos]) * 1
    return(mut)
  }
  
  muta <- function(i){
    if (runif(1) < pm) {
      return(mutacion(p[i,], n))
    }
  }
  
  peso2 <- function(seleccion, pesos, capacidad) {
    return(sum(seleccion * pesos))
  }
  
  peso1 <- function(i){
    return(peso2(p[i,1:n], pesos, capacidad))
  }
  
  reproduce <- function(){
    
  }
  
  #-----------------------------------------------------------------------------
  
  tam <- dim(p)[1]
  obj <- double()
  p$obj <- 1
  p$fact <- 0
  p$prob <- 1
  p$peso <- 0
  
  cluster <- makeCluster(detectCores() - 1)
  
  clusterExport(cluster, "p")
  clusterExport(cluster, "valores")
  clusterExport(cluster, "objetivo")
  clusterExport(cluster, "objet")
  clusterExport(cluster, "n")
  obj <- parSapply(cluster, 1:tam, objet)
  
  clusterExport(cluster, "p")
  clusterExport(cluster, "pesos")
  clusterExport(cluster, "capacidad")
  clusterExport(cluster, "factible")
  clusterExport(cluster, "factibilidad")
  fact <- parSapply(cluster, 1:tam, factibilidad)
  
  clusterExport(cluster, "p")
  clusterExport(cluster, "pesos")
  clusterExport(cluster, "capacidad")
  clusterExport(cluster, "peso1")
  clusterExport(cluster, "peso2")
  peso <- parSapply(cluster, 1:tam, peso1)
  
  stopCluster(cluster)
  
  p$obj <- obj
  p$fact <- fact
  suma <- sum(p$obj)
  p$prob <- p$obj/suma
  p$peso <- peso
  
  cluster <- makeCluster(detectCores() - 1)
  #           INICIA GENERACIONES
  for (iter in 1:tmax) {
    p$obj <- 1
    p$fact <- 0
    p$peso <- 0
    
    ##################################   MUTA   ###################################################    
    clusterExport(cluster, "pm")
    clusterExport(cluster, "n")
    clusterExport(cluster, "p")
    clusterExport(cluster, "mutacion")
    clusterExport(cluster, "muta")
    
    p_test <- parSapply(cluster, 1:tam,muta)
    p_test = p_test[-which(sapply(p_test, is.null))]
    p_test <- unlist(p_test)
    p_test <- matrix(p_test, ncol = n+4, byrow = TRUE)
    
    
    for(x in 1:nrow(p_test)){
      p <- rbind(p, p_test[x,]) 
    }
    ################################  REPRODUCE  ###################################################
    for (i in 1:rep) { # una cantidad fija de reproducciones
      tam <- dim(p)[1]
      padres <- sample(1:tam, 2, replace=FALSE ,prob = sort(p$prob, decreasing = FALSE))
      hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
      h1 <- unlist(hijos[1:(n+4)])
      h2 <- unlist(hijos[(n+5):((2*n)+5)])
      p <- rbind(p, h1)
      p <- rbind(p, h2) # segundo hijo
    }
    
    ###############################################################################################
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
    prob <- double()
    peso <- integer()
    
    clusterExport(cluster, "p")
    clusterExport(cluster, "valores")
    clusterExport(cluster, "objetivo")
    clusterExport(cluster, "objet")
    obj <- parSapply(cluster, 1:tam, objet)
    p$obj <- obj
    
    clusterExport(cluster, "pesos")
    clusterExport(cluster, "capacidad")
    clusterExport(cluster, "factible")
    clusterExport(cluster, "factibilidad")
    fact <- parSapply(cluster, 1:tam, factibilidad)
    p$fact <- fact
    
    clusterExport(cluster, "peso1")
    clusterExport(cluster, "peso2")
    peso <- parSapply(cluster, 1:tam, peso1)
    p$peso <- peso
    
    suma <- sum(p$obj)
    p$prob <- p$obj/suma
    
    ###############################################################################################    
    
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    #no_factibles <- p[p$fact == FALSE,]
    
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
    print(paste(iter,mejor))
    
    sol <- rbind(sol,mejor)
    
  }
  nombre <- paste0("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/reto1/",s,"_sol_reto1.csv")
  write.table(sol, nombre, sep=",") 
  write.table(s_obj, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/reto1/opt_reto1.csv", sep=",") 
  stopCluster(cluster)
}





