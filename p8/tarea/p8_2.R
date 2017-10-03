library(testit)
library(parallel)
library(doParallel)
tiempos <- data.frame()
k <- 120000
n <- 12000000
originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
assert(min(cumulos) > 0)
diferencia <- n - sum(cumulos)
doP <- TRUE

if (diferencia > 0) {
  for (i in 1:diferencia) {
    p <- sample(1:k, 1)
    cumulos[p] <- cumulos[p] + 1
  }
} else if (diferencia < 0) {
  for (i in 1:-diferencia) {
    p <- sample(1:k, 1)
    if (cumulos[p] > 1) {
      cumulos[p] <- cumulos[p] - 1
    }
  }
}

#	VALIDACIONES
assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
assert(sum(cumulos) == n)

c <- median(cumulos) # tamanio critico de cumulos
d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva

#--------------------------------------------------------------------
rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}

romperse <- function(tam, cuantos) {
  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  library(testit)
  assert(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}

rompe <- function(i){
  urna <- freq[i,]
  if (urna$tam > 1) { # no tiene caso romper si no se puede
    return(romperse(urna$tam, urna$num))
  } else {
    return(rep(1, urna$num))
  }
}
#--------------------------------------------------------------------
union <- function(x) {
  return (exp(-x / c))
}

unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}

une <- function(i){
  urna <- freq[i,]
  return(unirse(urna$tam, urna$num))
}
#--------------------------------------------------------------------

freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
duracion <- 100
digitos <- floor(log(duracion, 10)) + 1

if(doP){
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster, "c")
  clusterExport(cluster, "d")
} else{
  suppressMessages(library(doParallel))
  registerDoParallel(makeCluster(detectCores() - 1))
}

####################################################################################################################
for (paso in 1:duracion) {
print(paso)
    tiempo <- proc.time()
  assert(sum(cumulos) == n)
  
  #------------------------------FASE ROTURA---------------------------------------------
  cumulos <- integer()
  ############################   PARALELO  ###############################################
  if(doP){
    clusterExport(cluster, "rompe")
    clusterExport(cluster, "romperse")
    clusterExport(cluster, "rotura")
    clusterExport(cluster, "freq")
    cumulos <- parSapply(cluster, 1:dim(freq)[1],rompe)
    cumulos <- unlist(cumulos)
  } else{
    cumulos <- foreach(i = 1:dim(freq)[1], .combine=c) %dopar% romperse(freq,i)
  }
  ########################################################################################
  
  #	VALIDACIONES
  assert(sum(cumulos) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  freq <- as.data.frame(table(cumulos)) # actualizar urnas
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  
  #-----------------------------FASE UNION-----------------------------------------------
  cumulos <- integer()
  ############################   PARALELO  ###############################################
  if(doP){
    clusterExport(cluster, "une")
    clusterExport(cluster, "unirse")
    clusterExport(cluster, "union")
    clusterExport(cluster, "freq")
    cumulos <- parSapply(cluster, 1:dim(freq)[1], une)
    cumulos <- unlist(cumulos)
  } else{
  cumulos <- foreach(i = 1:dim(freq)[1], .combine=c) %dopar% unirse(freq,i)
  }
  ########################################################################################
  
  #	VALIDACIONES
  assert(sum(abs(cumulos)) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  juntarse <- -cumulos[cumulos < 0]
  cumulos <- cumulos[cumulos > 0]
  assert(sum(cumulos) + sum(juntarse) == n)
  nt <- length(juntarse)
  if (nt > 0) {
    if (nt > 1) {
      juntarse <- sample(juntarse)
      for (i in 1:floor(nt / 2) ) {
        cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
      }
    }
    if (nt %% 2 == 1) {
      cumulos <- c(cumulos, juntarse[nt])
    }
  }
  
  #	VALIDACIONES
  assert(sum(cumulos) == n)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  
  #------------------------------------DIBUJA-------------------------------------- 
  #tl <- paste(paso, "", sep="")
  #while (nchar(tl) < digitos) {
  #  tl <- paste("0", tl, sep="")
  #}
  #png(paste("p8r_ct", tl, ".png", sep=""), width=300, height=300)
  #tope <- 50 * ceiling(max(cumulos) / 50)
  #hist(cumulos, breaks=seq(0, tope, 50), 
  #     main=paste("Paso", paso, "con ambos fen\u{00f3}menos"), freq=FALSE,
  #     ylim=c(0, 0.05), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa")
  #graphics.off()
      tiempo  <- proc.time()- tiempo
      tiempos <- rbind(tiempos,tiempo[3])
}
write.table(tiempos, "tiempos_tarea.csv", sep="\t") 
if(doP){
  stopCluster(cluster)
} else{
  stopImplicitCluster()
}

