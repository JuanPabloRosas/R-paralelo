tiempos <- data.frame()
library(parallel)
cluster <- makeCluster(detectCores() - 1)
for(x in 0:7){
  print(x)
  for(y in 1:50){
  tiempo <- proc.time()
binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

prueba <- function(j){
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
  }
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
  return(contadores)
}

modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.995
modelos[modelos=='g'] <- 0.92
modelos[modelos=='b'] <- 0.002

r <- 5
c <- 3
dim <- r * c

tasa <- 0.15
tranqui <- 0.99

tope <- 9
digitos <- 0:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

n <- floor(log(k-1, 2)) + 1 #Cantidad de bits para el numero
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

#--------------------------------------  ENTRENA  -----------------------------------------
for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}

#------------------------------------------  PRUEBA  --------------------------------------

cuantos <- 300 + (x * 100)

clusterExport(cluster, "prueba")
clusterExport(cluster, "n")
clusterExport(cluster, "neuronas")
clusterExport(cluster, "dim")
clusterExport(cluster, "modelos")
clusterExport(cluster, "tope")
clusterExport(cluster, "binario")
clusterExport(cluster, "decimal")
clusterExport(cluster, "k")
clusterExport(cluster, "contadores")

 contadores <- parSapply(cluster, 1:cuantos, prueba)
 contadores <- matrix(rowSums(contadores), nrow = 10)
 colnames(contadores) <- c(0:tope,"NA")
 rownames(contadores) <- c(0:tope)
 
 #print(contadores) 
 tiempo <- proc.time() - tiempo
 tiempos <- rbind(tiempos, tiempo)
  }
}
stopCluster(cluster)
write.csv(tiempos,"/home/pabloide/Documentos/3 Semestre/R_paralelo/p12/tiempos.csv", sep = ",")