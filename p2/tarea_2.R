library(parallel)
library(functional)
dim <- 10
num <-  dim^2
datos <- data.frame()
exp <- 100
for(experimento in 1:exp){
  msj1 <- paste("Experimento: ", experimento)
  print(msj1)
  for(i in 1:10){
    prob <- .10 * i
    actual <- matrix((runif(num) < prob) * 1, nrow=dim, ncol=dim)
    #suppressMessages(library("sna"))
    #nom = paste(experimento, "_p2_t0.png")
    #png(nom)
    #plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
    #graphics.off()
    
    paso <- function(pos) {
      fila <- floor((pos - 1) / dim) + 1
      columna <- ((pos - 1) %% dim) + 1
      vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                          max(columna - 1, 1): min(columna + 1, dim)]
      return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
    }
    
    cluster <- makeCluster(detectCores() - 1)
    clusterExport(cluster, "dim")
    clusterExport(cluster, "paso")
    
    resultados <- data.frame()
    for (iteracion in 1:200) {
      clusterExport(cluster, "actual")
      siguiente <- parSapply(cluster, 1:num, paso)
      if(iteracion == 200){
        #msj <- paste("Iteracion: --  Probabilidad: ", prob)
        resultados <- rbind(resultados, 0)
        #print(msj)
        break;
      }
      if (sum(siguiente) == 0) { # todos murieron
        #msj <- paste("Iteracion: ",iteracion, " Probabilidad: ", prob)
        resultados <- rbind(resultados,iteracion)
        #print(msj)
        break;
      }
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
      #salida = paste(experimento,"_p2_t", iteracion, ".png", sep="")
      #tiempo = paste(experimento," Paso", iteracion)
      #png(salida)
      #plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
      #graphics.off()
    }
    stopCluster(cluster)
    resultados <- matrix(resultados)
    datos <- rbind(datos,resultados)
  }
}
datos <- matrix(datos$V1, nrow = exp, ncol = 10, byrow= TRUE)
print(datos)
write.csv(datos, 'resultados.csv')
