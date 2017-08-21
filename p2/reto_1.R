library(parallel)
dim <- 30
num <-  dim^2

#prob <- .01
#actual <- matrix((runif(num) < prob) * 1, nrow=dim, ncol=dim)
#suppressMessages(library("sna"))
#png("p2_t0.png")
#plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
#graphics.off()

nucleos <- 20
actual <- matrix(rep(0,num), nrow=dim, ncol=dim, )
al <- sample(1:num,nucleos)
al2 <- sample(1:nucleos)
for(y in 1:nucleos){
  actual[al[y]]=al2[y]
}
suppressMessages(library("sna"))
png("p2_t0.png")
image(actual, col =c(0,rainbow(20)))
#plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
graphics.off()

paso <- function(pos){
	fila <- floor((pos - 1) / dim) + 1
	columna <- ((pos - 1) %% dim) + 1
	if(actual[fila,columna] == 0){	
		vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
		                max(columna - 1, 1): min(columna + 1, dim)]

		if((sum(vecindad) - actual[fila, columna]) >= 1)
		{
			v <- vecindad > 0
	    		vecinos <- vecindad[v]
			vecino <- max(vecinos)
			return(vecino)
		}
		else
		{
			return(actual[fila,columna])
		}
	}
	else {
		return(actual[fila,columna])
	}
}


cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
 
for (iteracion in 1:20) {
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)
    siguiente[!is.finite(siguiente)] <- 0
    msj <- paste("Iteracion:", iteracion)
    print(msj)
    if (sum(siguiente) == num) { # todos murieron
        print("Ya no hay espacio.")
        break;
    }
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    salida = paste("p2_t", iteracion, ".png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    image(actual, col =c(0,rainbow(20)))
    #plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
}
stopCluster(cluster)
print(actual)
