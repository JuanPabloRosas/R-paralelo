library(parallel)
muestras <- 20
num <- 300
resultados <- numeric()
tiempos <- data.frame()

ruta <-"/home/pabloide/Documentos/3 Semestre/R_paralelo/p5/Reto1/"
for(m in 1:muestras){
	print(m)
	runs <- 100000 * m
	valor <- function(x){
	  xs <- runif(runs,min=-0.5,max=0.5)
	  ys <- runif(runs,min=-0.5,max=0.5)
	  in.circle <- xs^2 + ys^2 <= 0.5^2
	  return((sum(in.circle)/runs)*4)
	}

	  cluster <- makeCluster(detectCores() - 1)
	  clusterExport(cluster, "runs")
	  clusterExport(cluster, "valor")
	  
	  t <- system.time(aprox <- parSapply(cluster, FUN = valor, 1:num))
	  stopCluster(cluster)
	
	tiempos <- rbind(tiempos,t[3])
	if(m == 1){
		resultados <- c(resultados,aprox)
		}
	else{
		resultados <- cbind(resultados,aprox)
	}

	#PLOT RESULTADOS
	png(paste0(ruta,"Resultados/",m,"_reto1.png"))
	z <- 1:num
	plot(z, aprox, type="o", ylim=c(3.12,3.16), main = m)
	legend("topright", "3.141592", pch = "--", title = "Valor de π", col ="red")
	abline(h=3.141592, col="Red")
	graphics.off()

}

colnames(resultados) <- c(seq(100000, (100000 * muestras),100000))
write.table(resultados, paste0(ruta,"pi.csv") , sep= ",")
png(paste0(ruta,"Resultados/","d_resultados.png"))
boxplot(resultados , ylab="Valor", xlab="Tamaño de la muestra", main="Resultados")
abline(h=3.141592, col="Red")
graphics.off()

colnames(tiempos) <- c("dur")
x <- c(seq(100000, (100000 * muestras),100000))
write.table(tiempos, paste0(ruta,"tiempos_pi.csv") , sep= ",")
png(paste0(ruta,"Tiempos/","d_tiempos.png"))
plot(x,tiempos$dur ,type="o", ylab="Valor", xlab="Tamaño de la muestra", main="Tiempos")
graphics.off()

