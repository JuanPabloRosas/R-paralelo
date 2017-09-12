datos_r    <- data.frame()
datos_t    <- data.frame()
tam_muestra <- 10
repeticiones <- 100
ruta <-"/home/pabloide/Documentos/3 Semestre/R_paralelo/p5/"
for (r in 1:tam_muestra){
	pedazo <- 50000 * r
	print(paste("Muestra:",pedazo))
	resultados <- data.frame()
	tiempos    <- data.frame()
	for(j in 1:repeticiones){
		inicio <- -6
		final <- -inicio
		paso <- 0.25
		x <- seq(inicio, final, paso)
		f <- function(x) { return(1 / (exp(x) + exp(-x))) }
		#png("p5f.png") # dibujamos f(x) para ver como es
		#plot(x,  (2/pi) * (1/(exp(x)+exp(-x))))
		#lines(x,  (2/pi) * (1/(exp(x)+exp(-x))), type="l")
		#graphics.off()
		suppressMessages(library(distr))
		g <- function(x) { return((2 / pi) * f(x)) }
		generador  <- r(AbscontDistribution(d = g)) # creamos un generador
		muestra <- generador(50000) # sacamos una muestra
		#png(paste0(j,"_p5m.png")) # validamos con un dibujo
		#hist(muestra, freq=F, breaks=50,
		#     main="Histograma de g(x) comparado con g(x)",
		#     xlim=c(inicio, final), ylim=c(0, 0.4))
		#lines(x, g(x), col="red") # dibujamos g(x) encima del histograma
		#graphics.off()
		desde <- 3
		hasta <- 7
		cuantos <- 200 
		parte <- function(s) {
		    valores <- generador(pedazo)
		    #png(paste0(j,"_muestra.png"))
		    #hist(valores)
		    #graphics.off()
		    return(sum(valores >= desde & valores <= hasta))
		}


#		suppressMessages(library(doParallel))
#		registerDoParallel(makeCluster(detectCores() - 1))
#		t <- system.time(montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte())
#		stopImplicitCluster()

		suppressMessages(library(parallel))
		cluster <- makeCluster(detectCores() - 1)
		clusterExport(cluster, "parte")
		clusterExport(cluster, "generador")
		clusterExport(cluster, "pedazo")
		clusterExport(cluster, "desde")
		clusterExport(cluster, "hasta")
		t <- system.time(montecarlo <- parSapply(cluster, 1:cuantos, parte))
		stopCluster(cluster)
		
		integral <- sum(montecarlo) / (cuantos * pedazo)
		i <- (pi / 2) * integral
		print(paste(j, "T: ",t[3], "V: ", i))
		resultados <- rbind(resultados, i)
		tiempos <- rbind(tiempos ,t[3])
	}
	colnames(resultados)<-c("res")
	colnames(tiempos)<-c("tiempo")
	write.table(resultados,paste0(ruta,"Resultados/",r,"_resultados.csv"), sep= ",")
	write.table(tiempos,paste0(ruta,"Tiempos/",r,"_tiempos.csv"), sep= ",")
	
	x_range <-seq(1,repeticiones,1)

	png(paste0(ruta,"Resultados/",r,"_Resultados.png"))
	plot(x_range, resultados$res, type="o", ylab="Valor", xlab="Repeticiones", main=paste("Resultados",r), ylim = c(0.0486,0.0489))
	abline(h=0.048834, col="Red")
	graphics.off()
	png(paste0(ruta,"Tiempos/",r,"_Tiempos.png"))
	plot(x_range, tiempos$tiempo, type="o", ylab="Tiempo", xlab="Repeticiones", main=paste("Tiempos",r) ,ylim = c(0,100))
	graphics.off()

	datos_r <- rbind(datos_r,resultados$res)
	datos_t <- rbind(datos_t,tiempos$tiempo)
}

#datos_r <- matrix(datos_r, ncol = tam_muestra, nrow =repeticiones)
#datos_t <- matrix(datos_t, ncol = tam_muestra, nrow =repeticiones)

write.table(datos_r,"d_resultados.csv", sep= ",")
write.table(datos_t,"d_tiempos.csv", sep= ",")

colnames(datos_r)<-c(seq(5000,(5000 * tam_muestra),5000))
colnames(datos_t)<-c(seq(5000,(5000 * tam_muestra),5000))

png(paste0(ruta,"Resultados/","d_resultados.png"))
boxplot(datos_r,ylab="Valor", xlab="Tamaño de la muestra", main="Resultados")
abline(h=0.048834, col="Red")
graphics.off()

png(paste0(ruta,"Tiempos/","d_tiempos.png"))
boxplot(datos_t,ylab="Tiempo(s)", xlab="Tamaño de la muestra", main="Tiempos")
graphics.off()

