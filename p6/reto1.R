library(parallel)
l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
vacunados <- c(.05,.1,.15,.2,.25,.3,.35,.4)
datos_iteracion <- data.frame()
datos_epidemia <- data.frame()

for(v in vacunados){
	print(v)
	pv <- v
	agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())

	epidemia <- integer()
	sanos <- integer()
	rec <- integer()
	r <- 0.1
	tmax <- 1000
	digitos <- floor(log(tmax, 10)) + 1

	#------------------------------Iniciar vacunados---------------------------------
	for (i in 1:n) {
	  e <- "S"
	  if (runif(1) < pi) {
	    e <- "I"
	  } else if(runif(1) < pv){
	    e <- "R"
		}
	  agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
		                               dx = runif(1, -v, v), dy = runif(1, -v, v),
		                               estado = e))
	  levels(agentes$estado) <- c("S", "I", "R")
	}

	contagia <- function(i){
		a1 <- agentes[i, ]
		cont <- "S"
		if (a1$estado == "S") { # desde los infectados
			for (j in 1:n) {
				a2 <- agentes[j, ]
				if (a2$estado == "I") { # hacia los susceptibles
					dx <- a1$x - a2$x
					dy <- a1$y - a2$y
					d <- sqrt(dx^2 + dy^2)
					if (d < r) { # umbral
						p <- (r - d) / r
						if (runif(1) < p) {
							cont <- "I"
							break
						}
					}
				}
			}
		}
		else if (a1$estado == "I") { # ya estaba infectado
	    		if (runif(1) < pr) {
	      			cont <- "R" # recupera
	    		}
			else{
				cont <- "I"
			}
		}
		else if(a1$estado == "R"){
			cont <- "R"
		}
		return(cont)
	}

	mueve <- function(i){
	  a <<- agentes[i, ]
	  a$x <- a$x + a$dx
	  a$y <- a$y + a$dy
	  if (a$x > l) {
	    a$x <- a$x - l
	  }
	  if (a$y > l) {
	    a$y <<- a$y - l
	  }
	  if (a$x < 0) {
	    a$x <- a$x + l
	  }
	  if (a$y < 0) {
	    a$y <- a$y + l
	  }
	  agentes[i, ] <<- a
	}

		  cluster <- makeCluster(detectCores() - 1)
	#iteraciones <- function(agentes,pr,l,n,pi,tmax,r,epidema){
		for (tiempo in 1:tmax) {
		  infectados <- dim(agentes[agentes$estado == "I",])[1]
		  no_infectados <- dim(agentes[agentes$estado == "S",])[1]
		  recuperados <- dim(agentes[agentes$estado == "R",])[1]
		  epidemia <- c(epidemia, infectados)
		  sanos <- c(sanos, no_infectados)
		  rec <- c(rec, recuperados)
		  if (no_infectados == 0) {
		    break
		  }
		#----------------------------------------------------  

		  clusterExport(cluster, "agentes")
		  clusterExport(cluster, "r")
		  clusterExport(cluster, "n")
		  clusterExport(cluster, "pr")
		  contagiados <- parSapply(cluster, 1:n, contagia)

		  agentes$estado <- contagiados
		#----------------------------------------------------

		  #cluster <- makeCluster(detectCores() - 1)
		  #clusterExport(cluster, "agentes")
		  #clusterExport(cluster, "l")
		  #clusterExport(cluster, "mueve")
		  #parSapply(cluster, 1:n,mueve)
		  #stopCluster(cluster)
		    for (i in 1:n) { # movimientos
		  
			a <- agentes[i, ]
			a$x <- a$x + a$dx
			a$y <- a$y + a$dy
			if (a$x > l) {
			    a$x <- a$x - l
			}
			if (a$y > l) {
			    a$y <- a$y - l
			}
			if (a$x < 0) {
			    a$x <- a$x + l
			}
			if (a$y < 0) {
			    a$y <- a$y + l
			}
			agentes[i, ] <- a
		    }

		#----------------------------------------------------  
		#  aS <- agentes[agentes$estado == "S",]
		#  aI <- agentes[agentes$estado == "I",]
		#  aR <- agentes[agentes$estado == "R",]
		#  tl <- paste(tiempo, "", sep="")
		#  while (nchar(tl) < digitos) {
		#    tl <- paste("0", tl, sep="")
		#  }
		#  salida <- paste("p6_t", tl, ".png", sep="")
		#  tiempo <- paste("Paso", tiempo)
		#  png(salida)
		#  plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
		#  if (dim(aS)[1] > 0) {
		#    points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
		#  }
		#  if (dim(aI)[1] > 0) {
		#    points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
		#  }
		#  if (dim(aR)[1] > 0) {
		#    points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
		#  }
		#  graphics.off()
		}
	#}
		  stopCluster(cluster)
	#t <- system.time(iteraciones(agentes,pr,n,l,pi,tmax,r,epidemia))
	#print(t)

	datos_epidemia <- rbind(datos_epidemia, length(epidemia))
	datos_iteracion <- rbind(datos_iteracion,tiempo)
	png(paste0(v,"_p6e.png"), width=600, height=300)
	plot(1:length(epidemia),100 * sanos / n, xlab="Tiempo", ylab="Porcentaje de infectados",col="green", type="l")
		    lines(1:length(epidemia), 100 * epidemia / n, col="red")
		    lines(1:length(rec), 100 * rec / n, col ="yellow")
	graphics.off()
}
write.table(datos_epidemia,"d_epidemia.csv", sep= ",")
write.table(datos_iteracion,"d_tiempo.csv", sep= ",")
