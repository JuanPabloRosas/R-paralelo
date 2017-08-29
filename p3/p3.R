suppressMessages(library(doParallel))
resultados <- matrix()
resultados1 <- matrix()
resultados2 <- matrix()
resultados3 <- matrix()
resultados4 <- matrix()
resultados5 <- matrix()
resultados6 <- matrix()

  primo <- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if (n!= i &&(n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}
 
desde <- 1
hasta <-  10000

original <- desde:hasta
invertido <- hasta:desde
pares <- seq(2, hasta*2, by=2)
impares <- data.frame()
#Impares------------
for(y in 1:10000)
{
	impares <- rbind(impares,(2*y + 1))
}
#--------------------
serie <- 50000:60000

for (h in range(1,hasta*2))

replicas <- 3
nucleos <- detectCores() - 1

for(n in 1:nucleos){
	datos <- data.frame()
	datos1 <- data.frame()
	datos2 <- data.frame()
	datos3 <- data.frame()
	datos4 <- data.frame()
	datos5 <- data.frame()
	datos6 <- data.frame()

	registerDoParallel(makeCluster(n))
	ot <-  numeric()
	it <-  numeric()
	at <-  numeric()
	par <-  numeric()
	impar <-  numeric()
	se <-  numeric()

	print(paste("Tiempos con", n, "nucleos"))
	for (r in 1:replicas) {
	    print(paste("replica", r))
	    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
	    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
	    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # aleatorio
	    par <- c(par, system.time(foreach(n = sample(pares), .combine=c) %dopar% primo(n))[3]) # pares
	    impar <- c(impar, system.time(foreach(n = sample(impares), .combine=c) %dopar% primo(n))[3]) # impares
	    se <- c(se, system.time(foreach(n = sample(serie), .combine=c) %dopar% primo(n))[3]) # serie
	    
	    datos1 <- rbind(datos2, ot[length(ot)-1])
	    datos2 <- rbind(datos2, it[length(it)-1])
	    datos3 <- rbind(datos3, at[length(at)-1])
	    datos4 <- rbind(datos4, par[length(par)-1])
	    datos5 <- rbind(datos5, impar[length(impar)-1])
	    datos6 <- rbind(datos6, se[length(se)-1])

	    datos <- rbind(datos,ot)
	    datos <- rbind(datos,it)
	    datos <- rbind(datos,at)
	    datos <- rbind(datos,par)
	    datos <- rbind(datos,impar)
	    datos <- rbind(datos,se)
	}
	resultados <- data.frame(cbind(resultados,datos))

	resultados1 <- data.frame(cbind(resultados1,datos1))
	resultados2 <- data.frame(cbind(resultados2,datos2))
	resultados3 <- data.frame(cbind(resultados3,datos3))
	resultados4 <- data.frame(cbind(resultados4,datos4))
	resultados5 <- data.frame(cbind(resultados5,datos5))
	resultados6 <- data.frame(cbind(resultados6,datos6))
}
resultados <- resultados[,-1]
resultados1 <- resultados1[,-1]
resultados2 <- resultados2[,-1]
resultados3 <- resultados3[,-1]
resultados4 <- resultados4[,-1]
resultados5 <- resultados5[,-1]
resultados6 <- resultados6[,-1]

colnames(resultados) <- c(1:nucleos)
colnames(resultados1) <- c(1:nucleos)
colnames(resultados2) <- c(1:nucleos)
colnames(resultados3) <- c(1:nucleos)
colnames(resultados4) <- c(1:nucleos)
colnames(resultados5) <- c(1:nucleos)
colnames(resultados6) <- c(1:nucleos)

write.table(resultados, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p3/resultados.csv", sep=",") 
write.table(resultados1, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p3/resultados1.csv", sep=",") 
write.table(resultados2, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p3/resultados2.csv", sep=",") 
write.table(resultados3, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p3/resultados3.csv", sep=",") 
write.table(resultados4, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p3/resultados4.csv", sep=",") 
write.table(resultados5, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p3/resultados5.csv", sep=",") 
write.table(resultados6, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p3/resultados6.csv", sep=",") 

print("Todos")
print(resultados)
print("Ascendente")
print(resultados1)
print("Descendente")
print(resultados2)
print("Aleatorio")
print(resultados3)
print("Pares")
print(resultados4)
print("Impares")
print(resultados5)
print("Serie")
print(resultados6)

png("resultados.png")
boxplot(resultados, use.cols =FALSE, main="Todo", xlab= "Cantidad de Nucleos", ylab = "Tiempo(s)")

png("resultados1.png")
boxplot(resultados1, use.cols =FALSE, main="Orden Ascendente", xlab= "Cantidad de Nucleos", ylab = "Tiempo(s)")

png("resultados2.png")
boxplot(resultados2, use.cols =FALSE, main="Orden Descendente", xlab= "Cantidad de Nucleos", ylab = "Tiempo(s)")

png("resultados3.png")
boxplot(resultados3, use.cols =FALSE, main="Aleatorio", xlab= "Cantidad de Nucleos", ylab = "Tiempo(s)")

png("resultados4.png")
boxplot(resultados4, use.cols =FALSE, main="Pares", xlab= "Cantidad de Nucleos", ylab = "Tiempo(s)")

png("resultados5.png")
boxplot(resultados5, use.cols =FALSE, main="Impares", xlab= "Cantidad de Nucleos", ylab = "Tiempo(s)")

png("resultados6.png")
boxplot(resultados6, use.cols =FALSE, main="Numeros del 50000-60000", xlab= "Cantidad de Nucleos", ylab = "Tiempo(s)")
graphics.off()
