resultados = read.csv("resultados_pi.csv", sep=",", header=TRUE)
tiempos = read.csv("tiempos_pi.csv", sep=",", header=TRUE)
tiempos <- as.data.frame(t(tiempos))

colnames(resultados)<-c(1:20)
colnames(tiempos)<-c(1:20)

png("Resultados.png")
boxplot(resultados,ylab="Valor", xlab="Tamaño de la muestra", main="Resultados", ylim =c(3.12,3.16))
abline(h=3.141592, col="red")
legend("topright", "3.141592", pch = "--", title = "Valor de π", col ="red")
graphics.off()

x_range <-seq(1,20,1)
png("Tiempos.png")
plot(x_range, tiempos, type="o", ylab="Tiempo(s)", xlab="Tamaño de la muestra", main="Tiempos")
graphics.off()
