2#SIMULACION p-U
tamaños <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/U/resultados/tamaños.txt", sep="\n")
tiempos_computo <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/U/resultados/tiempos_computo.txt", sep="\n")
mejores_tiempos <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/U/resultados/mejores_tiempos.txt", sep = "\n")
mejores_promedios <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/U/resultados/mejores_promedios.txt", sep="\n")
cuantos_optimos <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/U/resultados/cuantos_optimos.txt", sep="\n")
#OPTIC
estados_optic <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/U/estados.txt", sep = "\n")
metricas_optic <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/U/metricas.txt", sep = "\n")
tiempos_optic <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/U/tiempos.txt", sep = "\n")
#OPTIMO
tiempos_optimos <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/p/U.txt", sep="\n")
promedios_optimos <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/p/pU.txt", sep="\n")

#SIMULACION p-V
tamaños2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/V/resultados/tamaños.txt", sep="\n")
tiempos_computo2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/V/resultados/tiempos_computo.txt", sep="\n")
mejores_tiempos2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/V/resultados/mejores_tiempos.txt", sep = "\n")
mejores_promedios2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/V/resultados/mejores_promedios.txt", sep="\n")
cuantos_optimos2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/p/V/resultados/cuantos_optimos.txt", sep="\n")
#OPTIC
estados_optic2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/V/estados.txt", sep = "\n")
metricas_optic2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/V/metricas.txt", sep = "\n")
tiempos_optic2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/V/tiempos.txt", sep = "\n")
#OPTIMO
tiempos_optimos2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/p/V.txt", sep="\n")
promedios_optimos2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/p/pV.txt", sep="\n")


#OPTIMO - SIMULACION
png("/home/juanpablo/Documentos/simulacion/instancias/plots/p/U/optimo_simulacion.png", width = 950)
plot(mejores_tiempos$V1[376:450], ylim=c(0,300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(tiempos_optimos$V1, type = "h", col="green", lwd= 6)
legend(64,290,c("Simulacion", "Optimo"), lty=c(1,1), lwd=c(10,8),col=c("blue","green"))
graphics.off()


#OPTIMO - OPTIC
png("/home/juanpablo/Documentos/simulacion/instancias/plots/p/U/optimo_optic.png", width = 950)
plot(metricas_optic$V1, ylim=c(0,300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(tiempos_optimos$V1, type = "h", col="green", lwd= 6)
legend(64,290,c("OPTIC", "Optimo"), lty=c(1,1), lwd=c(10,8),col=c("blue","green"))
graphics.off()


###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################


2#SIMULACION m-U
tamaños <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/U/resultados/tamaños.txt", sep="\n")
tiempos_computo <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/U/resultados/tiempos_computo.txt", sep="\n")
mejores_tiempos <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/U/resultados/mejores_tiempos.txt", sep = "\n")
mejores_promedios <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/U/resultados/mejores_promedios.txt", sep="\n")
cuantos_optimos <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/U/resultados/cuantos_optimos.txt", sep="\n")
#OPTIC
estados_optic <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/m/datos/U/estados.txt", sep = "\n")
metricas_optic <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/m/datos/U/metricas.txt", sep = "\n")
tiempos_optic <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/m/datos/U/tiempos.txt", sep = "\n")
#OPTIMO
tiempos_optimos <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/m/U.txt", sep="\n")
promedios_optimos <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/m/pU.txt", sep="\n")

#SIMULACION m-V
tamaños2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/V/resultados/tamaños.txt", sep="\n")
tiempos_computo2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/V/resultados/tiempos_computo.txt", sep="\n")
mejores_tiempos2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/V/resultados/mejores_tiempos.txt", sep = "\n")
mejores_promedios2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/V/resultados/mejores_promedios.txt", sep="\n")
cuantos_optimos2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/m/V/resultados/cuantos_optimos.txt", sep="\n")
#OPTIC
estados_optic2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/m/datos/V/estados.txt", sep = "\n")
metricas_optic2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/m/datos/V/metricas.txt", sep = "\n")
tiempos_optic2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/p/datos/V/tiempos.txt", sep = "\n")
#OPTIMO
tiempos_optimos2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/p/V.txt", sep="\n")
promedios_optimos2 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/p/pV.txt", sep="\n")

######################## U
#OPTIMO - SIMULACION
png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/optimo_simulacion.png", width = 950)
plot(mejores_tiempos$V1[376:450], ylim=c(0,1300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(tiempos_optimos$V1, type = "h", col="green", lwd= 6)
legend(64,1290,c("Simulacion", "Optimo"), lty=c(1,1), lwd=c(10,8),col=c("blue","green"))
graphics.off()


#OPTIMO - OPTIC
png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/optimo_optic.png", width = 950)
plot(metricas_optic$V1, ylim=c(0,1300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(tiempos_optimos$V1, type = "h", col="green", lwd= 6)
legend(64,1290,c("OPTIC", "Optimo"), lty=c(1,1), lwd=c(10,8),col=c("blue","green"))
graphics.off()

#OPTIMO - SIMULACION - OPTIC
png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/optimo_simulacion_optic.png", width = 950)
plot(mejores_tiempos$V1[376:450], ylim=c(0,1300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(metricas_optic$V1,  type = "h", col="red", lwd= 6)
lines(tiempos_optimos$V1, type = "h", col="green", lwd= 6)
legend(64,1290,c("Simulacion", "Optimo", "OPTIC"), lty=c(1,1,1), lwd=c(10,8,8),col=c("blue","green","red"))
graphics.off()

mejores_tiempos <- matrix(mejores_tiempos$V1, nrow = 75)
colnames(mejores_tiempos) <- c("10000","20000","30000","40000", "50000", "60000")
mejores_promedios <- matrix(mejores_promedios$V1, nrow = 75)
colnames(mejores_promedios) <- c("10000","20000","30000","40000", "50000", "60000")
tiempos_computo <- matrix(tiempos_computo$V1, nrow = 75)
colnames(tiempos_computo) <- c("10000","20000","30000","40000", "50000", "60000")
temporal <- matrix(c(tiempos_optic$V1,tiempos_computo[,6]), nrow = 75)
colnames(temporal) <- c("OPTIC", "60000")

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/metricas_tiempo.png", width = 950)
boxplot(mejores_tiempos, ylab = "Tiempo (s)",xlab="Cantidad de Soluciones")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/metricas_promedio.png", width = 950)
boxplot(mejores_promedios, ylab = "Tiempo (s)", xlab="Cantidad de Soluciones")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/tiempos_computo_sim.png", width = 950)
boxplot(tiempos_computo, ylab = "Tiempo (s)", xlab="Cantidad de Soluciones")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/tiempos_computo_sim_opt.png", width = 950)
boxplot(temporal, ylab = "Tiempo (s)")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/soluciones1.png")
plot(mejores_promedios[,1], mejores_tiempos[,1], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="10000", ylim = c(870,1150), xlim = c(70,88))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/soluciones2.png")
plot(mejores_promedios[,2], mejores_tiempos[,2], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="20000", ylim = c(870,1150), xlim = c(70,88))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/soluciones3.png")
plot(mejores_promedios[,3], mejores_tiempos[,3], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="30000", ylim = c(870,1150), xlim = c(70,88))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/soluciones4.png")
plot(mejores_promedios[,4], mejores_tiempos[,4], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="40000", ylim = c(870,1150), xlim = c(70,88))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/soluciones5.png")
plot(mejores_promedios[,5], mejores_tiempos[,5], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="50000", ylim = c(870,1150), xlim = c(70,88))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/soluciones6.png")
plot(mejores_promedios[,6], mejores_tiempos[,6], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="60000", ylim = c(870,1150), xlim = c(70,88))
graphics.off()



png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/mueve_sol1.png")
plot(mejores_promedios[1,1], mejores_tiempos[1,1], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="10000", ylim = c(800,1000), xlim = c(70,90))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/mueve_sol2.png")
plot(mejores_promedios[1,2], mejores_tiempos[1,2], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="20000", ylim = c(800,1000), xlim = c(70,90))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/mueve_sol3.png")
plot(mejores_promedios[1,3], mejores_tiempos[1,3], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="30000", ylim = c(800,1000), xlim = c(70,90))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/mueve_sol4.png")
plot(mejores_promedios[1,4], mejores_tiempos[1,4], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="40000", ylim = c(800,1000), xlim = c(70,90))
graphics.off()
png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/mueve_sol5.png")
plot(mejores_promedios[1,5], mejores_tiempos[1,5], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="50000", ylim = c(800,1000), xlim = c(70,90))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/U/mueve_sol6.png")
plot(mejores_promedios[1,6], mejores_tiempos[1,6], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="60000", ylim = c(800,1000), xlim = c(70,90))
graphics.off()
################# U

################# V


#OPTIMO - SIMULACION
png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/optimo_simulacion.png", width = 950)
plot(mejores_tiempos2$V1[376:450], ylim=c(0,1300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(tiempos_optimos2$V1, type = "h", col="green", lwd= 6)
legend(64,1290,c("Simulacion", "Optimo"), lty=c(1,1), lwd=c(10,8),col=c("blue","green"))
graphics.off()


#OPTIMO - OPTIC
png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/optimo_optic.png", width = 950)
plot(metricas_optic2$V1, ylim=c(0,1300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(tiempos_optimos2$V1, type = "h", col="green", lwd= 6)
legend(64,1290,c("OPTIC", "Optimo"), lty=c(1,1), lwd=c(10,8),col=c("blue","green"))
graphics.off()

#OPTIMO - SIMULACION - OPTIC
png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/optimo_simulacion_optic.png", width = 950)
plot(mejores_tiempos2$V1[376:450], ylim=c(0,1300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(metricas_optic2$V1,  type = "h", col="red", lwd= 6)
lines(tiempos_optimos2$V1, type = "h", col="green", lwd= 6)
legend(64,1290,c("Simulacion", "Optimo", "OPTIC"), lty=c(1,1,1), lwd=c(10,8,8),col=c("blue","green","red"))
graphics.off()


mejores_tiempos2 <- matrix(mejores_tiempos2$V1, nrow = 75)
colnames(mejores_tiempos2) <- c("10000","20000","30000","40000", "50000", "60000")
mejores_promedios2 <- matrix(mejores_promedios2$V1, nrow = 75)
colnames(mejores_promedios2) <- c("10000","20000","30000","40000", "50000", "60000")
tiempos_computo2 <- matrix(tiempos_computo2$V1, nrow = 75)
colnames(tiempos_computo2) <- c("10000","20000","30000","40000", "50000", "60000")
temporal2 <- matrix(c(tiempos_optic2$V1,tiempos_computo2[,6]), nrow = 75)
colnames(temporal2) <- c("OPTIC", "60000")

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/metricas_tiempo.png", width = 950)
boxplot(mejores_tiempos2, ylab = "Tiempo (s)",xlab="Cantidad de Soluciones")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/metricas_promedio.png", width = 950)
boxplot(mejores_promedios2, ylab = "Tiempo (s)", xlab="Cantidad de Soluciones")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/tiempos_computo_sim.png", width = 950)
boxplot(tiempos_computo2, ylab = "Tiempo (s)", xlab="Cantidad de Soluciones")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/tiempos_computo_sim_opt.png", width = 950)
boxplot(temporal2, ylab = "Tiempo (s)")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones1.png")
plot(mejores_promedios2[,1], mejores_tiempos2[,1], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="10000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones2.png")
plot(mejores_promedios2[,2], mejores_tiempos2[,2], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="20000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones3.png")
plot(mejores_promedios2[,3], mejores_tiempos2[,3], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="30000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones4.png")
plot(mejores_promedios2[,4], mejores_tiempos2[,4], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="40000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones5.png")
plot(mejores_promedios2[,5], mejores_tiempos2[,5], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="50000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones6.png")
plot(mejores_promedios2[,6], mejores_tiempos2[,6], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="60000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()



png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol1.png")
plot(mejores_promedios2[1,1], mejores_tiempos2[1,1], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="10000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol2.png")
plot(mejores_promedios2[1,2], mejores_tiempos2[1,2], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="20000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol3.png")
plot(mejores_promedios2[1,3], mejores_tiempos2[1,3], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="30000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol4.png")
plot(mejores_promedios2[1,4], mejores_tiempos2[1,4], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="40000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()
png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol5.png")
plot(mejores_promedios2[1,5], mejores_tiempos2[1,5], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="50000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol6.png")
plot(mejores_promedios2[1,6], mejores_tiempos2[1,6], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="60000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()



############## V


#SIMULACION g-U
tamaños3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/g/U/resultados/tamaños.txt", sep="\n")
tiempos_computo3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/g/U/resultados/tiempos_computo.txt", sep="\n")
mejores_tiempos3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/g/U/resultados/mejores_tiempos.txt", sep = "\n")
mejores_promedios3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/g/U/resultados/mejores_promedios.txt", sep="\n")
cuantos_optimos3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/simulacion/g/U/resultados/cuantos_optimos.txt", sep="\n")
#OPTIC
estados_optic3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/g/datos/U/estados.txt", sep = "\n")
metricas_optic3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/g/datos/U/metricas.txt", sep = "\n")
tiempos_optic3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optic/g/datos/U/tiempos.txt", sep = "\n")
#OPTIMO
tiempos_optimos3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/g/U.txt", sep="\n")
promedios_optimos3 <- read.table("/home/juanpablo/Documentos/simulacion/instancias/optimo/g/pU.txt", sep="\n")


#OPTIMO - SIMULACION
png("/home/juanpablo/Documentos/simulacion/instancias/plots/g/U/optimo_simulacion.png")
plot(mejores_tiempos3$V1, ylim=c(0,3300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(tiempos_optimos3$V1, type = "h", col="green", lwd= 6)
legend(12,3290,c("Simulacion", "Optimo"), lty=c(1,1), lwd=c(10,8),col=c("blue","green"))
graphics.off()


#OPTIMO - OPTIC
png("/home/juanpablo/Documentos/simulacion/instancias/plots/g/U/optimo_optic.png")
plot(metricas_optic3$V1, ylim=c(0,3300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(tiempos_optimos3$V1, type = "h", col="green", lwd= 6)
legend(12,3290,c("OPTIC", "Optimo"), lty=c(1,1), lwd=c(10,8),col=c("blue","green"))
graphics.off()

#OPTIMO - SIMULACION - OPTIC
png("/home/juanpablo/Documentos/simulacion/instancias/plots/g/U/optimo_simulacion_optic.png")
plot(mejores_tiempos3$V1, ylim=c(0,3300), type = "h", col="blue", lwd= 8, xlab = "Numero de Problema", ylab = "Tiempo (s)")
lines(metricas_optic3$V1,  type = "h", col="red", lwd= 6)
lines(tiempos_optimos3$V1, type = "h", col="green", lwd= 6)
legend(12,3290,c("Simulacion", "Optimo", "OPTIC"), lty=c(1,1,1), lwd=c(10,8,8),col=c("blue","green","red"))
graphics.off()


mejores_tiempos2 <- matrix(mejores_tiempos2$V1, nrow = 75)
colnames(mejores_tiempos2) <- c("10000","20000","30000","40000", "50000", "60000")
mejores_promedios2 <- matrix(mejores_promedios2$V1, nrow = 75)
colnames(mejores_promedios2) <- c("10000","20000","30000","40000", "50000", "60000")
tiempos_computo2 <- matrix(tiempos_computo2$V1, nrow = 75)
colnames(tiempos_computo2) <- c("10000","20000","30000","40000", "50000", "60000")
temporal3 <- matrix(c(tiempos_optic3$V1[1:20],tiempos_computo3$V1), ncol = 2)
colnames(temporal3) <- c("OPTIC", "100000")

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/metricas_tiempo.png", width = 950)
boxplot(mejores_tiempos2, ylab = "Tiempo (s)",xlab="Cantidad de Soluciones")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/metricas_promedio.png", width = 950)
boxplot(mejores_promedios2, ylab = "Tiempo (s)", xlab="Cantidad de Soluciones")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/tiempos_computo_sim.png", width = 950)
boxplot(tiempos_computo2, ylab = "Tiempo (s)", xlab="Cantidad de Soluciones")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/tiempos_computo_sim_opt.png", width = 950)
boxplot(temporal3, ylab = "Tiempo (s)")
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones1.png")
plot(mejores_promedios2[,1], mejores_tiempos2[,1], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="10000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones2.png")
plot(mejores_promedios2[,2], mejores_tiempos2[,2], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="20000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones3.png")
plot(mejores_promedios2[,3], mejores_tiempos2[,3], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="30000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones4.png")
plot(mejores_promedios2[,4], mejores_tiempos2[,4], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="40000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones5.png")
plot(mejores_promedios2[,5], mejores_tiempos2[,5], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="50000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/soluciones6.png")
plot(mejores_promedios2[,6], mejores_tiempos2[,6], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="60000", ylim = c(900,1150), xlim = c(78,95))
graphics.off()



png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol1.png")
plot(mejores_promedios2[1,1], mejores_tiempos2[1,1], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="10000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol2.png")
plot(mejores_promedios2[1,2], mejores_tiempos2[1,2], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="20000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol3.png")
plot(mejores_promedios2[1,3], mejores_tiempos2[1,3], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="30000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol4.png")
plot(mejores_promedios2[1,4], mejores_tiempos2[1,4], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="40000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()
png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol5.png")
plot(mejores_promedios2[1,5], mejores_tiempos2[1,5], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="50000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()

png("/home/juanpablo/Documentos/simulacion/instancias/plots/m/V/mueve_sol6.png")
plot(mejores_promedios2[1,6], mejores_tiempos2[1,6], type = "p", col="green", pch = 19, ylab = "Tiempo (s)", xlab="Promedio", main="60000", ylim = c(800,1000), xlim = c(80,100))
graphics.off()

