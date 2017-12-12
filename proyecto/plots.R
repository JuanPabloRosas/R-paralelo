tamaños <- read.table("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/instancias_medianas/r_m/tamaños.txt", sep="\n")
tiempos_computo <- read.table("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/instancias_medianas/r_m/tiempos_computo.txt", sep="\n")
tiempos <- read.table("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/instancias_medianas/r_m/mejores_tiempos.txt", sep = "\n")
tiempos_optimos <- read.table("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/instancias_medianas/r_m/tiempos_optimos.txt", sep="\n")
promedios <- read.table("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/instancias_medianas/r_m/mejores_promedios.txt", sep="\n")
promedios_optimos <- read.table("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/instancias_medianas/r_m/promedios_optimos.txt", sep="\n")
cuantos_optimos <- read.table("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/instancias/instancias_medianas/r_m/cuantos_optimos.txt", sep="\n")

t <- matrix(tiempos$V1,ncol = 6)
t <- cbind(t,tiempos_optimos$V1)
colnames(t)<-c("R2000","R5000","R10000","R15000", "R20000", "R25000","Optimo")
t <- data.frame(t)

c <- matrix(cuantos_optimos$V1,ncol = 6)
c <- cbind(c,tiempos_optimos$V1)
colnames(c)<-c("R2000","R5000","R10000","R15000", "R20000", "R25000","Optimo")
c <- data.frame(c)

c <- data.frame(cuantos_optimos)
c <- cbind(c,c(rep(2000,75),rep(5000,75),rep(10000,75),rep(15000,75),rep(20000,75),rep(25000,75)))
c <- cbind(c,c(rep(1:75,6)))
colnames(c)<-c("Cuantos","Repeticiones", "Problema")



plot(c$R2000, col="red",ylim=c(0,25), type="l", ylab = "Cantidad", xlab = "Numero de Problema")
lines(c$R5000, col="blue")
lines(c$R10000, col="yellow")
lines(c$R15000, col="brown")
lines(c$R20000, col="black")
lines(c$R25000, col="purple")
legend(68,24,c("R2000", "R5000","R10000","R15000", "R20000", "R25000"), lty=c(1,1,1,1,1,1), lwd=c(2.5,2.5),col=c("red","blue","yellow","brown","black","purple"))

ggplot(c, aes(c$Repeticiones, c$Cuantos, group = c$Problema)) +
  geom_area(aes(fill = c$Problema)) +
  geom_line(aes(group = c$Problema), position = "stack")
boxplot(tiempos_computo$V1[1:75])

# Map the time of day to different fill colors
ggplot(data=c, aes(x=c$Problema, y=c$Cuantos, fill=c$Repeticiones)) +
  geom_bar(stat="identity" ,position = "stack") +
  ylim(0,5)

ggplot()+
  geom_line(data=c[1:75,],aes(y=c$Cuantos[1:75],x= c(1:75),fill=c$Repeticiones[1:75]),size=1, colour="red" ) +
  geom_line(data=c[76:150,],aes(y=c$Cuantos[76:150],x= c(1:75),fill=c$Repeticiones[76:150]),size=1, colour="blue" ) +
  geom_line(data=c[151:225,],aes(y=c$Cuantos[151:225],x= c(1:75),fill=c$Repeticiones[151:225]),size=1, colour="green" ) +
  geom_line(data=c[226:300,],aes(y=c$Cuantos[226:300],x= c(1:75),fill=c$Repeticiones[226:300]),size=1, colour="purple" ) +
  geom_line(data=c[301:375,],aes(y=c$Cuantos[301:375],x= c(1:75),fill=c$Repeticiones[301:375]),size=1, colour="yellow" ) +
  geom_line(data=c[376:450,],aes(y=c$Cuantos[376:450],x= c(1:75),fill=c$Repeticiones[376:450]),size=1, colour="orange" ) +
  scale_color_discrete(name = "Repeticiones", labels = c$Repeticiones) 


library(ggplot2)
ggplot()+
  geom_line(data=c,aes(y=c$R2000,x= c(1:75)),size=1, col="red")+
  geom_line(data=c,aes(y=c$R5000,x= c(1:75)),size=1, col="blue") +
  geom_line(data=c,aes(y=c$R10000,x= c(1:75)),size=1, col="yellow") +
  geom_line(data=c,aes(y=c$R15000,x= c(1:75)),size=1, col="brown") +
  geom_line(data=c,aes(y=c$R20000,x= c(1:75)),size=1, col="black") +
  geom_line(data=c,aes(y=c$R25000,x= c(1:75)),size=1, col="purple") +
  #scale_color_discrete(name = "Repeticiones", labels = c("R2000", "R5000","R10000","R15000", "R20000", "R25000")) +
  scale_color_manual(values=c(R2000="blue4", R5000="red4",R10000="purple4", R15000="gray", R20000="gray", R25000="gray"))+
  scale_y_continuous("Tiempo(s)", limits = c(0,50)) + 
  labs(title="Tiempos")



plot(tiempos_optimos[,1], col="green", type = "l", xlab = "Numero de Problema", ylab = "Tiempo", main = "Problemas U")
lines(tiempos[1:75,], col="orange")
lines(tiempos[76:150,], col="blue")
lines(tiempos[151:225,], col="red")
lines(tiempos[226:300,], col="gray")
lines(tiempos[301:375,], col="yellow")
lines(tiempos[376:450,], col="black")

plot(tiempos_computo[1:75,], col="orange", type = "l", ylim = range(0:15), ylab = "Tiempo(s)", xlab = "Numero de Problema", main = "Tiempo Computacional")
lines(tiempos_computo[76:150,], col="blue")
lines(tiempos_computo[151:225,], col="red")
lines(tiempos_computo[226:300,], col="gray")
lines(tiempos_computo[301:375,], col="yellow")
lines(tiempos_computo[376:450,], col="black")

png("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_p/soluciones.png")
plot(promedios[,1],tiempos[,1] , xlab = "Puntaje", ylab="Tiempo", main="Mejores Soluciones", col="green", pch=16)
points(promedios_optimos[,1],tiempos_optimos[,1], col="red")
graphics.off()


png("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_p/soluciones_optimas.png")
plot(promedios[,1],tiempos[,1] , xlab = "Puntaje", ylab="Tiempo", main="Mejores Soluciones")
graphics.off()

png("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_p/tamaños.png")
boxplot(tamaños, main ="Tamaño de las Soluciones", ylab="Cantidad de Actividades")
graphics.off()

#--------------------------------------------------------------------------------------------------------------------------

tamaños <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_m/tamaños.txt", sep="\n")
tiempos <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_m/mejores_tiempos.txt", sep="\n")
tiempos_optimos <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_m/tiempos_optimos.txt", sep="\n")
promedios <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_m/mejores_promedios.txt", sep="\n")
promedios_optimos <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_m/promedios_optimos.txt", sep="\n")

plot(c(60:200), xlim = c(60:260), type="n")
plot(tiempos_optimos[,1], col="red", type = "l")
lines(tiempos[,1], col="green")

png("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_p/soluciones.png")
plot(promedios[,1],tiempos[,1] , xlab = "Puntaje", ylab="Tiempo", main="Mejores Soluciones", col="green", pch=16)
points(promedios_optimos[,1],tiempos_optimos[,1], col="red")
graphics.off()


png("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_p/soluciones_optimas.png")
plot(promedios[,1],tiempos[,1] , xlab = "Puntaje", ylab="Tiempo", main="Mejores Soluciones")
graphics.off()

png("/home/pabloide/Documentos/3 Semestre/R_paralelo/proyecto/resultados_p/tamaños.png")
boxplot(tamaños, main ="Tamaño de las Soluciones", ylab="Cantidad de Actividades")
graphics.off()

library(ggplot2)
ggplot(data=tiempos, aes(x=c(1:149), y=X884)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+
  theme_minimal()