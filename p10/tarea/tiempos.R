tiempo_p1 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/50_tiempos_tarea2.csv", sep = ",")
tiempo_p2 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/100_tiempos_tarea2.csv", sep = ",")
tiempo_p3 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/150_tiempos_tarea2.csv", sep = ",")
tiempo_p4 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/200_tiempos_tarea2.csv", sep = ",")
tiempo_p5 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/250_tiempos_tarea2.csv", sep = ",")

tiempo_s1 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/50_tiempos_tarea_s.csv", sep = ",")
tiempo_s2 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/100_tiempos_tarea_s.csv", sep = ",")
tiempo_s3 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/150_tiempos_tarea_s.csv", sep = ",")
tiempo_s4 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/200_tiempos_tarea_s.csv", sep = ",")
tiempo_s5 <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/250_tiempos_tarea_s.csv", sep = ",")


tiempos_p <- data.frame(tiempo_p1,tiempo_p3,tiempo_p3,tiempo_p4,tiempo_p5)
colnames(tiempos_p) <- c(50,100,150,200,250)

tiempos_s <- data.frame(tiempo_s1,tiempo_s3,tiempo_s3,tiempo_s4,tiempo_s5)
colnames(tiempos_s) <- c(50,100,150,200,250)

png("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/tiempos_p.png")
boxplot(tiempos_p, xlab="Población Inicial" ,ylab ="Tiempo (s)", main="Tiempos", ylim=c(0,2.5))
graphics.off()

png("/home/pabloide/Documentos/3 Semestre/R_paralelo/p10/tarea/tiempos_s.png")
boxplot(tiempos_s, xlab="Población Inicial", ylab ="Tiempo (s)", main="Tiempos", ylim=c(0,2.5))
graphics.off()
