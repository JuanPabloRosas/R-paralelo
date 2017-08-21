datos <- read.csv("resultados.csv", header = TRUE)
datos2 <- read.csv("resultados2.csv", header = TRUE)

datos$X <- NULL
datos2$X <- NULL

png("r1_bp.png")
boxplot(datos, use.cols = FALSE, names = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))

png("r2_bp.png")
boxplot(datos2, use.cols = FALSE, names = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))


p1 <- mean(datos$V1)
p2 <- mean(datos$V2)
p3 <- mean(datos$V3)
p4 <- mean(datos$V4)
p5 <- mean(datos$V5)
p6 <- mean(datos$V6)
p7 <- mean(datos$V7)
p8 <- mean(datos$V8)
p9 <- mean(datos$V9)
p10 <- mean(datos$V10)

p1_2 <- mean(datos2$V1)
p2_2 <- mean(datos2$V2)
p3_2 <- mean(datos2$V3)
p4_2 <- mean(datos2$V4)
p5_2 <- mean(datos2$V5)
p6_2 <- mean(datos2$V6)
p7_2 <- mean(datos2$V7)
p8_2 <- mean(datos2$V8)
p9_2 <- mean(datos2$V9)
p10_2 <- mean(datos2$V10)

promedios1 <- matrix(c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10))
promedios2 <- matrix(c(p1_2,p2_2,p3_2,p4_2,p5_2,p6_2,p7_2,p8_2,p9_2,p10_2))

png("r1_p.png")
plot(c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1), promedios1, type="b", ylab = "Numero de Iteraciones", xlab = "Probabilidad de Nucleos")
png("r2_p.png")
plot(c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1), promedios2, type="b", ylab = "Numero de Iteraciones", xlab = "Probabilidad de Nucleos")

