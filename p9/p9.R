library(ggplot2)
n <- 20
fuerzas <- data.frame()
vx <- data.frame()
vy <- data.frame()
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1

colores <- c("blue","red")
p$g <- colores[(p$c < 0) + 1] # coloreamos segun la carga a 11 niveles de -5 a 5
p$m <- abs((100 * p$c))
p$r <- (p$m/10)

png("p9i.png" ,width = 400, height = 400)
ggplot() + scale_x_continuous(limits = c(0,1)) +  scale_y_continuous(limits = c(0,1))+
  geom_point(aes(x=p$x, y=p$y), data=p, size=p$r*2, shape=19, color= p$g)
graphics.off()
eps <- 0.001

fuerza <- function(i) {
    xi <- p[i,]$x
    yi <- p[i,]$y
    ci <- p[i,]$c
    cm <- p[i,]$m
    fx <- 0
    fy <- 0
    for (j in 1:n) {
        cj <- p[j,]$c
        dir <- (-1)^(1 + 1 * (ci * cj < 0))
        dx <- xi - p[j,]$x
        dy <- yi - p[j,]$y
        factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
        fx <- fx - dx * factor
        fy <- fy - dy * factor
    }
    return(c(fx/cm, fy/cm))
    #return(c(fx, fy))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

tmax <- 200
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
ggplot() + scale_x_continuous(limits = c(0,1)) +  scale_y_continuous(limits = c(0,1))+
  geom_point(aes(x=p$x, y=p$y), data=p, size=p$r*2, shape=1, color= p$g)
graphics.off()

rx <- function(i, delta,p){
  return(max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0))
}

ry <- function(i, delta,p){
  return(max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0))
}

for (iter in 1:tmax) {
    f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
    delta <- .01 / max(abs(f)) # que nadie desplace una paso muy largo
    p$x <- foreach(i = 1:n, .combine=c) %dopar% rx(i, delta,p)
    p$y <- foreach(i = 1:n, .combine=c) %dopar% ry(i, delta,p)
    
    tl <- paste0(iter, "", sep="")
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    png(paste0("p9_t", tl, ".png", sep=""))
    plot(ggplot() + scale_x_continuous(limits = c(0,1)) +  scale_y_continuous(limits = c(0,1))+
           geom_point(aes(x=p$x, y=p$y), data=p, size=p$r*2, shape=19, color= p$g))
    graphics.off()
    
    for(z in 1:n){
      v <- sqrt((f[z])^2 + (f[z + 1])^2)
      fuerzas <- rbind(fuerzas, v)
    }
}
write.table(fuerzas, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p9/fuerzas.csv", sep = ",")
write.table(p$m, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p9/masas.csv", sep = ",")
write.table(p$c, "/home/pabloide/Documentos/3 Semestre/R_paralelo/p9/cargas.csv", sep = ",")
stopImplicitCluster()

system("convert -delay 10 -size 300x300 p9_t*.png -loop 0 p9.gif") # creamos animacion con ImageMagick
system("rm p9*.png")

#velocidades <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p9/reporte/fuerzas.csv", header = TRUE)
#masas <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p9/reporte/masas.csv", header = TRUE)
#cargas <- read.csv("/home/pabloide/Documentos/3 Semestre/R_paralelo/p9/reporte/cargas.csv", header = TRUE)

#masas <- order(masas)

#velocidades <- t(matrix(velocidades$X0.111932036694811, ncol = 100))

#png("/home/pabloide/Documentos/3 Semestre/R_paralelo/p9/reporte/images/plot1.png")
#boxplot(velocidades[,masas], xlab="Masa", ylab="Velocidad")
#graphics.off()