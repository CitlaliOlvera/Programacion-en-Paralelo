library(ggplot2)
n <- 15
c <- rnorm(n)
m <- abs(c * 50)
p <- data.frame(x = rnorm(n), y=rnorm(n), c, m)
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)

png("p9i.png")
grafica <- ggplot(p, aes(x=p$x, y=p$y))
grafica+geom_point(aes(size= p$m, color = p$g))+ xlab("x")+ ylab("y") + 
  labs(color= "carga", size="masa")
graphics.off()

eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
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
  return(c(fx, fy))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
file.remove(list.files(pattern=".png")) # borramos anteriores en el caso que lo hayamos corrido
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
graficas <- ggplot(p, aes(x=p$x, y=p$y))
graficas+geom_point(aes(size= p$m, color = p$g))+ xlab("x")+ ylab("y") + 
  labs(color= "carga", size="masa")
graphics.off()

for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + (delta * (1/p[i,]$m)) * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + (delta * (1/p[i,]$m)) * f[c(FALSE, TRUE)][i], 1), 0)
  
  #p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
 # p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta  * f[c(FALSE, TRUE)][i], 1), 0)
  
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p9_t", tl, ".png", sep=""))
  graficass <- ggplot(p, aes(x=p$x, y=p$y))
  graficass = graficass+geom_point(aes(size= p$m, color = p$g))+ xlab("x")+ ylab("y") + 
    labs(color= "carga", size="masa")
  print(graficass)
  graphics.off()
}
stopImplicitCluster()
system("convert -delay 50 -size 300x300 p9_t*.png -loop 0 p9.gif") # creamos ani