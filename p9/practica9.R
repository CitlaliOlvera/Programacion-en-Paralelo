n <- 15
c <- rnorm(n)
#m <- round(abs(c * 50))
m <- 1:15 / 2
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


auxdatosx <- c(p$x)
auxdatosy <- c(p$y)

#png("p9i.png")
#grafica <- ggplot(p, aes(x=p$x, y=p$y))
#grafica+geom_point(aes(size= p$m, color = p$g))+ xlab("x")+ ylab("y") + 
 # labs(color= "carga", size="masa")
#graphics.off()

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
tmax <- 50
#digitos <- floor(log(tmax, 10)) + 1
#tl <- "0"
#while (nchar(tl) < digitos) {
 # tl <- paste("0", tl, sep="")
#}
#png(paste("p9_t", tl, ".png", sep=""))
#graficas <- ggplot(p, aes(x=p$x, y=p$y))
#graficas+geom_point(aes(size= p$m, color = p$g))+ xlab("x")+ ylab("y") + 
 # labs(color= "carga", size="masa")
#graphics.off()

for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
 # p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + (delta * (1/p[i,]$m)) * f[c(TRUE, FALSE)][i], 1), 0)
  #p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + (delta * (1/p[i,]$m)) * f[c(FALSE, TRUE)][i], 1), 0)
 
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta  * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta  * f[c(FALSE, TRUE)][i], 1), 0)
  
   
  auxdatosx <- c(auxdatosx, p$x)
  auxdatosy <- c(auxdatosy, p$y)
  
  #tl <- paste(iter, "", sep="")
  #while (nchar(tl) < digitos) {
   # tl <- paste("0", tl, sep="")
  #}
  #png(paste("p9_t", tl, ".png", sep=""))
  #graficass <- ggplot(p, aes(x=p$x, y=p$y))
  #graficass = graficass+geom_point(aes(size= p$m, color = p$g))+ xlab("x")+ ylab("y") + 
   # labs(color= "carga", size="masa")
  #print(graficass)
  #graphics.off()
}
velocidad <- function(i){
  v <- sqrt((auxdatosx[i]^2) + (auxdatosy[i]^2))
  return(v)
}


v <-  foreach(p = 1:765, .combine=c) %dopar% velocidad(p)
masas <- rep(p$m, 51)
datos <- data.frame(auxdatosx, auxdatosy, v, masas)
stopImplicitCluster()
system("convert -delay 50 -size 300x300 p9_t*.png -loop 0 p9.gif") # creamos ani

write.csv(datos[with(datos, order(masas)), ], file = "datosvelocidadsinmasa.csv")

sinmasa <- as.data.frame(read.csv("datosvelocidadsinmasa.csv"))
conmasa <- as.data.frame(read.csv("datosvelocidad1.csv"))

sinmasa1 <- c(sinmasa$v[1:51])
sinmasa2 <- c(sinmasa$v[52:102])
sinmasa3 <- c(sinmasa$v[103:153])
sinmasa4 <- c(sinmasa$v[154:204])
sinmasa5 <- c(sinmasa$v[205:255])
sinmasa6 <- c(sinmasa$v[256:306])
sinmasa7 <- c(sinmasa$v[307:357])
sinmasa8 <- c(sinmasa$v[358:408])
sinmasa9 <- c(sinmasa$v[409:459])
sinmasa10 <- c(sinmasa$v[460:510])
sinmasa11 <- c(sinmasa$v[511:561])
sinmasa12 <- c(sinmasa$v[562:612])
sinmasa13 <- c(sinmasa$v[613:663])
sinmasa14 <- c(sinmasa$v[664:714])
sinmasa15 <- c(sinmasa$v[715:765])

conmasa1 <- c(conmasa$v[1:51])
conmasa2 <- c(conmasa$v[52:102])
conmasa3 <- c(conmasa$v[103:153])
conmasa4 <- c(conmasa$v[154:204])
conmasa5 <- c(conmasa$v[205:255])
conmasa6 <- c(conmasa$v[256:306])
conmasa7 <- c(conmasa$v[307:357])
conmasa8 <- c(conmasa$v[358:408])
conmasa9 <- c(conmasa$v[409:459])
conmasa10 <- c(conmasa$v[460:510])
conmasa11 <- c(conmasa$v[511:561])
conmasa12 <- c(conmasa$v[562:612])
conmasa13 <- c(conmasa$v[613:663])
conmasa14 <- c(conmasa$v[664:714])
conmasa15 <- c(conmasa$v[715:765])

datosboxsm <- data.frame(sinmasa1, sinmasa2, sinmasa3, sinmasa4, sinmasa5, 
                         sinmasa6, sinmasa7, sinmasa8, sinmasa9, sinmasa10,
                         sinmasa11, sinmasa12, sinmasa13, sinmasa14, sinmasa15)
png("p9sm.png")
colnames(datosboxsm)<- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5)
boxplot(datosboxsm, use.cols =FALSE, xlab= "masa", ylab = "velocidad", cex.lab = 1.5, cex.axis = 1.5)
graphics.off()

datosboxcm <- data.frame(conmasa1, conmasa2, conmasa3, conmasa4, conmasa5, 
                         conmasa6, conmasa7, conmasa8, conmasa9, conmasa10,
                         conmasa11, conmasa12, conmasa13, conmasa14, conmasa15)
png("p9cm.png")
colnames(datosboxcm)<- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5)
boxplot(datosboxcm, use.cols =FALSE, xlab= "masa", ylab = "velocidad", cex.lab = 1.5, cex.axis = 1.5)
graphics.off()