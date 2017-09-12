  
  pedazos <- c(10000,50000,100000,150000,200000)
  auxerrorp5 <- c()
  tiempoboxp5 <- c()
  
  #inicio <- -6
  #final <- -inicio
  #paso <- 0.25
  #x <- seq(inicio, final, paso)
  f <- function(x) { return(1 / (exp(x) + exp(-x))) }
  #png("p5f.png") # dibujamos f(x) para ver como es
  #plot(x,  (2/pi) * (1/(exp(x)+exp(-x))))
  #lines(x,  (2/pi) * (1/(exp(x)+exp(-x))), type="l")
  #graphics.off()
  suppressMessages(library(distr))
  g <- function(x) { return((2 / pi) * f(x)) }
  generador  <- r(AbscontDistribution(d = g)) # creamos un generador
  #muestra <- generador(50000) # sacamos una muestra
  #png("p5m.png") # validamos con un dibujo
  #hist(muestra, freq=F, breaks=50,
   #    main="Histograma de g(x) comparado con g(x)",
    #   xlim=c(inicio, final), ylim=c(0, 0.4))
  #lines(x, g(x), col="red") # dibujamos g(x) encima del histograma
#graphics.off()
for(v in pedazos){
desde <- 3
hasta <- 7
pedazo <- v
cuantos <- 100
iteracionesp5 <- 10
aproxp5 <- data.frame()
tiempop5 <- data.frame()

parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}

for(j in 1:iteracionesp5){
  print(j)
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
tp5 <- system.time(montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte())
stopImplicitCluster()
integral <- sum(montecarlo) / (cuantos * pedazo)
integralverdadera <-(pi / 2) * integral
if(j ==1){
  aproxp5 <- c(aproxp5,integralverdadera)
  }else{
  aproxp5 <- cbind(aproxp5, integralverdadera)
}
tiempop5 <- rbind(tiempop5,tp5[3])
auxerrorp5 <- c(auxerrorp5, abs(integralverdadera - 0.048834))
tiempoboxp5 <- c(tiempoboxp5, tp5[3])
}

colnames(tiempop5) <- c("tiempos")
x <- c(1:iteracionesp5)
png(paste0(v, "_tiemposP5.png"))
plot(x, tiempop5$tiempos, type = "o", main = "Tiempos")
graphics.off()
png(paste0(v,"_aproximacionesP5.png"))
plot(x, aproxp5, type="o", main = "Aproximaciones")
abline(h=0.048834, col="red")
graphics.off()

error2p5 <- c(auxerrorp5[1:10])
error4p5 <- c(auxerrorp5[11:20])
error6p5 <- c(auxerrorp5[21:30])
error8p5 <- c(auxerrorp5[31:40])
error10p5 <- c(auxerrorp5[41:50])
errorp5 <- data.frame(error2p5, error4p5, error6p5, error8p5, error10p5)

t2p5 <- c(tiempoboxp5[1:10])
t4p5 <- c(tiempoboxp5[11:20])
t6p5 <- c(tiempoboxp5[21:30])
t8p5 <- c(tiempoboxp5[31:40])
t10p5 <- c(tiempoboxp5[41:50])
tiempoboxplotp5 <- data.frame(t2p5, t4p5, t6p5, t8p5, t10p5)

png("errorP5.png")
colnames(errorp5)<- c(1,5,10,15,20)
boxplot(errorp5, use.cols=FALSE, 
        xlab="corridas", ylab="error", cex.lab = 1.5, cex.axis= 1.5)
graphics.off()

png("tiempoP5.png")
colnames(tiempoboxplotp5)<- c(1,5,10,15,20)
boxplot(tiempoboxplotp5, use.cols=FALSE, 
        xlab="corridas", ylab="tiempo", cex.lab = 1.5, cex.axis= 1.5)
graphics.off()
}