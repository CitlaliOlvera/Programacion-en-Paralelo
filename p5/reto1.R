library(parallel)
runs <- c(20000,40000,60000,80000,100000)

auxerror <- c()
tiempobox <- c()
for(v in runs){
runs <- v
cuantos <- 300
iteraciones <- 30

aprox <- data.frame()
tiempo <- data.frame()


monpi <- function(z){
  xs <- runif(runs,min=-0.5,max=0.5)
  ys <- runif(runs,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  mc.pi <- (sum(in.circle)/ runs)* 4
  return(mc.pi)
}

for(j in 1:iteraciones){
 # print(j)
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster,"monpi")
  clusterExport(cluster,"runs")
  t <- system.time(montecarlopi <- parSapply(cluster,1:cuantos,monpi))
  stopCluster(cluster)
  apropi <- (sum(montecarlopi)/cuantos)
  if(j == 1){
    aprox <- c(aprox, apropi)  
  }
  else{
    aprox<- cbind(aprox, apropi)
  }
  tiempo <-rbind(tiempo,t[3])
 auxerror <- c(auxerror, abs(apropi - 3.14159265))
 tiempobox <- c(tiempobox, t[3])
}

colnames(tiempo) <- c("tiempos")
x <- c(1:iteraciones)
png(paste0(v,"_tiempos.png"))
plot(x, tiempo$tiempos, type="o", main="Tiempos")
graphics.off()
png(paste0(v,"_aproximaciones.png"))
plot(x, aprox, type="o", main="Aproximaciones")
abline(h=3.14159265, col ="red")
graphics.off()
#}

error2 <- c(auxerror[1:30])
error4 <- c(auxerror[31:60])
error6 <- c(auxerror[61:90])
error8 <- c(auxerror[91:120])
error10 <- c(auxerror[121:150])
error <- data.frame(error2, error4, error6, error8, error10)

t2 <- c(tiempobox[1:30])
t4 <- c(tiempobox[31:60])
t6 <- c(tiempobox[61:90])
t8 <- c(tiempobox[91:120])
t10 <- c(tiempobox[121:150])
tiempoboxplot <- data.frame(t2, t4, t6, t8, t10)


png("errorR1.png")
colnames(error)<- c(2,4,6,8,10)
boxplot(error, use.cols=FALSE, 
        xlab="corridas", ylab="error", cex.lab = 1.5, cex.axis= 1.5)
graphics.off()

png("tiempoR1.png")
colnames(tiempoboxplot)<- c(2,4,6,8,10)
boxplot(tiempoboxplot, use.cols=FALSE, 
        xlab="corridas", ylab="tiempo", cex.lab = 1.5, cex.axis= 1.5)
graphics.off()
}