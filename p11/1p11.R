library(parallel)
cluster <- makeCluster(detectCores() - 1)
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

vc <- 4
md <- 3
tc <- 5
k <- 2 # cuantas funciones objetivo
obj <- list()
for (i in 1:k) {
  obj[[i]] <- poli(vc, md, tc)
}
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 200 # cuantas soluciones aleatorias
porcen.sols.no.dom <- c()
#tiempos <- c()
for(ij in 1:25){
#tp1 <- Sys.time()
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
#########funcion a paralelizar##############
evalua.solucion <- function(i){
  val <- matrix(rep(NA, k), ncol=k)
  for (j in 1:k) { # para todos los objetivos
    val[,j] <- eval(obj[[j]], sol[i,], tc)
  }
  return(val)
}
###########################################

clusterExport(cluster, "n")
clusterExport(cluster, "k")
clusterExport(cluster, "sol")
clusterExport(cluster, "tc")
clusterExport(cluster, "obj")
clusterExport(cluster, "eval")
clusterExport(cluster, "dim")
clusterExport(cluster, "evalua.solucion")

val <- parSapply(cluster, 1:n, evalua.solucion)
val <- t(val)

mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
png("p11_init.png")
plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
graphics.off()
png("p11_mejores.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=1.5)
points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=1.5)
graphics.off()
no.dom <- logical()
dominadores <- integer()

###############funciones a paralelizar #########
dominadores.aux <- function(i){
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  return(dominadores)
}

quien.dominado <- function(i){
  no.dom <- c(no.dom, dominadores[i] == 0) # nadie le domina
  return(no.dom)
}
##################################################

clusterExport(cluster, "quien.dominado")
clusterExport(cluster, "n")
clusterExport(cluster, "val")
clusterExport(cluster, "k")
clusterExport(cluster, "sign")
clusterExport(cluster, "domin.by")
clusterExport(cluster, "dominadores")
clusterExport(cluster, "no.dom")
clusterExport(cluster, "dominadores.aux")

dominadores <- parSapply(cluster, 1:n, dominadores.aux)
clusterExport(cluster, "dominadores")
no.dom <- parSapply(cluster, 1:n, quien.dominado)

frente <- subset(val, no.dom) # solamente las no dominadas
png("p11_frente.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()
library(ggplot2) # recordar instalar si hace falta
data <- data.frame(pos=rep(0, n), dom=dominadores)
png("p11_violin.png")
gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="darkgreen", color="white", lwd=2) +
  xlab("") +
  ylab("Frecuencia") +
  ggtitle("Cantidad de soluciones dominantes")
graphics.off()
#tp2<- Sys.time()
#tiempo.par <- tp2-tp1
#tiempos <- c(tiempos, tiempo.par)
porcen.sols.no.dom <- c(porcen.sols.no.dom,((dim(frente)[1]) *100)/200)
}
write.csv(porcen.sols.no.dom , file = "nodomin10.csv")
#write.csv(tiempos , file = "tiempoParalelo400.csv")
stopCluster(cluster)
write.csv(data , file = "datosviolin2fun.csv")

vio2 <-as.data.frame(read.csv("datosviolin2fun.csv"))
vio3 <-as.data.frame(read.csv("datosviolin3fun.csv"))

dom23 <- c(vio2$dom, vio3$dom)
pos23 <- c(rep("2",200), rep("3",200))
cant.fun <- c(rep(2,200),rep(3,200))
datos23 <- data.frame(pos23,dom23)
datos23$cant.fun <- as.factor(datos23$cant.fun)
library(ggplot2)
png("violin23.png")
gr <- ggplot(datos23, aes(x=pos23, y=dom23)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="darkgreen", color="white", lwd=2) +
  xlab("Cantidad de funciones") +
  ylab("Frecuencia") +theme(axis.text=element_text(size=15),axis.title=element_text(size=15,face="bold"))
graphics.off()
 # ggtitle("Cantidad de soluciones dominantes")
theme(axis.text=element_text(size=15),axis.title=element_text(size=15,face="bold"))
graphics.off()

vio4 <-as.data.frame(read.csv("datosviolin4fun.csv"))
vio5 <-as.data.frame(read.csv("datosviolin5fun.csv"))
vio6 <-as.data.frame(read.csv("datosviolin6fun.csv"))
vio7 <-as.data.frame(read.csv("datosviolin7fun.csv"))
vio8 <-as.data.frame(read.csv("datosviolin8fun.csv"))
vio9 <-as.data.frame(read.csv("datosviolin9fun.csv"))
domtodos <- c(vio2$dom, vio3$dom, vio4$dom, vio5$dom,vio6$dom, vio7$dom,vio8$dom,vio9$dom)
postodos <- c(rep("2",200),rep("3",200),rep("4",200), rep("5",200), rep("6",200),
           rep("7",200),rep("8",200),rep("9",200))
datostodos <- data.frame(postodos,domtodos)
library(ggplot2)
png(("violin2.png"), width = 800)
gr <- ggplot(datostodos, aes(x=postodos, y=domtodos)) + geom_violin(fill="orange", color="red", scale = "width")
gr + geom_boxplot(width=0.2, fill="darkgreen", color="white", lwd=2) +
  xlab("Cantidad de funciones") +
  ylab("Frecuencia") +theme(axis.text=element_text(size=15),axis.title=element_text(size=15,face="bold"))
graphics.off()
