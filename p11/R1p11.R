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
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
#val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
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
png("r1p11_init.png")
plot(val[,1], val[,2], xlab=xl, ylab=yl)
graphics.off()
png("r1p11_mejores.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"))
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
png("r1p11_frente.png")
plot(val[,1], val[,2], xlab=paste(xl, " "),
     ylab=paste(yl," "), cex.lab=1.5)
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()

############diversificacion########
m.x <- c(val[mejor1,1], val[mejor2,1])
m.y <- c(val[mejor1,2], val[mejor2,2])
da <- data.frame(m.x,m.y)
xm <- (val[mejor1,1] + val[mejor2,1])/2
ym <- (val[mejor1,2] + val[mejor2,2])/2

xm14 <- (val[mejor1,1] + xm)/2
ym14 <- (val[mejor1,2] + ym)/2

xm34 <- (val[mejor2,1] + xm)/2
ym34 <- (val[mejor2,2] + ym)/2

di.a.m <- function(i){
  di <- sqrt((frente[i,1] - xm)^2 + (frente[i,2] - ym)^2)
}

vex.aux.minpm <- c()
for(ind in 1:length(frente[,1])){
  dism <- di.a.m(ind)
  vex.aux.minpm <- c(vex.aux.minpm, dism)
}
min.ptomedio <- which.min(vex.aux.minpm)

di.a.pto14 <- function(i){
  di <- sqrt((frente[i,1] - xm14)^2 + (frente[i,2] - ym14)^2)
}
vex.aux.minp14 <- c()
for(ind in 1:length(frente[,1])){
  dis14 <- di.a.pto14(ind)
  vex.aux.minp14 <- c(vex.aux.minp14, dis14)
}
min.pto14 <- which.min(vex.aux.minp14)

di.a.pto34 <- function(i){
  di <- sqrt((frente[i,1] - xm34)^2 + (frente[i,2] - ym34)^2)
}
vex.aux.minp34 <- c()
for(ind in 1:length(frente[,1])){
  dis34 <- di.a.pto34(ind)
  vex.aux.minp34 <- c(vex.aux.minp34, dis34)
}
min.pto34 <- which.min(vex.aux.minp34)
xs <- c(frente[min.ptomedio,1],frente[min.pto14,1], frente[min.pto34,1])
ys <- c(frente[min.ptomedio,2],frente[min.pto14,2], frente[min.pto34,2])
ptodiver <- data.frame(xs,ys)

png("r1p11_frenteDiver.png")
plot(val[,1], val[,2], xlab=paste(xl, " "),
     ylab=paste(yl," "),cex.lab=1.5)
lines(da, type="o", lty=2, col="red")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
points(xm, ym, col="red", pch=3, cex=1.5)
points(xm14, ym14, col="red", pch=3, cex=1.5)
points(xm34, ym34, col="red", pch=3, cex=1.5)
points(frente[min.ptomedio,1], frente[min.ptomedio,2], col="red", pch=16, cex=1.5)
points(frente[min.pto14,1], frente[min.pto14,2], col="red", pch=16, cex=1.5)
points(frente[min.pto34,1], frente[min.pto34,2], col="red", pch=16, cex=1.5)
points(val[mejor2, 1], val[mejor2, 2], col="red", pch=16, cex=1.5)
points(val[mejor1, 1], val[mejor1, 2], col="red", pch=16, cex=1.5)
#points(ptodiver[1], ptodiver[2], col="red", pch=16, cex=1.5)
graphics.off()

##########################################################

library(ggplot2) # recordar instalar si hace falta
data <- data.frame(pos=rep(0, n), dom=dominadores)
png("p11_violin.png")
gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="darkgreen", color="white", lwd=2) +
  xlab("") +
  ylab("Frecuencia") +
  ggtitle("Cantidad de soluciones dominantes")
graphics.off()

stopCluster(cluster)