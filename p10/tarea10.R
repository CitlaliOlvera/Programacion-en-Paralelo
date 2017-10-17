library(testit)
library(parallel)
valor.calidad1 <- c()
for(indi in 1:20){
knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}
################# funcion mutacion #################
fun.mutar <- function(i){
  if (runif(1) < pm) {
    solm <- mutacion(p[i,], n)
   # pp <- rbind(pp, mutacion(pp[i,], nn))
    return(solm)
  }
  return(p[i,])
}
#################################################### 

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

#########funcion reproduccion ##############
fun.reproduccion <- function(i){
  padres <- sample(1:tam, 2, replace=FALSE)
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  return(hijos)
}
###########################################

#############funcion calcula objetivo y verifica factibilidad #####
val.objetivo <- function(i){
  vobj <- objetivo(p[i,], valores)
  return(vobj)
}

fun.factibilidad <- function(i){
  esfact <- factible(p[i,], pesos, capacidad)
  return(esfact)
}
###################################################################



n <- 50
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)
optimo <- knapsack(capacidad, pesos, valores)
init <- 200
p <- poblacion.inicial(n, init)
tam <- dim(p)[1]
assert(tam == init)
pm <- 0.05
rep <- 50
tmax <- 50
mejores <- double()

cluster <- makeCluster(detectCores() - 1)
tiempos20 <- c()
tiempos50 <- c()
tiempos75 <- c()

calidad.solucion.sin.ruleta <- c()
#for(indi in 1:30){
t1 <- Sys.time()
#for (iter in 1:tmax) {
for (iter in 1:30) {
  p$obj <- NULL
  p$fact <- NULL
  
# ////////////fase mutacion////////// 

  clusterExport(cluster, "pm")
  clusterExport(cluster, "p")
  clusterExport(cluster, "mutacion")
  clusterExport(cluster, "n")
  clusterExport(cluster, "tam")
  clusterExport(cluster, "fun.mutar")
  
  
  vec.mutados <- parSapply(cluster, 1:tam, fun.mutar)
  vec.mutados <- unlist(vec.mutados)
  for(auxiliar in 1:tam){
    p[auxiliar, ]<- c(vec.mutados[(50 * (auxiliar-1))+1 : (auxiliar * 50)])
  }
  
  #### fase reproduce #########

  clusterExport(cluster, "n")
  clusterExport(cluster, "tam")
  clusterExport(cluster, "rep")
  clusterExport(cluster, "p")
  clusterExport(cluster, "reproduccion")
  clusterExport(cluster, "fun.reproduccion")
  
  vec.hijos <-  parSapply(cluster, 1:rep, fun.reproduccion)
  vec.hijos <- unlist(vec.hijos)
  for(auxiliar in 1:rep){
    p <- rbind(p,vec.hijos[(50 * (auxiliar-1))+1 : (auxiliar * 50)])
  }
  
  ###### factibilidad y valor objetivo ############
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  
  clusterExport(cluster, "val.objetivo")
  clusterExport(cluster, "fun.factibilidad")
  clusterExport(cluster, "tam")
  clusterExport(cluster, "obj")
  clusterExport(cluster, "fact")
  clusterExport(cluster, "objetivo")
  clusterExport(cluster, "valores")
  clusterExport(cluster, "p")
  clusterExport(cluster, "factible")
  clusterExport(cluster, "pesos")
  clusterExport(cluster, "capacidad")
 
  obj <- parSapply(cluster, 1:tam, val.objetivo)
  fact <- parSapply(cluster, 1:tam, fun.factibilidad)
  
  p <- cbind(p, obj)
  p <- cbind(p, fact)
  
  mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
  p <- p[mantener,]
  tam <- dim(p)[1]
  assert(tam == init)
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
}
t2 <- Sys.time()
print(paste(mejor, (optimo - mejor) / optimo))
#}
val <- (optimo - mejor) / optimo

valor.calidad1 <- c(valor.calidad1, val)

tiempo.paralelo <- t2-t1
tiempos20 <- c(tiempos20, 20 * tiempo.paralelo)
tiempos50 <- c(tiempos50, 50 * tiempo.paralelo)
tiempos75 <- c(tiempos75, 75 * tiempo.paralelo)
}
stopCluster(cluster)
write.csv(valor.calidad1, file = "r2CalidadSR.csv")

write.csv(calidad.solucion.sin.ruleta, file = "CalidadSolucionSinRuleta.csv")

tiempoparalelo <- data.frame(tiempos20, tiempos50, tiempos75)
write.csv(tiempoparalelo, file = "tiempoParalelotmax.csv")

tpar <- as.data.frame(read.csv("tiempoParalelotmax.csv"))

datosboxtp <- data.frame(tpar$tiempos20, tpar$tiempos50, tpar$tiempos75)
png("p10tp.png")
colnames(datosboxtp)<- c(20,50,75)
boxplot(datosboxtp, use.cols =FALSE, xlab= "generaciones", ylab = "tiempo", cex.lab = 1.5, cex.axis = 1.5)
graphics.off()

tsec <- as.data.frame(read.csv("tiempoSecuencial.csv"))

datosboxts <- data.frame(tsec$tiemposS20, tsec$tiemposS50, tsec$tiemposS75)
png("p10ts.png")
colnames(datosboxts)<- c(20,50,75)
boxplot(datosboxts, use.cols =FALSE, xlab= "generaciones", ylab = "tiempo", cex.lab = 1.5, cex.axis = 1.5)
graphics.off()

png("p10t20.png")
plot(tsec$tiemposS20, type = "o", col="red",pch=22, ylim =c(15,60) ,xlab = "generaciones", ylab = "tiempo")
lines(tpar$tiempos20, type="o", pch=1, col="blue")
#legend("topright",legend=c("Secuencial","Paralelo"),pch=c(2,1),col=c("red","blue"))
legend(0.5,   60,   c("Secuencial","Paralelo"),   cex=1.1,   col=c("red",   "blue"),
       pch=22:1)
graphics.off()

png("p10t50.png")
plot(tsec$tiemposS50, type = "o", col="red",pch=22, ylim =c(40,150) ,xlab = "generaciones", ylab = "tiempo")
lines(tpar$tiempos50, type="o", pch=1, col="blue")
#legend("topright",legend=c("Secuencial","Paralelo"),pch=c(2,1),col=c("red","blue"))
legend(0.5,   150,   c("Secuencial","Paralelo"),   cex=1.1,   col=c("red",   "blue"),
       pch=22:1)
graphics.off()

png("p10t75.png")
plot(tsec$tiemposS75, type = "o", col="red",pch=22, ylim =c(70,220) ,xlab = "generaciones", ylab = "tiempo")
lines(tpar$tiempos75, type="o", pch=1, col="blue")
#legend("topright",legend=c("Secuencial","Paralelo"),pch=c(2,1),col=c("red","blue"))
legend(0.5,   220,   c("Secuencial","Paralelo"),   cex=1.1,   col=c("red",   "blue"),
       pch=22:1)
graphics.off()

plot(density(tsec$tiemposS20)) # lo generado que era normal
print(shapiro.test(tsec$tiemposS20))
qqnorm(tsec$tiemposS20)
qqline(tsec$tiemposS20, col = 2)

plot(density(tpar$tiempos20)) # lo generado que era normal
print(shapiro.test(tpar$tiempos20))
qqnorm(tpar$tiempos20)
qqline(tpar$tiempos20, col = 2)

datos <- c(tsec$tiemposS20, tpar$tiempos20)
sec.par <- as.factor(c(rep(c("secuencial","paralelo"), each=30)))
png("p10normales.png")
shapiro.test(datos)
plot(density(datos)) # lo generado que era normal
print(shapiro.test(datos))
qqnorm(datos)
qqline(datos, col = 2)
graphics.off()

datos.kruskal <- data.frame(secuencial= a1 <-c(tsec$tiemposS20),paralelo= a2<-c( tpar$tiempos20))
kruskal.test(secuencial~paralelo, data = datos.kruskal)
