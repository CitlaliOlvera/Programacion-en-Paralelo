library(testit)
library(parallel)

valor.calidad <- c()
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

  for (iter in 1:30) {
    p$obj <- NULL
    p$fact <- NULL
    p$probabilidad <- NULL
    
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
    
    suma <- sum(p$obj)
    probabilidad <- c()
    for(indice in 1:tam){
      pr <- (p$obj[indice]) / suma
      probabilidad <- c(probabilidad,pr)
    }
    p <- cbind(p, probabilidad)
    mantener <- sample(1:tam, init, replace=FALSE, prob = p$probabilidad)
    #mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
  }
  print(paste(mejor, (optimo - mejor) / optimo))
  
 val <- (optimo - mejor) / optimo
  valor.calidad <- c(valor.calidad,val)
  #calidad.solucion.sin.ruleta <- c(calidad.solucion.sin.ruleta, val)
  
}
stopCluster(cluster)
write.csv(valor.calidad, file = "r2Calidad.csv")

calidadSolucionR2 <- as.data.frame(read.csv("r2Calidad.csv"))
calidadSolucionR2SR <- as.data.frame(read.csv("r2CalidadSR.csv"))

datosboxR2 <- data.frame(calidadSolucionR2SR$x, calidadSolucionR2$x)
png("boxplotR2.png")
colnames(datosboxR2)<- c("sin ruleta", "con ruleta")
boxplot(datosboxR2, use.cols =FALSE, xlab= "tipo de seleccion de supervivencia",
        ylab = "calidad de la solucion", cex.lab = 1.5, cex.axis = 1.5)
graphics.off()

png("p10r2g.png")
plot(calidadSolucionR2SR$x, type = "o", col="red",pch=22, ylim =c(0.04,0.20) ,xlab = "replica", ylab = "calidad de la solucion")
lines(calidadSolucionR2$x, type="o", pch=1, col="blue")
#legend("topright",legend=c("Secuencial","Paralelo"),pch=c(2,1),col=c("red","blue"))
legend(0.3,   0.20,   c("Sin ruleta","Con ruleta"),   cex=1.1,   col=c("red",   "blue"),
       pch=22:1)
graphics.off()

datos2 <- c(calidadSolucionR2SR$x,calidadSolucionR2$x)
ruleta <- as.factor(c(rep(c("sin ruleta", "con ruleta"), each =20)))
shapiro.test(datos2)

png("p10normalesR2.png")
plot(density(datos2))
graphics.off()
qqnorm(datos2)
qqline(datos2, col = 2)

anva = aov( lm(datos2 ~ ruleta) )
summary(anva)

qf(0.05, 2-1, 20-2, lower.tail = F)
