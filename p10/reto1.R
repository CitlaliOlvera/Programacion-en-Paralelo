library(testit)
library(parallel)

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
  padres <- sample(1:tam, 2, replace=FALSE, prob = p$probabilidad)
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
t1 <- Sys.time()
#for (iter in 1:tmax) {
calidad.solucion <- c()
for(auxi in 1:30){
for (iter in 1:1) {
  p$obj <- NULL
  p$fact <- NULL
  p$aux.obj <- NULL
  p$probabilidad <- NULL
  
# ///////////fase mutacion////////// 

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
  
  #############creando vector de probabilidades#####
  tam <- dim(p)[1]
  aux.obj <- double()
  
  clusterExport(cluster, "val.objetivo")
    clusterExport(cluster, "tam")
  clusterExport(cluster, "aux.obj")
    clusterExport(cluster, "objetivo")
  clusterExport(cluster, "valores")
  clusterExport(cluster, "p")
   clusterExport(cluster, "pesos")
  clusterExport(cluster, "capacidad")
   
  aux.obj <- parSapply(cluster, 1:tam, val.objetivo)
   p <- cbind(p, aux.obj)
   
   suma <- sum(p$aux.obj)
   
   probabilidad <- c()
   for(indice in 1:tam){
     pr <- (p$aux.obj[indice]) / suma
     probabilidad <- c(probabilidad,pr)
   }
   p <- cbind(p, probabilidad)
  
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
#png("p10.png", width=600, height=300)
#plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
#points(1:tmax, mejores, pch=15)
#abline(h=optimo, col="green", lwd=3)
#graphics.off()
print(paste(mejor, (optimo - mejor) / optimo))
valor <- (optimo - mejor) / optimo
calidad.solucion <- c(calidad.solucion, valor)
}
write.csv(calidad.solucion, file = "calidadSolucionRuleta.csv")
stopCluster(cluster)

calidadSolucion <- as.data.frame(read.csv("calidadSolucionRuleta.csv"))
calidadSolucionSR <- as.data.frame(read.csv("CalidadSolucionSinRuleta.csv"))

datosbox <- data.frame(calidadSolucionSR$x, calidadSolucion$x)
png("p10R1.png")
colnames(datosbox)<- c("sin ruleta", "con ruleta")
boxplot(datosbox, use.cols =FALSE, xlab= "tipo de seleccion", ylab = "calidad de la solucion", cex.lab = 1.5, cex.axis = 1.5)
graphics.off()

png("p10r1g.png")
plot(calidadSolucionSR$x, type = "o", col="red",pch=22, ylim =c(0.04,0.20) ,xlab = "replica", ylab = "calidad de la solucion")
lines(calidadSolucion$x, type="o", pch=1, col="blue")
#legend("topright",legend=c("Secuencial","Paralelo"),pch=c(2,1),col=c("red","blue"))
legend(0,   0.20,   c("Sin ruleta","Con ruleta"),   cex=1.1,   col=c("red",   "blue"),
       pch=22:1)
graphics.off()

datos1 <- c(calidadSolucionSR$x,calidadSolucion$x)
shapiro.test(datos1)

datos.kruskalR1 <- data.frame(sin.ruleta= a1 <-c(calidadSolucionSR$x),con.ruleta= a2<-c( calidadSolucion$x))
kruskal.test(sin.ruleta~con.ruleta, data = datos.kruskalR1)
