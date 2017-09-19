library(parallel)
l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
pv <- 0.1
v <- l / 30
agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character())
for (i in 1:n) {
  e <- "S"
  if (runif(1) < pv) {
    e <- "R"
  }
  agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                       dx = runif(1, -v, v), dy = runif(1, -v, v),
                                       estado = e))
  levels(agentes$estado) <- c("S", "I", "R")
}

for(i in 1:n){
  vac <- agentes[i, ]
  if(vac$estado == "S"){
    if(runif(1)< pi){
      vac$estado <- "I"
      agentes[i, ] <- vac
    }
  }
}


epidemia <- integer()
r <- 0.1
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1

contagios <- rep(FALSE,n)
quiencontagio <- c()
for(d in 1:n){
  w <- agentes[d, ]
  if(w$estado == "I"){
    contagios[d]<- TRUE
    quiencontagio <- c(quiencontagio, d)
  }
}
###fun contagios
contagiado <- function(i){
  a1 <- agentes[i, ]
  if(a1$estado == "S"){
    print("es susceptible")
    for(k in quiencontagio){
      a2 <- agentes[k, ]
      dx <- a1$x - a2$x
      dy <- a1$y - a2$y
      d <- sqrt(dx^2 + dy^2)
      if(d < r){
        print("tiene distancia menor que el umbral")
        p <- (r-d)/r
        if(runif(1) < p){
          print("el va a cambiar el estado")
          return("I")
        }
      }
    }
    print("no entro en la probabilidad se queda con su estado")
    return("S")
  }
  if(a1$estado == "I"){
    print("ya estaba infectado")
    return("I")
  }
  if(a1$estado == "R"){
    print("Estaba recuperado")
    return("R")
  }
}

#### fun recuperado
recuperado <- function(z){
  a <- agentes[z, ]
  if(z %in% quiencontagio){
    print("esta infectado")
    if(runif(1) < pr){
      print("paso la probabilidad")
      return("R")
    }else{
      print("no paso la probabilidad")
      return("I")}
  }
  if(a$estado == "R"){
    print("ya esta recuperado")
    return("R")
  }
  if(a$estado == "S"){
    print("no esta infectado")
    return("S")
  }
}
### funciones de movimientos
movimientosx <- function(k){
  ax <- agentes[k, ]
  val.x <- ax$x + ax$dx
  print("valor de x actualizado es ")
  print(val.x)
  if(val.x > l){
    print("es mayor que l")
    print(val.x - l)
    return(val.x - l)
  }
  if(val.x < 0){
    print(" es menos que cero")
    print(val.x +l)
    return(val.x + l)
  }
  print("se queda con el valor actualizado")
  return(val.x)
}

movimientosy <- function(k){
  ay <- agentes[k, ]
  val.y <- ay$y + ay$dy
  print("valor de y actualizado es ")
  print(val.y)
  if(val.y > l){
    print("es mayor que l")
    print(val.y - l)
    return(val.y - l)
  }
  if(val.y < 0){
    print(" es menos que cero")
    print(val.y +l)
    return(val.y + l)
  }
  print("se queda con el valor actualizado")
  return(val.y)
}

#######
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster, "contagiado")
clusterExport(cluster, "n")
clusterExport(cluster, "agentes")
clusterExport(cluster, "r")
clusterExport(cluster, "recuperado")
clusterExport(cluster, "pr")
clusterExport(cluster, "movimientosx")
clusterExport(cluster, "l")
clusterExport(cluster, "movimientosy")
  for (tiempo in 1:tmax) {
    infectados <- dim(agentes[agentes$estado == "I",])[1]
    epidemia <- c(epidemia, infectados)
    if (infectados == 0) {
      break
    }
    contagios <- rep(FALSE,n)
    quiencontagio <- c()
    for(d in 1:n){
      w <- agentes[d, ]
      if(w$estado == "I"){
        contagios[d]<- TRUE
        quiencontagio <- c(quiencontagio, d)
      }
    }
    #contagio
    clusterExport(cluster, "quiencontagio")
    vecnvoscontagios <- parSapply(cluster, 1:n, contagiado)
    
    #recuperado
    vecrecuperados <- parSapply(cluster, 1:n, recuperado)
    
    #los dos for sigguientes relacionan los dos vectores anteriores
    quienrecupera <- c()
    for(sss in 1:n){
      if(vecrecuperados[sss] == "R"){
        quienrecupera <- c(quienrecupera, sss)
      }
    }
    
    for(q in quienrecupera){
      vecnvoscontagios[q] <- "R"
    }
    
    # movimientos x
    nvosx <- parSapply(cluster, 1:n, movimientosx)
    
    #movimientos y
    nvosy <- parSapply(cluster, 1:n, movimientosy)
    
    agentes$estado <- vecnvoscontagios
    agentes$x <- nvosx
    agentes$y <- nvosy
    
    aS <- agentes[agentes$estado == "S",]
    aI <- agentes[agentes$estado == "I",]
    aR <- agentes[agentes$estado == "R",]
    tl <- paste(tiempo, "", sep="")
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    #graficas
     salida <- paste("wwwP6_t", tl, ".png", sep="")
      tiempo <- paste("Paso", tiempo)
     png(salida)
      plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
     if (dim(aS)[1] > 0) {
      points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
      }
     if (dim(aI)[1] > 0) {
      points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
    }
    if (dim(aR)[1] > 0) {
     points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
    }
    graphics.off()
  }
stopCluster(cluster)
  ######grafica epidemia
  png("R1p6e.png", width=600, height=300)
  plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentaje de infectados")
  graphics.off()