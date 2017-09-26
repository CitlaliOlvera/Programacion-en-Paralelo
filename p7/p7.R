g <- function(x, y) {
  a<- (((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
  return(a)
}

low <- -5
high <- 4
step <- 0.01
replicas <- 100
replica <- function(t){
  curr <- c(runif(1, low, high), runif(1, low, high))
  best <- curr
  for (tiempo in 1:t) {
    delta <- runif(1, 0, step)
    x1 <- curr + c(-delta,0)
    x2 <- curr + c(delta,0)
    y1 <- curr + c(0,-delta)
    y2 <- curr + c(0,delta)
    puntos <- c(x1,x2,y1,y2)
    for(k in 1:8){
      if(puntos[k] < (-5)){
        puntos[k] <- puntos[k]+10 
      }
      if(puntos[k] > 5){
        puntos[k] <- puntos[k]-10
      }
    }
    vecx <- c()
    vecy <- c()
    for(p in 1:8){
      if(p %% 2 == 0){
        vecy <- c(vecy,puntos[p])
      }else{
        vecx <- c(vecx,puntos[p])
      }
    }
    valg <- c()
    for(q in 1:4){
      valg <- c(valg, g(vecx[q], vecy[q]) )
    }
    dm <- which.max(valg)
    curr <- c(vecx[dm], vecy[dm])
    if(g(curr[1],curr[2]) > g(best[1],best[2])){
      best <- curr
    }
  }
  return(best)
}

####pensar aqui######
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))


for (pot in 2:4) {
  tmax <- 10^pot
  resultados <- foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax)
  
  vecx <- c()
  vecy <- c()
  aux <- (2*replicas)
  for(p in 1:aux){
    if(p %% 2 == 0){
      vecy <- c(vecy,resultados[p])
    }else{
      vecx <- c(vecx,resultados[p])
    }
  }
  
  valores <- c()
  for(q in 1:replicas){
    valores <- c(valores, g(vecx[q], vecy[q]))
  }
  mejor <- which.max(valores)
  #graficas
  x <- seq(-6, 5, 0.25)
  y <-  x
  z <- outer(x, y, g)
  #z <- outer(vecx,vecy,g)
  dimnames(z) <- list(x, y)
  library(reshape2) # recuerda instalar paquetes antes de intentar su uso
  d <- melt(z)
  names(d) <- c("x", "y", "z")
  library(lattice) # lo mismo aplica con este paquete
  png(paste0("p7", tmax, ".png", sep=""), width=500, height=500)
  plot(levelplot(z ~ x * y, data = d))
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vecx, vecy, pch=1, col="blue", cex=1)
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1, highlight=FALSE)
  lpoints(vecx[mejor], vecy[mejor], pch=19, col="red", cex=1)
  trellis.unfocus()
  
  graphics.off() 
}
stopImplicitCluster()