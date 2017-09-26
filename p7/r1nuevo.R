g <- function(x, y) {
  a<- (((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
  return(a)
}

low <- -5
high <- 4
step <- 0.1
replicas <- 100

replica <- function(t){
  puntosxy<- c()
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
   puntosxy <- c(puntosxy, vecx[dm],vecy[dm])
  }
  return(puntosxy)
}

resultado <- c()
for(q in 1:5){
resultado <- c(resultado, replica(100))
}

vx <- c()
vy <- c()
for(p in 1:1000){
  if(p %% 2 == 0){
    vy <- c(vy,resultado[p])
  }else{
    vx <- c(vx,resultado[p])
  }
}

vx1 <- c(vx[1:100])
vx2 <- c(vx[101:200])
vx3 <- c(vx[201:300])
vx4 <- c(vx[301:400])
vx5 <- c(vx[401:500])

vy1 <- c(vy[1:100])
vy2 <- c(vy[101:200])
vy3 <- c(vy[201:300])
vy4 <- c(vy[301:400])
vy5 <- c(vy[401:500])


for(j in 1:100){
x <- seq(-6, 5, 0.25)
y <-  x
z <- outer(x, y, g)
#z <- outer(vecx,vecy,g)
dimnames(z) <- list(x, y)
library(reshape2) # recuerda instalar paquetes antes de intentar su uso
d <- melt(z)
names(d) <- c("x", "y", "z")
library(lattice) # lo mismo aplica con este paquete
if(j < 10){
nombre <-  paste0("r1nvo00", j, ".png", sep="")
}else if(j>= 10 & j < 100){ 
  nombre <-  paste0("r1nvo0", j, ".png", sep="") }else{
    nombre <-  paste0("r1nvo", j, ".png", sep="")
  }
png(nombre, width=500, height=500)
plot(levelplot(z ~ x * y, data = d))
trellis.focus("panel", 1, 1, highlight=FALSE)
lpoints(vx1[j], vy1[j], pch=19, col="blue", cex=1)
trellis.unfocus()
trellis.focus("panel"[1], 1, 1, highlight=FALSE)
lpoints(vx2[j], vy2[j], pch=19, col="red", cex=1)
trellis.unfocus()
trellis.focus("panel"[1], 1, 1, highlight=FALSE)
lpoints(vx3[j], vy3[j], pch=19, col="green", cex=1)
trellis.unfocus()
trellis.focus("panel"[1], 1, 1, highlight=FALSE)
lpoints(vx4[j], vy4[j], pch=19, col="orange", cex=1)
trellis.unfocus()
trellis.focus("panel"[1], 1, 1, highlight=FALSE)
lpoints(vx5[j], vy5[j], pch=19, col="yellow", cex=1)
trellis.unfocus()
graphics.off() 
}