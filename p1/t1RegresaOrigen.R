crep <- seq(100,550,150)
for(i in 1:4){
repetir <- crep[i]
duracion <- 2000

library(parallel)
datos <- data.frame()

experimento <- function(replica) {
  pos <- rep(0, dimension)
  numregreso <- 0
  for (t in 1:duracion) {
    cambiar <- sample(1:dimension, 1)
    cambio <- 1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    pos[cambiar] <- pos[cambiar] + cambio
    auxiliar <- 0
    for(i in 1:dimension){
      
      if(pos[i]==0){
        auxiliar <- auxiliar +1
      }
    }
    if(auxiliar==dimension){
     
      numregreso <- numregreso + 1
    }
  }
  
  return(numregreso)
  }

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
clusterExport(cluster, "experimento")

for (dimension in 1:8) {
  clusterExport(cluster, "dimension")
  resultado <- parSapply(cluster, 1:repetir, experimento)
  datos <- rbind(datos, resultado)
}
stopCluster(cluster)
 
  y<- paste0("t1regresa",toString(i),".png",collapse = '')
  png(y)
  boxplot(data.matrix(datos), use.cols=FALSE,
          xlab="Dimensi\u{F3}n", ylab="vuelve Origen", main="Regresa al origen")

graphics.off()
}