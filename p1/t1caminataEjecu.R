t1caminata <- function(dim, dur) {
  pos <- rep(0, dim)
  mayor <- 0
  for (t in 1:dur) {
    cambiar <- sample(1:dim, 1)
    cambio <- 1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    pos[cambiar] <- pos[cambiar] + cambio
    
  }
 return(pos)
}


dime <- 1:5
pasos <- c(1000,2000,3000)
tiempoejecu <- data.frame(dime)

for(p in pasos){
teje <- c()
 for(w in dime){
  tejeaux <- c()
  for(r in 1:5){
    tejeaux <- c(tejeaux, system.time(t1caminata(w,100))[3])
  }
  teje <- c(teje, mean(tejeaux))
}
numpas <- rep(100,5)
tiempoejecu$numpasos <- numpas
tiempoejecu$tiempoejecucion <- teje

g<- paste0("tejec",toString(p),".png",collapse = '')
png(g)
plot(tiempoejecu$dime, tiempoejecu$tiempoejecucion, type = "overplotted",
     pch=1, col="red", xlab = "Dimension", ylab = "tiempo de ejecucion",
     main = "Dimension vs Tiempo de ejcucion ")
graphics.off()
}
