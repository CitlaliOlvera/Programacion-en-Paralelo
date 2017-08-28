suppressMessages(library(doParallel))
resultados1 <- matrix()
resultados2 <- matrix()
resultados3 <- matrix()
resultados4 <- matrix()
resultados5 <- matrix()

primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if (n!= i &&(n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

desde <- 1
hasta <-  10000

pri<- c()
nopri <- c()

for(r in desde:hasta){
  if(primo(r)== TRUE){
    pri <- c(pri, r)
  }else{
    nopri <- c(nopri,r)
  }
}


original <- desde:hasta
invertido <- hasta:desde
primospri <- c(pri,nopri)
primosdes <- c(nopri,pri)

replicas <- 8
nucleos <- detectCores() - 1


for(n in 1:nucleos){
  datos1 <- data.frame()
  datos2 <- data.frame()
  datos3 <- data.frame()
  datos4 <- data.frame()
  datos5 <- data.frame()
  
  registerDoParallel(makeCluster(n))
  ot <-  numeric()
  it <-  numeric()
  at <-  numeric()
  m1 <-  numeric()
  m2 <-  numeric()
  
  print(paste("Tiempos con", n, "nucleos"))
  for (r in 1:replicas) {
    print(paste("replica", r))
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # aleatorio
    m1 <- c(m1, system.time(foreach(n = primospri, .combine=c) %dopar% primo(n))[3]) # primos al inicio
    m2 <- c(m2, system.time(foreach(n = primosdes, .combine=c) %dopar% primo(n))[3]) # primos al final
    
    datos1 <- rbind(datos1, ot[length(ot)-1])
    datos2 <- rbind(datos2, it[length(it)-1])
    datos3 <- rbind(datos3, at[length(at)-1])
    datos4 <- rbind(datos4, m1[length(m1)-1])
    datos5 <- rbind(datos5, m2[length(m2)-1])
  
  }

  resultados1 <- data.frame(cbind(resultados1,datos1))
  resultados2 <- data.frame(cbind(resultados2,datos2))
  resultados3 <- data.frame(cbind(resultados3,datos3))
  resultados4 <- data.frame(cbind(resultados4,datos4))
  resultados5 <- data.frame(cbind(resultados5,datos5))

}
resultados1 <- resultados1[,-1]
resultados2 <- resultados2[,-1]
resultados3 <- resultados3[,-1]
resultados4 <- resultados4[,-1]
resultados5 <- resultados5[,-1]


colnames(resultados1) <- c(1:nucleos)
colnames(resultados2) <- c(1:nucleos)
colnames(resultados3) <- c(1:nucleos)
colnames(resultados4) <- c(1:nucleos)
colnames(resultados5) <- c(1:nucleos)


png("resultados1.png")
boxplot(resultados1, use.cols =FALSE, main="Orden Ascendente", xlab= "Cantidad de Nucleos", ylab = "Tiempo")

png("resultados2.png")
boxplot(resultados2, use.cols =FALSE, main="Orden Descendente", xlab= "Cantidad de Nucleos", ylab = "Tiempo")

png("resultados3.png")
boxplot(resultados3, use.cols =FALSE, main="Aleatorio", xlab= "Cantidad de Nucleos", ylab = "Tiempo")

png("resultados4.png")
boxplot(resultados4, use.cols =FALSE, main="Primos primero", xlab= "Cantidad de Nucleos", ylab = "Tiempo")

png("resultados5.png")
boxplot(resultados5, use.cols =FALSE, main="Primos al final", xlab= "Cantidad de Nucleos", ylab = "Tiempo")
graphics.off()

d1<- c()
d2 <- c()
d3 <- c()
d4 <- c()
d5 <- c()

for(i in 1:nucleos){ 
  d1<- c(d1,resultados1[,i])
}

for(ii in 1:nucleos){ 
  d2<- c(d2,resultados2[,ii])
}

for(iii in 1:nucleos){ 
  d3<- c(d3,resultados3[,iii])
}

for(rr in 1:nucleos){ 
  d4<- c(d4,resultados4[,rr])
}

for(g in 1:nucleos){ 
  d5<- c(d5,resultados5[,g])
}
dp <- c(d1,d2,d3,d4,d5)

tipoorden <- c(rep(1,7*nucleos), rep(2,7*nucleos), rep(3,7*nucleos), rep(4,7*nucleos), rep(5,7*nucleos))
naux <- c(rep(1,7), rep(2,7), rep(3,7))
cantnucleos <- c(rep(naux,5))
datosPrueba <- data.frame(tipoorden, cantnucleos, dp)

kruskal.test(datosPrueba[,3]~ tipoorden)
kruskal.test(datosPrueba[,3]~ cantnucleos)