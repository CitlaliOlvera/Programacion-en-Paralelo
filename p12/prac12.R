library(parallel)
binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- 0.002
modelos[modelos=='g'] <- 0.002
modelos[modelos=='b'] <- 0.002

r <- 5
c <- 3
dim <- r * c

tasa <- 0.15
tranqui <- 0.99

tope <- 9
digitos <- 0:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}

atino <- function(i){
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
    if(correcto[i] != salida[i]){
      return(c(d,0))
    }
  }
return(c(decimal(salida,n),1))
}

cluster <- makeCluster(detectCores() - 1)
#cuantos <- c(100,300,500,1000)
porcentajesTodBajos <- c()
for(i in 1:10){
paralelo <- c()
#for(i in cuantos){
#for(j in 1:25){
#tp1 <- Sys.time()
clusterExport(cluster, "atino")
clusterExport(cluster, "modelos")
clusterExport(cluster, "binario")
clusterExport(cluster, "decimal")
clusterExport(cluster, "neuronas")
clusterExport(cluster, "tope")
clusterExport(cluster, "dim")
clusterExport(cluster, "n")
clusterExport(cluster, "cuantos")
vec.auxiliar <- parSapply(cluster, 1:300, atino)
vec.auxiliar <- t(vec.auxiliar)
#tp2 <- Sys.time()
porcentaje.correcto <- ((sum(vec.auxiliar[,2]) * 100)/300)
porcentajesTodBajos <- c(porcentajesTodBajos, porcentaje.correcto)
}
#tiempo.paralelo <- tp2 - tp1
#paralelo <- c(paralelo, tiempo.paralelo)
#}
#}
stopCluster(cluster)
porcen <- data.frame(porcentajes,porcentajesBalto, porcentajesIntNyB, porcentajesIntNyG, porcentajesTodAltos)
write.csv(porcen , file = "porcentajes.csv")
por <- c(porcentajes, porcentajesBalto, porcentajesIntNyB, porcentajesIntNyG, porcentajesTodAltos)
probabilidades <- c(rep("A",10), rep("B", 10), rep("C",10), rep("D", 10), rep("E", 10))
datos <- data.frame(probabilidades, por)
library(ggplot2)
ggplot(datos, aes(x = probabilidades, y = por)) +
  geom_boxplot() + scale_x_discrete(name = "Probabilidades") + 
  scale_y_discrete(name = "Porcentaje de aciertos")  
p <- ggplot(datos, aes(x = probabilidades, y = por)) +
  geom_boxplot() + scale_x_discrete(name = "Probabilidades") + 
  ylab("Porcentaje de aciertos")
  #scale_y_discrete(name = "Porcentaje de aciertos") 
p <- p + geom_jitter()
png("porcentajes.png")
p
graphics.off()
#tiem.paralelo <-as.data.frame(read.csv("t_paralelo.csv"))
#tiem.secuencial <-as.data.frame(read.csv("t_secuencial.csv"))
#tiempo <- c(tiem.secuencial$x, tiem.paralelo$x)
#tipo <- c(rep("Secuencial",100), rep("Paralelo",100))
#digitos <- c(rep(100,25),rep(300,25),rep(500,25),rep(1000,25),rep(100,25),rep(300,25),rep(500,25),rep(1000,25))
print(porcentaje.correcto)