library(parallel)
#library(doParallel)
library(testit) # para pruebas, recuerda instalar antes de usar
k <- 10000
n <- 1000000

##### valores normales ############
originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
assert(min(cumulos) > 0)
diferencia <- n - sum(cumulos)
if (diferencia > 0) {
  for (i in 1:diferencia) {
    p <- sample(1:k, 1)
    cumulos[p] <- cumulos[p] + 1
  }
} else if (diferencia < 0) {
  for (i in 1:-diferencia) {
    p <- sample(1:k, 1)
    if (cumulos[p] > 1) {
      cumulos[p] <- cumulos[p] - 1
    }
  }
}
#####################################
assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
assert(sum(cumulos) == n)
c <- median(cumulos) # tamanio critico de cumulos
d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva

############funcion union############
union <- function(x) {
  return (exp(-x / c))
}

unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}

faseunion <- function(i){
  urna <- freq[i,]
  return(unirse(urna$tam, urna$num))
}

for (i in 1:dim(freq)[1]) { # fase de union
  urna <- freq[i,]
  cumulos <- c(cumulos, unirse(urna$tam, urna$num))
}

#############funcion romperse #####################
rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}
  
  romperse <- function(tam, cuantos) {
    romper <- round(rotura(tam) * cuantos) # independientes
    resultado <- rep(tam, cuantos - romper) # los demas
    if (romper > 0) {
      for (cumulo in 1:romper) { # agregar las rotas
        t <- 1
        if (tam > 2) { # sample no jala con un solo valor
          t <- sample(1:(tam-1), 1)
        }
        resultado <- c(resultado, t, tam - t)
      }
    }
   
    assert(sum(resultado) == tam * cuantos) # no hubo perdidas
    return(resultado)
  }
  
faserotura <- function(i){
  urna <- freq[i,]
  if (urna$tam > 1) { # no tiene caso romper si no se puede
    #cumu <- c(romperse(urna$tam, urna$num))
    return(romperse(urna$tam, urna$num))
  } else {
    #cumu <- c( rep(1, urna$num))
    return(rep(1, urna$num))
  }
  #return(cumu)
}



###############################################

freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
duracion <- 5
digitos <- floor(log(duracion, 10)) + 1


cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "faserotura")
clusterExport(cluster, "romperse")
clusterExport(cluster, "rotura")
clusterExport(cluster, "freq")
clusterExport(cluster, "c")
clusterExport(cluster, "d")
clusterExport(cluster, "union")
clusterExport(cluster, "unirse")
clusterExport(cluster, "faseunion")
clusterExport(cluster, "assert")
###########Paralelo#################################
tp1 <- Sys.time()
for (paso in 1:duracion) {
  
  assert(sum(cumulos) == n)
  cumulos <- integer()
  clusterExport(cluster, "freq")
  cumulos <- parSapply(cluster, 1:dim(freq)[1], faserotura)
  cumulos <- unlist(cumulos)
  
  assert(sum(cumulos) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  freq <- as.data.frame(table(cumulos)) # actualizar urnas
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  cumulos <- integer()
 
  clusterExport(cluster, "freq")
  cumulos <- parSapply(cluster, 1:dim(freq)[1], faseunion)
  cumulos <- unlist(cumulos)
  
  assert(sum(abs(cumulos)) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  juntarse <- -cumulos[cumulos < 0]
  cumulos <- cumulos[cumulos > 0]
  assert(sum(cumulos) + sum(juntarse) == n)
  nt <- length(juntarse)
  if (nt > 0) {
    if (nt > 1) {
      juntarse <- sample(juntarse)
      for (i in 1:floor(nt / 2) ) {
        cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
      }
    }
    if (nt %% 2 == 1) {
      cumulos <- c(cumulos, juntarse[nt])
    }
  }
  assert(sum(cumulos) == n)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
#  tl <- paste(paso, "", sep="")
 # while (nchar(tl) < digitos) {
  #  tl <- paste("0", tl, sep="")
  #}
  #png(paste("miop8_ct", tl, ".png", sep=""), width=300, height=300)
  #tope <- 50 * ceiling(max(cumulos) / 50)
  #hist(cumulos, breaks=seq(0, tope, 50), 
   #    main=paste("Paso", paso, "con ambos fen\u{00f3}menos"), freq=FALSE,
    #   ylim=c(0, 0.05), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa")
  #graphics.off()
}
##########
#stopImplicitCluster()
stopCluster(cluster)
tp2 <- Sys.time()

