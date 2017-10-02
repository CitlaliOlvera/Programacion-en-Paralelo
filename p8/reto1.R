library(testit)
library(parallel)
library(doParallel)
valores <- c(1000,5000,10000,15000,20000,25000)
tiempos <- data.frame()

for(r in valores){
  k <- r
  n <- k * 30
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

  for (paso in 1:duracion) {
    tiempo <- proc.time()
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
  tp2 <- proc.time()
    tiempo  <- tiempo- tp2
    tiempos <- rbind(tiempos,tiempo[3])
  }



library(testit) # para pruebas, recuerda instalar antes de usar
valores <- c(1000,5000,10000,15000,20000,25000)
tiempos1 <- data.frame()
for(r in valores){
  k <- r
  n <- k * 30
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
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  assert(sum(cumulos) == n)
  c <- median(cumulos) # tamanio critico de cumulos
  d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
  rotura <- function(x) {
    return (1 / (1 + exp((c - x) / d)))
  }
  union <- function(x) {
    return (exp(-x / c))
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
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  duracion <- 50
  digitos <- floor(log(duracion, 10)) + 1
  
  for (paso in 1:duracion) {
    print(paste(r,"-",paso))
    tiempo1 <- proc.time()
    assert(sum(cumulos) == n)
    cumulos <- integer()
    for (i in 1:dim(freq)[1]) { # fase de rotura
      urna <- freq[i,]
      if (urna$tam > 1) { # no tiene caso romper si no se puede
        cumulos <- c(cumulos, romperse(urna$tam, urna$num))
      } else {
        cumulos <- c(cumulos, rep(1, urna$num))
      }
    }
    assert(sum(cumulos) == n)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    freq <- as.data.frame(table(cumulos)) # actualizar urnas
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    assert(sum(freq$num * freq$tam) == n)
    cumulos <- integer()
    for (i in 1:dim(freq)[1]) { # fase de union
      urna <- freq[i,]
      cumulos <- c(cumulos, unirse(urna$tam, urna$num))
    }
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
    # tl <- paste(paso, "", sep="")
    # while (nchar(tl) < digitos) {
    #  tl <- paste("0", tl, sep="")
    #}
    #png(paste("p8_ct", tl, ".png", sep=""), width=300, height=300)
    #tope <- 50 * ceiling(max(cumulos) / 50)
    #hist(cumulos, breaks=seq(0, tope, 50), 
    #    main=paste("Paso", paso, "con ambos fen\u{00f3}menos"), freq=FALSE,
    #   ylim=c(0, 0.05), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa")
    #graphics.off()
    tiempo1  <- proc.time()- tiempo1
    tiempos1 <- rbind(tiempos1,tiempo1[3])
  }
}

plot(tiempos1$X0.0130000000062864[251:300], type = "o", col="red",pch=2, ylim =c(1.22, 1.51) , xlab = "corrida", ylab = "tiempo")
lines(tiempos$X0.157000000006519[251:300], type="o", pch=1, col="blue")
legend("topright",legend=c("Secuencial","Paralelo"),pch=c(2,1),col=c("red","blue"))

plot(tiempos$X0.157000000006519[2:50], type = "o", col="green",pch=1, ylim = c(0.1,0.04), xlab = "corrida", ylab = "tiempo")

datosbox <- data.frame(tiempos$X0.157000000006519, tiempos1$X0.0130000000062864)
colnames(datosbox)<-c("paralelo", "secuencial")

bp1 <- c(datosbox$paralelo[1:50])
bp2 <- c(datosbox$paralelo[51:100])
bp3 <- c(datosbox$paralelo[101:150])
bp4 <- c(datosbox$paralelo[151:200])
bp5 <- c(datosbox$paralelo[201:250])
bp6 <- c(datosbox$paralelo[251:300])

datosPar <- data.frame(bp1, bp2, bp3, bp4, bp5, bp6)

png("paraleloP8.png")
colnames(datosPar)<- c(1,5,10,15,20,25)
boxplot(datosPar, use.cols=FALSE, 
        xlab="valor de k", ylab="tiempo", cex.lab = 1.5, cex.axis= 1.5)
graphics.off()

bs1 <- c(datosbox$secuencial[1:50])
bs2 <- c(datosbox$secuencial[51:100])
bs3 <- c(datosbox$secuencial[101:150])
bs4 <- c(datosbox$secuencial[151:200])
bs5 <- c(datosbox$secuencial[201:250])
bs6 <- c(datosbox$secuencial[251:300])

datosSec <- data.frame(bs1, bs2, bs3, bs4, bs5, bs6)

png("secuencialP8.png")
colnames(datosSec)<- c(1,5,10,15,20,25)
boxplot(datosSec, use.cols=FALSE, 
        xlab="valor de k", ylab="tiempo", cex.lab = 1.5, cex.axis= 1.5)
graphics.off()

ambos <- data.frame(bp1,bs1, bp2, bs2, bp3, bs3, bp4,bs4, bp5, bs5, bp6, bs6)
png("ambosP8.png")
colnames(ambos)<- c(1,1,5,5,10,10,15,15,20,20,25,25)
boxplot(ambos, use.cols=FALSE, 
        xlab="valor de k", ylab="tiempo", col=(c("blue","red")) ,cex.lab = 1.5, cex.axis= 1.5)
graphics.off()

var.test(datosbox$paralelo,datosbox$secuencial); t.test(datosbox$paralelo, datosbox$secuencial, alternative="greater")
wilcox.test(datosbox$paralelo, datosbox$secuencial, paired = TRUE, alternative = "greater")

plot(density(datos)) # lo generado que era normal
print(shapiro.test(datosbox$paralelo))
qqnorm(datosbox$paralelo)
qqline(datosbox$paralelo, col = 2)
datos <- datosbox$paralelo