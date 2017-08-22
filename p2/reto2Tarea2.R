library(parallel)
dim <- 20
num <-  dim^2
nucleos <- 20

actual <- matrix(rep(0,num), nrow=dim, ncol=dim)
al <- sample(1:num,nucleos)
al2 <- sample(1:nucleos)
for(y in 1:nucleos){
  actual[al[y]]=al2[y]
}
suppressMessages(library("sna"))
png("Reto22p2_t0P.png")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
graphics.off()

colores <- c(0)

for(o in 1:num){
  if(actual[o] != 0 && actual[o] %in% colores == FALSE){
    colores <- c(colores, actual[o])
  }
}

print(colores)

paso <- function(pos){
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  
  
  
  if(actual[fila, columna] == 0){	
    if(runif(1)<=0.4){
      
      vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                          max(columna - 1, 1): min(columna + 1, dim)]
      
      if((sum(vecindad)) > 0)
      {
        vecaux <- c()
        for(g in 1:length(vecindad)){
          if(vecindad[g] != 0 ){
            vecaux <- c(vecaux, vecindad[g])
          }
        }
        print(vecaux)
        
        valor <- vecaux[1]
        return(valor)
      }
      else
      {
        return(actual[fila,columna])
      }
    }
    else{return(actual[fila,columna])}
  }
  
  else {
    return(actual[fila,columna])
  }
}


cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for (iteracion in 1:50) {
  
  clusterExport(cluster, "actual")
  siguiente <- parSapply(cluster, 1:num, paso)
  print(siguiente)
  msj <- paste("Iteracion:", iteracion)
  print(msj)
  
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  posiceros <- c()
  for(e in 1:num){
    if(actual[e]==0){
      posiceros <- c(posiceros,e)
      }
  }
  if(length(posiceros)>1){
  posnuevonucleo <- sample(posiceros,1)
  for(x in 1:num){
    if(x %in% colores == FALSE){
      colores <- c(colores,x)
      nc <- x
      break;
    }
  }
  actual[posnuevonucleo]=x
  }
  if(length(posiceros)==1){
    for(xx in 1:num){
      if(xx %in% colores == FALSE){
        colores <- c(colores,xx)
        nc <- xx
        break;
      }
    }
    actual[posiceros]=xx
  }
  
  salida = paste("Reto22p2_tP", iteracion, ".png", sep="")
  tiempo = paste("Paso", iteracion)
  png(salida)
  plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
  graphics.off()
  
  if (all(actual != 0)) { # todos murieron
    print("Termina propagacion")
    break;
  }
}
stopCluster(cluster)

tabla <- table(actual)

coloresborde <- c()
for(f in 1:dim){
  if(actual[1,f] %in% coloresborde == FALSE){
    coloresborde <- c(coloresborde, actual[1,f])
  }
}
for(ff in 1:dim){
  if(actual[dim, ff] %in% coloresborde == FALSE){
    coloresborde <- c(coloresborde, actual[dim, ff])
  }
}
for(c in 1:dim){
  if(actual[c, 1] %in% coloresborde == FALSE){
    coloresborde <- c(coloresborde, actual[c,1])
  }
}
for(cc in 1:dim){
  if(actual[cc, dim] %in% coloresborde == FALSE){
    coloresborde <- c(coloresborde, actual[cc,dim])
  }
}

coloresnoborde <- c()
for(r in 1:length(tabla)){
  if(r %in% coloresborde==FALSE){
    coloresnoborde <- c(coloresnoborde,r)
  }
}
#for(s in 2:dim-1){
 # for(l in 2:dim-1){
  #  if(actual[s,l] %in% coloresborde ==FALSE && actual[s,l] %in% coloresnoborde ==FALSE){
   #   coloresnoborde <- c(coloresnoborde, s)
    #}
  #}
#}



tamanoCborde <- c()
for(p in coloresborde){
  tamanoCborde <- c(tamanoCborde, tabla[p])
}

tamanoNborde <- c()
for(pp in coloresnoborde){
  tamanoNborde <- c(tamanoNborde, tabla[pp])
}

d1 <- data.frame(coloresborde,tamanoCborde)
d2 <- data.frame(coloresnoborde, tamanoNborde)

g1 = d1$tamanoCborde
names(g1)=d1$coloresborde
png("ColoresBordeR2.png")
barplot(g1)
graphics.off()

g2 = d2$tamanoNborde
names(g2)=d2$coloresnoborde
png("ColoresNoBordeR2.png")
barplot(g2)
graphics.off()