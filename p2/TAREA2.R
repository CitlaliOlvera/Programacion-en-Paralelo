  
  library(parallel)
  dimensiones <- c(10,20,30,40)
  probabilidades <-c(0.1, 0.25, 0.35, 0.5, 0.65, 0.75, 0.9)
  resultados <- data.frame()
  
  vecnumerosupiteraciones <- c()
  for(w in dimensiones){
    promedioiteraciones <- c()
    
    for(s in probabilidades){
      datospromedio <- c()
      
      for(z in 1:10){
  dim <- w
  num <-  dim^2
  proba <- s
  actual <- matrix(round(runif(num)<proba)*1, nrow=dim, ncol=dim)
  suppressMessages(library("sna"))
  
  paso <- function(pos) {
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
  }
  
  cluster <- makeCluster(detectCores() - 2)
  clusterExport(cluster, "dim")
  clusterExport(cluster, "paso")
  
  numeroiteraciones <- 0
  for (iteracion in 1:100) {
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)
    if (sum(siguiente) == 0) { # todos murieron
     # print("Ya no queda nadie vivo.")
      numeroiteraciones <- iteracion - 1
      break;
    }else{
      vecnumerosupiteraciones <- c(vecnumerosupiteraciones ,numeroiteraciones + 1)
    }
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  
  }
  stopCluster(cluster)
  datospromedio <- c(datospromedio, numeroiteraciones)
      }
      promedioiteraciones <- c(promedioiteraciones, mean(datospromedio))
    }
   # resultados <- cbind(resultados,promedioiteraciones)
   # print(probabilidades)
    #print(promedioiteraciones)
    
    g<- paste0("T2",toString(w),".png",collapse = '')
    png(g)
    plot(probabilidades, promedioiteraciones, type = "overplotted",
         pch=1, col="blue", xlab = "probabilidad", ylab = "iteraciones",
         main = "probabilidad vs iteraciones ")
    graphics.off()
  }
  
 