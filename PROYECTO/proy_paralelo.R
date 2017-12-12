#################################################
######## PROYECTO DE SIMULACION -- PARALELO #####
#################################################
library(parallel)
cluster <- makeCluster(detectCores() - 1)

cant.paradas <- 4 #cantidad de paradas
#cant.paradas <- cant.paradas+1
cap.camion <- 100 #capacidad del camion
t.llegada <- runif(cant.paradas) #probabilidad de llegada de personas a la parada por unidad de tiempo
t.bajada <- c(0,runif(cant.paradas -1)) #tasa de bajada de pasajeros por parada
t.entre.paradas <- sample(3:10,cant.paradas, replace = TRUE) #tiempo de traslado entre paradas aproximado luego se le va a sumar  un numero de variacion
p.esperando <- rep(0, cant.paradas) #personas esperando en cada parada
Paradas <- data.frame(t.llegada, t.bajada, t.entre.paradas, p.esperando)

Llegada <- c(0)#auxiliares solo para que el data frame T.espera tenga estos nombres en las columnas 
Salida <- c(0)
Parada <- c(0)
T.Espera <- data.frame(Llegada, Salida, Parada)#Registro de tiempo de espera de las personas en las paradas.

t.estimado.llegada <- c() #tiempo estimado de llegada de los camiones a cada parada 
for(i in 1:cant.paradas){
  t.estimado.llegada <- c(t.estimado.llegada, sum(Paradas$t.entre.paradas[1:i]))
}
#camiones
camiones <- matrix(0, ncol = 2+cant.paradas)
camiones[,2]<- cap.camion
camiones <- data.frame(camiones)
colnames(camiones) <- c("pos", "cap", 1:cant.paradas)
#-----------------------------
#tmax <- 5* sum(Paradas$t.entre.paradas[1:cant.paradas]) #duracion de la simulacion
tmax <- 1000 #duracion de la simulacion
pa <- 0.01 #probabilidad de accidentes
pv <- 0.02 #probabilidad de anticipasion
#-----------------------------
genera_pasaje <- function(i){
  for(j in 1:cant.paradas){ #las personas pueden llegar para cada min
    n <- runif(1)
    if(n < Paradas$t.llegada[j]){
      Paradas$p.esperando[j] <<- Paradas$p.esperando[j]+1 #llega una persona a la parada
      T.Espera <- rbind(T.Espera, c(i, 0, j))#se registra la persona que acaba de llegar
    }
  }
  return(T.Espera)
}
#------------------------------
act.pos.camion <- function(g){#actualiza la posicion del camion
  if(camiones[g,cant.paradas+2]==0){
     posca <- camiones[g,1]+1
     return(posca)
  }else{
    return(camiones[g,1])
  }
}
#------------------------------
accidente <- function(g){#funcion de accidentes
  if(camiones[g,cant.paradas+2]==0){#es un camion que todavia esta realizando el recorrido
    if(runif(1) < pa){
      retardo <- camiones[g,1]- sample(1:10,1) #se agrega el tiempo de accidente
      if(retardo < 0){#que  no haya posiciones negativas 
        retardo <- 0
        return(retardo)
      }
      return(retardo)
    }else{
      return(camiones[g,1])
    }
  }else{
    return(camiones[g,1])
    
  }
}
#--------------------------------

cant.camiones <- 1 #iniciamos con un camion
t.nvo.camion <- 10 #tiempo en que se libera un camion

#######INICIA SIMULACION#############
tiempoPar1 <- Sys.time()
for(i in 1:tmax){
  #i=100
  if(i%%t.nvo.camion == 0){#verifica si podemos liberar un camion
    camiones <- rbind(camiones, c(-1,cap.camion, rep(0,cant.paradas)))
    cant.camiones <- cant.camiones+1
  }
  
  T.Espera<-genera_pasaje(i)#se genera pasaje en cada parada
  
  #actualiza posicion del camion
  clusterExport(cluster, "act.pos.camion")
  clusterExport(cluster, "cant.camiones")
  clusterExport(cluster, "camiones")
  clusterExport(cluster, "cant.paradas")
  posi <- parSapply(cluster, 1:cant.camiones, act.pos.camion)
  
  camiones$pos <- posi #actualiza pos del camion
  
  for(w in 1:cant.camiones){#revisaremos si en la posicion que se encuentra cada camion es una parada, si es así baja y sube pasaje
    # w=1
    # if(camiones[w,1]==t.estimado.llegada[1]){
    if(camiones[w,1]==t.estimado.llegada[1] & camiones$`1`[w] == 0 ){#estoy en la primera parada y asegura que si la ha ocurrido accidente no volver a subir pasaje
      if(camiones[w,2] >= Paradas$p.esperando[1]){#se suben todas las personas si hay espacio suficiente
        camiones[w,2] <- camiones[w,2] - Paradas$p.esperando[1] #actualiza capacidad del camion 
        #se registra tiempo de salida o termino de espera para el data frame t.Espera
        personas.esperando <- which(T.Espera$Parada == 1 & T.Espera$Salida == 0)#quienes son las personas que estan esperando
        for(d in personas.esperando){
          T.Espera$Salida[d] <- i #en que momento se abordo la persona el camion
        }
        Paradas$p.esperando[1] <- 0 #se subieron todas las personas, entonces no hay nadie esperando en la parada
        camiones[w,3] <- i #actualiza la posicion del camion
      }else{#si no hay espacio suficiente se suben las que quepan
        cuantas<- Paradas$p.esperando[1] - camiones[w,2] #cuantas se quedan esperando
        if(Paradas$p.esperando[1] != camiones[w,2]){
          personas.esperando <- which(T.Espera$Parada == 1 & T.Espera$Salida == 0)#quienes son las personas que estan esperando
          #se suben las primeras personas que llegaron y que quepan en el camion
          for(d in personas.esperando[1:camiones[w,2]]){
            T.Espera$Salida[d] <- i
          } 
        }
        Paradas$p.esperando[1] <- cuantas #actualiza, cuantas son las personas que no pudieron abordar el camion
        camiones[w,2] <- 0 #el camion ya no tiene capacidad
        camiones[w,3] <- i #actualiza posicion del camion
      }
    }
    hora.estimada <- t.estimado.llegada[2:cant.paradas] #hora que tiene que llegar un camion a una parada distinta de la primera
    if(camiones[w,1] %in% hora.estimada){#si nos encontramos en una parada distinta de la primera
      parad <- which(t.estimado.llegada == camiones[w,1])#en que parada estamos
      if(camiones[w,parad+2]==0){#esto auxiliar por si ocurre accidentes no bajamos pasaje ni subimos
        camiones[w,2] <- camiones[w,2] + floor((cap.camion - camiones[w,2])* Paradas$t.bajada[parad])#se bajan personas del camion
        if(camiones[w,2] >= Paradas$p.esperando[parad]){#se suben todas las personas si hay espacio suficiente
          camiones[w,2] <- camiones[w,2] - Paradas$p.esperando[parad] #se actualiza capacidad del camion
          #se registra tiempo de salida o termino de espera para el data frame t.Espera
          personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
          for(d in personas.esperando){
            T.Espera$Salida[d] <- i #a que hora abordo el pasajero
          }
          Paradas$p.esperando[parad] <- 0 #se subieron todas las personas, no hay nadie esperando
          camiones[w,parad+2] <- i #en que momento llego a la parada
        }else{#si no hay espacio suficiente se suben las que quepan
          cuantas<- Paradas$p.esperando[parad] - camiones[w,2] #cuantas se quedan
          personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
          for(d in personas.esperando[1:camiones[w,2]]){
            T.Espera$Salida[d] <- i #a que hora abordaron los pasajeros
          }
          Paradas$p.esperando[parad] <- cuantas #cuantas personas se quedaron esperado en la parada
          camiones[w,2] <- 0 #el camion ya no tiene capacidad
          camiones[w,parad+2] <- i #a que hora llego a la parada
        }
      }
    }
  }

  clusterExport(cluster, "accidente")
  clusterExport(cluster, "cant.camiones")
  clusterExport(cluster, "camiones")
  clusterExport(cluster, "cant.paradas")
  clusterExport(cluster, "pa")
  acposi <- parSapply(cluster, 1:cant.camiones, accidente)
  
  camiones$pos <- acposi #actualiza pos del camion cuando hay accidente
  
  #VENTAJA
  for(g in 1: cant.camiones){#cualquier camion que este realizando recorrido puede tener una antisipacion
    # g=1
    if(camiones[g,cant.paradas+2]==0){#es un camion que todavia esta realizando un recorrido
      if(runif(1) < pv){
        camiones[g,1] <- camiones[g,1] + 1 #solo puede adelantarse un minuto
        #como se adelanta un minuto puede que se encuentre en una parada y si es asi se hace lo respectivo
        if(camiones[g,1]==t.estimado.llegada[1]){#estoy en la primera parada
          if(camiones[g,2] >= Paradas$p.esperando[1]){#se suben todas las personas si hay espacio suficiente
            camiones[g,2] <- camiones[g,2] - Paradas$p.esperando[1]
            #se registra tiempo de salida o termino de espera para el data frame t.Espera
            personas.esperando <- which(T.Espera$Parada == 1 & T.Espera$Salida == 0)
            for(d in personas.esperando){
              T.Espera$Salida[d] <- i
            }
            Paradas$p.esperando[1] <- 0 
            camiones[g,3] <- i
          }else{#si no hay espacio suficiente se suben las que quepan
            cuantas<- Paradas$p.esperando[1] - camiones[g,2] #cuantas se quedan
            personas.esperando <- which(T.Espera$Parada == 1 & T.Espera$Salida == 0)
            for(d in personas.esperando[1:camiones[g,2]]){
              T.Espera$Salida[d] <- i
            }
            Paradas$p.esperando[1] <- cuantas
            camiones[g,2] <- 0
            camiones[g,3] <- i
          }
        }
        if(camiones[g,1] %in% t.estimado.llegada[2:cant.paradas]){#si no estamos en la primera parada
          parad <- which(t.estimado.llegada == camiones[g,1])#en que parada estamos
          camiones[g,2] = camiones[g,2] + floor((cap.camion - camiones[g,2])* Paradas$t.bajada[parad])#se bajan personas del camion
          if(camiones[g,2] >= Paradas$p.esperando[parad]){#se suben todas las personas si hay espacio suficiente
            camiones[g,2] <- camiones[g,2] - Paradas$p.esperando[parad]
            #se registra tiempo de salida o termino de espera para el data frame t.Espera
            personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
            for(d in personas.esperando){
              T.Espera$Salida[d] <- i
            }
            Paradas$p.esperando[parad] <- 0 
            camiones[g,parad+2] <- i
          }else{#si no hay espacio suficiente se suben las que quepan
            cuantas<- Paradas$p.esperando[parad] - camiones[g,2]#cuantas se quedan
            personas.esperando <- which(T.Espera$Parada == parad & T.Espera$Salida == 0)
            for(d in personas.esperando[1:camiones[g,2]]){
              T.Espera$Salida[d] <- i
            }
            Paradas$p.esperando[parad] <- cuantas
            camiones[g,2] <- 0
            camiones[g,parad+2] <- i
          }
        }
      }
    }
  }
  
  if(all(camiones[,cant.paradas+2]!= 0)){
    break
  }
}
T.Espera <- T.Espera[-1,]
tiempoPar2 <- Sys.time()
tiempoEjecuP <- tiempoPar2 - tiempoPar1
stopCluster(cluster)