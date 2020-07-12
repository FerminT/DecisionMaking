#### Simulaciones ####
library(MASS)

## RCE rule
# Confianza 1: eS1 < -0.5, eS2 < -0.5
# Confianza 2: eS1 < 1, eS2 < 1
# Confianza 3: eS1 < 2.5, eS2 < 2.5
# Confianza 4: eS1 <= 4, eS2 <= 4
# Confianza 5: eS1 > 4, eS2 > 4
categorizar_confianza_RCE <- function (evidencia_estimulo){
  if (evidencia_estimulo < -0.5){
    return(1)
  } else if (evidencia_estimulo < 1){
    return(2)
  } else if (evidencia_estimulo < 2.5){
    return(3)
  } else if (evidencia_estimulo < 4){
    return(4)
  } else{
    return(5)
  }
}

## BE rule
#     Confianza 1           Confianza 2             Confianza 3             Confianza 4         Confianza 5
# |eS2 - eS1| <= 0.5, 0.5 < |eS2 - eS1| <= 2, 2 < |eS2 - eS1| <= 3.5, 3.5 < |eS2 - eS1| <= 5, |eS2 - eS1| > 5
categorizar_confianza_BE <- function(eS1, eS2){
  distancia <- abs(eS2 - eS1)
  if (distancia < 0.5){
    return(1)
  } else if (distancia < 2){
    return(2)
  } else if (distancia < 3.5){
    return(3)
  } else if (distancia < 5){
    return(4)
  } else{
    return(5)
  }
}

N <- 10000 # Number of random samples
n_trials <- 2 * N

# La distribución S1 no varía
# S1 Distribution
mu_S1.eS1 <- 0.95
mu_S1.eS2 <- 0

# Parameters for bivariate normal distribution
mu.S1 <- c(mu_S1.eS2, mu_S1.eS1) # Mean
sigma.S1 <- diag(2) # Covariance matrix

# Posibles valores que toma la media de S2
valores_mu_S2 <- seq(0.05, 3.05, by=0.3)

# En este directorio se van a guardar los resultados en formato CSV
path_simulacion <- "E:/Documents/Exactas/Toma de Decisiones/TDD/Simulaciones/Resultados"
count <- 0

# Realizo cada una de las simulaciones
for (mu_S2 in valores_mu_S2){
  count <- count + 1
  # S2 Distribution
  mu_S2.eS1 <- 0
  mu_S2.eS2 <- mu_S2
  
  # Parameters for bivariate normal distribution
  mu.S2 <- c(mu_S2.eS2, mu_S2.eS1) # Mean
  sigma.S2 <- diag(2) # Covariance matrix
  
  # Realizo la simulación, tomo N valores de S1 y N valores de S2
  S1 <- mvrnorm(N, mu = mu.S1, Sigma = sigma.S1) # from MASS package
  colnames(S1) <- c("eS2", "eS1")
  S1 <- as.data.frame(S1)
  
  S2 <- mvrnorm(N, mu = mu.S2, Sigma = sigma.S2) # from MASS package
  colnames(S2) <- c("eS2", "eS1")
  S2 <- as.data.frame(S2)
  
  # Si el estímulo es S1, entonces estimulo=0. Caso contrario, estímulo=1.
  # Si la respuesta es S1, entonces respuesta=0. Caso contrario, respuesta=1.
  # La confianza se mide según los niveles explicados anteriormente.
  # Ordeno los estímulos tal que los S1 vienen primero y los S2 después.
  datos_trials <- data.frame(estimulo=rep(NA, n_trials), respuesta=rep(NA, n_trials), confianza_BE=rep(NA, n_trials), confianza_RCE=rep(NA, n_trials))
  for (i in 1:n_trials){
    if (i <= N){
      # El estímulo fue S1
      datos_trials$estimulo[i] <- 0
      # Para la regla RCE, la confianza depende de la respuesta
      if (S1$eS1[i] > S1$eS2[i]){
        # Respuesta = S1
        datos_trials$respuesta[i]  <- 0
        datos_trials$confianza_RCE[i] <- categorizar_confianza_RCE(S1$eS1[i])
      } else{
        # Respuesta = S2
        datos_trials$respuesta[i]  <- 1
        datos_trials$confianza_RCE[i] <- categorizar_confianza_RCE(S1$eS2[i])
      }
      datos_trials$confianza_BE[i] <- categorizar_confianza_BE(S1$eS1[i], S1$eS2[i])
    } else{
      j <- i - N
      # El estímulo fue S2
      datos_trials$estimulo[i] <- 1
      if (S2$eS1[j] > S2$eS2[j]){
        # Respuesta = S1
        datos_trials$respuesta[i] <- 0
        datos_trials$confianza_RCE[i] <- categorizar_confianza_RCE(S2$eS1[j])
      } else{
        # Respuesta = S2
        datos_trials$respuesta[i]  <- 1
        datos_trials$confianza_RCE[i] <- categorizar_confianza_RCE(S2$eS2[j])
      }
      datos_trials$confianza_BE[i] <- categorizar_confianza_BE(S2$eS1[j], S2$eS2[j])
    }
  }

  archivo <- paste(path_simulacion, "/simulacion", toString(count), ".csv", sep="")  
  write.csv(datos_trials, archivo)  
}


#### Correr script SimulationResults.py para procesarlos y generar las distintas métricas ####


# Directorio donde se almacenaron los resultados del script anterior
path_metricas <- "E:/Documents/Exactas/Toma de Decisiones/TDD/Simulaciones/Métricas"
archivos_metricas <- paste0(paste(path_metricas, "/metricas", sep=""), 1:count, ".csv")

# Cargo los datos
metricas <- lapply(archivos_metricas, read.csv)


#### Gráfico meta d' / d' ####

# Cargo los datos de d' y meta d'
dp              <- rep(NA, count)
meta_dp_BE      <- rep(NA, count)
meta_dp_RCE     <- rep(NA, count)
meta_dp_RCE_rS1 <- rep(NA, count)
meta_dp_RCE_rS2 <- rep(NA, count)
for (i in 1:count){
  dp[i] <- metricas[[i]]$dp[metricas[[i]]$rule == 'BE'] # Da igual la regla para el d'
  
  meta_dp_BE[i]      <- metricas[[i]]$meta_dp[metricas[[i]]$rule == 'BE']
  meta_dp_RCE[i]     <- metricas[[i]]$meta_dp[metricas[[i]]$rule == 'RCE']
  meta_dp_RCE_rS1[i] <- metricas[[i]]$meta_dp_rS1[metricas[[i]]$rule == 'RCE']
  meta_dp_RCE_rS2[i] <- metricas[[i]]$meta_dp_rS2[metricas[[i]]$rule == 'RCE']
}

plot(dp, meta_dp_BE, xlim=c(0.5, 3), ylim=c(-1, 3.5), xlab="d'", ylab="meta-d'", lwd=2, lty="dotted", type="b", pch=4)
lines(dp, meta_dp_RCE, lwd=2, lty="dashed", type="b", pch=15)
lines(dp, meta_dp_RCE_rS1, lwd=2, lty="dotted", col="red", type="b", pch=17)
lines(dp, meta_dp_RCE_rS2, lwd=2, lty="dotted", col="blue", type="b", pch=17)
legend("topleft", legend=c("BE", "RCE, general", "RCE, resp=\"S1\"", "RCE, resp=\"S2\""), col=c("black", "black", "red", "blue"), 
      lty=c(3, 3, 3, 3), pch=c(4, 15, 17, 17))


#### Gráfico ROC tipo 2 ####
par(mfrow=c(3, 4))
# Para la diagonal
x <- seq(0, 1, by=0.1)
y <- seq(0, 1, by=0.1)

nRatings <- 5
# Estos valores se van a sobreescribir, solo me interesa que haya un 0 al principio y 1 al final
HR2_rS1_RCE <- seq(from = 0, to = 1, length.out = nRatings + 1)
HR2_rS2_RCE <- seq(from = 0, to = 1, length.out = nRatings + 1)
FAR2_rS1_RCE <- seq(from = 0, to = 1, length.out = nRatings + 1)
FAR2_rS2_RCE <- seq(from = 0, to = 1, length.out = nRatings + 1)
HR2_rS1_BE <- seq(from = 0, to = 1, length.out = nRatings + 1)
HR2_rS2_BE <- seq(from = 0, to = 1, length.out = nRatings + 1)
FAR2_rS1_BE <- seq(from = 0, to = 1, length.out = nRatings + 1)
FAR2_rS2_BE <- seq(from = 0, to = 1, length.out = nRatings + 1)

offset <- 4 # Posición de los FAR2
columnas_tipo_2_rS1 <- c('HR2_rS1_1', 'HR2_rS1_2', 'HR2_rS1_3', 'HR2_rS1_4', 'FAR2_rS1_1', 'FAR2_rS1_2', 'FAR2_rS1_3', 'FAR2_rS1_4')
columnas_tipo_2_rS2 <- c('HR2_rS2_1', 'HR2_rS2_2', 'HR2_rS2_3', 'HR2_rS2_4', 'FAR2_rS2_1', 'FAR2_rS2_2', 'FAR2_rS2_3', 'FAR2_rS2_4')
for (i in 1:count){
  # Cambio los márgenes para que salga más grande
  par(mar = c(2,2,1,1))
  
  current <- metricas[[i]]
  currentRow_BE_rS1   <- as.numeric(current[current$rule == 'BE', columnas_tipo_2_rS1])
  currentRow_BE_rS2   <- as.numeric(current[current$rule == 'BE', columnas_tipo_2_rS2])
  currentRow_RCE_rS1  <- as.numeric(current[current$rule == 'RCE', columnas_tipo_2_rS1])
  currentRow_RCE_rS2  <- as.numeric(current[current$rule == 'RCE', columnas_tipo_2_rS2])
  for (j in 1:(nRatings - 1)){
    k <- nRatings - j
    HR2_rS1_BE[j + 1]  <- currentRow_BE_rS1[k]
    HR2_rS2_BE[j + 1]  <- currentRow_BE_rS2[k]
    HR2_rS1_RCE[j + 1] <- currentRow_RCE_rS1[k]
    HR2_rS2_RCE[j + 1] <- currentRow_RCE_rS2[k]
    
    FAR2_rS1_BE[j + 1]  <- currentRow_BE_rS1[k + offset]
    FAR2_rS2_BE[j + 1]  <- currentRow_BE_rS2[k + offset]
    FAR2_rS1_RCE[j + 1] <- currentRow_RCE_rS1[k + offset]
    FAR2_rS2_RCE[j + 1] <- currentRow_RCE_rS2[k + offset]
  }
  plot(FAR2_rS1_BE, HR2_rS1_BE, main=paste("eS2 =", toString(valores_mu_S2[i])), lwd=2, type="b", lty="dotted", pch=24, xlab="type 2 FAR", ylab="type 2 HR", bg="black")
  lines(FAR2_rS2_BE, HR2_rS2_BE, lwd=2, type="b", lty="dotted", pch=25, bg="black")
  lines(FAR2_rS1_RCE, HR2_rS1_RCE, lwd=2, type="b", lty="dotted", pch=24, col="red", bg="red")
  lines(FAR2_rS2_RCE, HR2_rS2_RCE, lwd=2, type="b", lty="dotted", pch=25, col="blue", bg="blue")
  lines(x, y, lwd=2, lty="dashed")
}
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend("center", legend=c("BE, resp=\"S1\"", "BE, resp=\"S2\"", "RCE, resp=\"S1\"", "RCE, resp=\"S2\""), 
       col=c("black", "black", "red", "blue"), lty=c(3, 3, 3, 3), pch=c(24, 25, 24, 25), cex=1.3)