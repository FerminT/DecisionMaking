library(mixtools)  #for ellipse
library(MASS)

# Function to draw ellipse for bivariate normal data
ellipse_bvn <- function(bvn, alpha, color = "red"){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col=color, lwd=2)
}

#### Gráfico #####

N <- 10000 # Number of random samples

# S1 Distribution
# Target parameters for univariate normal distributions
mu.eS1 <- 0.95; sigma.eS1 <- 1
mu.eS2 <- 0; sigma.eS2 <- 1

# Parameters for bivariate normal distribution
mu.S1 <- c(mu.eS2,mu.eS1) # Mean
sigma.S1 <- diag(2) # Covariance matrix
#rho <- -0.6
#sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2) # Covariance matrix

S1 <- mvrnorm(N, mu = mu.S1, Sigma = sigma.S1) # from MASS package
colnames(S1) <- c("eS2","eS1")

plot(S1, xlab="evidence for S2 (eS2)", ylab="evidence for S1 (eS1)", main="Two dimensional SDT model", col="firebrick", xlim = c(-4, 6), ylim= c(-4, 6))
ellipse_bvn(S1,.5)
ellipse_bvn(S1,.05)

# S2 Distribution
# Target parameters for univariate normal distributions
mu.eS1 <- 0; sigma.eS1 <- 1
mu.eS2 <- 3.05; sigma.eS2 <- 1

# Parameters for bivariate normal distribution
mu.S2 <- c(mu.eS2,mu.eS1) # Mean
sigma.S2 <- diag(2) # Covariance matrix
#rho <- -0.6
#sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2) # Covariance matrix

S2 <- mvrnorm(N, mu = mu.S2, Sigma = sigma.S2 ) # from MASS package
colnames(S2) <- c("eS2","eS1")

S2_dataframe <- as.data.frame(S2)
points(S2_dataframe$eS2, S2_dataframe$eS1, col="darkblue")
ellipse_bvn(S2,.5, "blue")
ellipse_bvn(S2,.05, "blue")
x <- c(-5, 7)
y <- c(-5, 7)
lines(x, y, lty="dashed", lwd=2)
## RCE rule
# Confianza 1: eS1 < -0.5, eS2 < -0.5
segments(-5, -0.5, -0.5, -0.5, lty="dashed", lwd=2)
segments(-0.5, -5, -0.5, -0.5, lty="dashed", lwd=2)
# Confianza 2: eS1 < 1, eS2 < 1
segments(-5, 1, 1, 1, lty="dashed", lwd=2)
segments(1, -5, 1, 1, lty="dashed", lwd=2)
# Confianza 3: eS1 < 2.5, eS2 < 2.5
segments(-5, 2.5, 2.5, 2.5, lty="dashed", lwd=2)
segments(2.5, -5, 2.5, 2.5, lty="dashed", lwd=2)
# Confianza 4: eS1 <= 4, eS2 <= 4
segments(-5, 4, 4, 4, lty="dashed", lwd=2)
segments(4, -5, 4, 4, lty="dashed", lwd=2)
# Confianza 5: eS1 > 4, eS2 > 4

## BE rule
# |eS2 - eS1| <= 0.5, 0.5 < |eS2 - eS1| <= 2, 2 < |eS2 - eS1| <= 3.5, 3.5 < |eS2 - eS1| <= 5, |eS2 - eS1| > 5

# Recordar que tiene que haber al menos algún punto en cada nivel de confianza
# Sumar 0.5 es una posibilidad

#### Simulaciones ####
library(MASS)
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

# |eS2 - eS1| < 0.5, 0.5 <= |eS2 - eS1| < 2, 2 <= |eS2 - eS1| < 3.5, 3.5 <= |eS2 - eS1| < 5, |eS2 - eS1| > 5
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
# Si el estímulo es S1, entonces estimulo=0. Caso contrario, estímulo=1.
# Si la respuesta es S1, entonces respuesta=0. Caso contrario, respuesta=1.
# La confianza se mide según los niveles explicados anteriormente.
datos_trials <- data.frame(estimulo=rep(NA, n_trials), respuesta=rep(NA, n_trials), confianza_BE=rep(NA, n_trials), confianza_RCE=rep(NA, n_trials))

# S1 Distribution
# Target parameters for univariate normal distributions
mu_S1.eS1 <- 0.95
mu_S1.eS2 <- 0

# Parameters for bivariate normal distribution
mu.S1 <- c(mu_S1.eS2, mu_S1.eS1) # Mean
sigma.S1 <- diag(2) # Covariance matrix

# S2 Distribution
# Target parameters for univariate normal distributions
mu_S2.eS1 <- 0
mu_S2.eS2 <- 0.05

# Parameters for bivariate normal distribution
mu.S2 <- c(mu_S2.eS2, mu_S2.eS1) # Mean
sigma.S2 <- diag(2) # Covariance matrix

S1 <- mvrnorm(N, mu = mu.S1, Sigma = sigma.S1) # from MASS package
colnames(S1) <- c("eS2", "eS1")
S1 <- as.data.frame(S1)
S2 <- mvrnorm(N, mu = mu.S2, Sigma = sigma.S2) # from MASS package
colnames(S2) <- c("eS2", "eS1")
S2 <- as.data.frame(S2)
# Ordeno los estímulos tal que los S1 vienen primero y los S2 después.
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

path <- "E:/Documents/Exactas/Toma de Decisiones/TDD/Simulaciones/Resultados/simulacion1.csv"
write.csv(datos_trials, path)