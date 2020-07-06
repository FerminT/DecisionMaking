library(mixtools)  #for ellipse
library(MASS)

# Function to draw ellipse for bivariate normal data
ellipse_bvn <- function(bvn, alpha, color = "red"){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col=color, lwd=2)
}

N <- 1000 # Number of random samples

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
x <- c(-6, 7)
y <- c(-6, 7)
lines(x, y, lty="dashed", lwd=2)