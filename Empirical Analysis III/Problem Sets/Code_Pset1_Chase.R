# Empirical Analysis III
# Mogstad
# Problem Set 1
# 
# Chase Abram, Tom Hierons, Jeanne Sorin

# Get packages(s)
# install.packages("mvtnorm");
library(mvtnorm)

# Problem 2

# Parameters
sigma = 0.75
rho = 0.5
N = 100000

# "true" results
trueATE = 0
truebeta = (sigma^2 - 1)/sqrt(1 - 2*rho*sigma + sigma^2)*sqrt(2/pi)
trueATT = sqrt(1-2*rho*sigma + sigma^2)*sqrt(2/pi)
trueATUT = -trueATT

# Generate data
mymean = c(0,0)
# Note I generate (U_0, U_1), NOT (U_1, U_0)
mysigma = matrix(c(1, rho*sigma, rho*sigma, sigma^2), ncol = 2)
U <- rmvnorm(n=N, mean=mymean, sigma = mysigma)

# Spot check
plot(U, xlab="U_0", ylab="U_1", main="(U_0, U_1)")

# returns subset of X which is treated
treated <- function(X) {
  treats = matrix(, nrow = 0, ncol = 2)
  for(i in 1:length(X[,1]))
    if(X[i,2] > X[i,1]) {
      treats <- rbind(treats, X[i,])
    }
  return(treats)
}

# returns subset of X which is untreated
untreated <- function(X) {
  untreats = matrix(, nrow = 0, ncol = 2)
  for(i in 1:length(X[,1]))
    if(X[i,2] <= X[i,1]) {
      untreats <- rbind(untreats, X[i,])
    }
  return(untreats)
}

# Get treated and untreated groups
# Note that we have the counterfactuals in this experiment
tr <- treated(U)
ut <- untreated(U)

# Calculate treatment effects
ATE <- mean(U[,2] - U[,1])
ATT <- mean(tr[,2] - tr[,1])
ATUT <- mean(ut[,2] - ut[,1])
betaOLS <- mean(tr[,2]) - mean(ut[,1])

#########################

# Problem 3

# Monte Carlo

# a)

# Setup
N = 10000
X1 = rep(1, len=N)
X2 = rnorm(n=N, mean = 0, sd = 10)
# X2 = c(-4999:5000)
X = matrix(c(X1, X2), ncol=2)
beta = c(2,3)
sigma = 2
U = rnorm(n = N, mean = 0, sd = sigma)
Y = X%*%beta + U

betahat <- function(X,Y) {
  return(solve(t(X)%*%X)%*%t(X)%*%Y)
}

mybetahat <- betahat(X,Y)

# Uses homoskedasticity
Vhat <- function(X,Y) {
   return(solve(1/length(Y)*t(X)%*%X)*mean((Y-X%*%betahat(X,Y))^2))
}

myVhat <- Vhat(X,Y)

se <- function(X,Y) {
  return(sqrt(1/length(Y)*diag(Vhat(X,Y))))
}

myse <- se(X,Y)

# b)

sees <- function(bs) {
  return(sqrt(rowMeans(bs^2) - rowMeans(bs)^2))
}

S = 10

betas = matrix(, nrow = 2, ncol = 0)

for(s in 1:S) {
  Us <- rnorm(n = N, mean = 0, sd = sigma)
  Ys <- X%*%beta + Us
  betas <- cbind(betas, betahat(X,Ys))
}

mysees <- sees(betas)

hist(betas[2,])

# Nonparametric bootstrap

# a)

N = 10000

U1 <- rnorm(n=N, mean = 0, sd = 1)
U2 <- rnorm(n=N, mean = 0, sd = 1)
D <- rbinom(n=N,1,0.5)
augD <- matrix(c(rep(1,len=N), D), ncol = 2)
Y1 <- 5 + U1
Y0 <- 2 + U2
Y <- Y0 + D*(Y1 - Y0)
nbbetahat <- betahat(augD,Y)
nbse <- se(augD, Y)

# b)


bootsamp <- function(X,Y,N) {
  indices <- ceiling(runif(N,0,length(Y)))
  Xsamp <- X[indices,]
  Ysamp <- Y[indices]
  return(matrix(c(Xsamp,Ysamp), ncol=ncol(X)+1))
}

bootsamp(augD,Y,N)

S <- 10

nbbetas = matrix(, nrow = 2, ncol = 0)

for(s in 1:S) {
  samp <- bootsamp(augD,Y,N)
  Ds <- samp[,1:2]
  Ys <- samp[,3]
  nbbetas <- cbind(nbbetas, betahat(Ds,Ys))
}

nbsees <- sees(nbbetas)

hist(nbbetas[2,])



