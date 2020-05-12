## ---------------------------
##
## Script name: Problem Set 1 - Jeanne
##
## Purpose of script: self-explanatory
##
## Author: Jeanne Sorin
##
## Date Created: 2020-04-09
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC

setwd("~/Documents/Skwad/")    

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(data.table)

## ---------------------------

## load up our functions into memory

source("~/Documents/PhD/Skwad/Random_Resources/JeanneR_formula.R") 

## ---------------------------

library(MASS)
library(pracma)
library(rlist)

######################### Problem 2 #################################################

# Define functions
# Compute yi : the observed outcome at the individual level
compute_yi <- function(x, N){
  yi <- replicate(N, 0)
  di <- replicate(N,0)
  for (i in 1:N){
    if (x[i,1] > x[i,2]){
      di[i] <- 1
      yi[i] <- x[i,1]
    }
    else{
      di[i] <- 0
      yi[i] <- x[i,2]
    }
  }
  output <- list("yi" = yi, "Di" = di)
  return(output)
}

parameters <- function(dataset){
  ATE = mean(dataset$y1 - dataset$y0)
  ATT = mean(dataset[dataset$Di==1,]$y1 - dataset[dataset$Di==1,]$y0)
  ATU = mean(dataset[dataset$Di==0,]$y1 - dataset[dataset$Di==0,]$y0)
  # Compute Bols
  regression <- lm(Yi ~ Di, data=dataset)  
  beta = coef(regression)[2]
  # E(Y | D = 1) - E(Y | D = 0) --> corresponds to B_{OLS}
  EY_diff = mean(dataset[dataset$Di==1,]$Yi) - mean(dataset[dataset$Di==0,]$Yi)
  
  list <- list("ATE"=ATE, "ATT"=ATT, "ATU"=ATU, "beta"=beta, EY_diff="EY_diff")
  return(list)
  }


routine <- function(s, r, N=10000, print=TRUE, ret=FALSE){
  s <- s
  r <- r
  N <- N
  
  # Variance covariance matrix
  Sigma <- matrix(c(s^2, s*r, s*r, 1), 2, 2)
  # mean
  mu <- c(0, 0)
  
  # Simulate the multivariate normal and create a dataset
  x <-  mvrnorm(n=N, mu, Sigma)
  Yi_output <- compute_yi(x, N)
  Yi <- Yi_output$yi
  Di <- Yi_output$Di
  dataset <- data.frame(y1 = x[,1],
                        y0 = x[,2],
                        Yi = Yi,
                        Di = Di)
  
  estimates <- parameters(dataset)
  
  if (print==TRUE){
    print(paste("Parameters are: sigma = ", s, ", rho = ", r))
    print(paste("Estimates are: ATE = ", estimates$ATE, " ATT = ", estimates$ATT, ", ATU = ", estimates$ATU, 
                " and beta_ols = ", estimates$beta))
    print(paste("Moreover, mean(D) = ", mean(dataset$Di), ", mean(Yi) = ", mean(dataset$Yi)))
  }
  if (ret==TRUE){
    return(estimates)
  }
}



#### Run functions with parameters from pset
routine(s=2, r=0.5, N=10000)
routine(s=3, r=0, N=10000)
routine(s=2, r=-0.5, N=10000)

ATT = replicate(100, 0)
ATU = replicate(100, 0)

seq_s = seq(0.1, 3, length.out=100)
for (i in 1:100){
  ATT[i] <- routine(s=seq_s[i], r=0.5, N=10000, print=FALSE, ret=TRUE)$ATT
  ATU[i] <- routine(s=seq_s[i], r=0.5, N=10000, print=FALSE, ret=TRUE)$ATU
  }

plot(seq_s, ATT, type="l", xlab="sigma")
# Yay : effect decreases with sigma for sigma < 0, then increases : intuition was right

plot(seq_s, ATU, type="l", xlab="sigma")




######################### Problem 3 #################################################

##### Monte Carlo Simulations

### Parameters
N <- 10000
beta <- c(2,3)
sigma <- 2


########
# PART a
########

### Home made functions for the data generating process
draw_x <- function(N, b=1){ #Generate bivariate X
  return(x = mvrnorm(n=N, c(b, 0), matrix(c(0,0,0,100), 2, 2)))
}
draw_u <- function(N, sigma, mean=0){ #Generate error u
  return(u = mvrnorm(n=N, mean, sigma^2))
}
compute_y <- function(x, beta, u){ # Compute resulting u
  return(y = x %*% beta + u)
}
compute_beta <- function(x, y){
  return(beta_hat = solve(t(x) %*% x) %*% (t(x) %*% y))
}
compute_beta_std <- function(x, y){
  vhat = solve(1/length(y)*t(x)%*%x)*mean((y-x%*%compute_beta(x,y))^2)
  return(std = sqrt(1/length(y)*diag(vhat)))
}

  
### Draw data
x = draw_x(N)
u = draw_u(N, sigma)
y = compute_y(x, beta, u)

### Estimate beta and its standard errors
beta_hat = compute_beta(x, y)
beta_hat_std = compute_beta_std(x, y)

beta_hat
beta_hat_std





########
# PART b
########

make_monte_carlo<- function(S, x, beta, sigma, plot=TRUE, ret=FALSE){
  # Initiate the matrix of beta_hats (S*2)
  beta_hats = matrix(replicate(S*2,0), S)
  # Compute beta_hats for each s
  for (s in 1:S){
    us = draw_u(dim(x)[1], sigma)
    ys = compute_y(x, beta, us)
    beta_hats[s,] = compute_beta(x, ys)
  }
  # Plot histogram of the first component of beta_hats
  if (plot==TRUE){hist(beta_hats[,1], breaks=100)}
  
  # Compute empirical se of beta_hats
  std = sqrt(colMeans(beta_hats^2) - colMeans(beta_hats)^2)
  
  
  print(paste("The standard error of beta_hat[1] from MC is ", std[1]))
  print(paste("The standard error of beta_hat[2] from MC is ", std[2]))
  
  # Return beta_hats vector (option)
  if (ret==TRUE){return(beta_hats)}
}

make_monte_carlo(10000, x, beta, sigma, plot=TRUE)
# [1] "The standard deviation of beta_hat[1] from MC is  0.0143377288638622"
# [1] "The standard deviation of beta_hat[2] from MC is  0.00142616781194698"







##### Non-parametric Bootstrap

########
# PART a
########

### Parameters
N = 10000

### Data generating process
u = draw_u(N,mean=c(0,0), sigma = matrix(c(1, 0, 0, 1), 2, 2))
y1 = u[,1] + 5                   # Potential outcome y1
y0 = u[,2] + 2                   # Potential outcome y0
D = rbinom(N, size=1, prob=0.5)  # Randomly assigned treatment
Y = y0 + D*(y1 - y0)             # Observed Y
D = matrix(c(rep(1, N), D), ncol = 2)   # Add a constant

### Compute
compute_beta(D, Y)
compute_beta_std(D, Y)







########
# PART b
########

pick_replacement <- function(N, X, Y){ # Function that randomly picks N observations out of an initial dataset
  indices <- ceiling(runif(N,0,length(Y)))
  #indices2 <- ceiling(runif(N,0,length(Y)))
  
  Xs <- X[indices,]
  Ys <- Y[indices]
  return(matrix(c(Xs,Ys), ncol=ncol(X)+1))
}

bootstrap_function = function(N, S, Y, X){
  beta_hats = matrix(replicate(S*2,0), S)
  
  for (s in 1:S){
    data <- pick_replacement(N, D, Y)
    Ds <- data[,1:2]
    Ys <- data[,3]
    beta_hats[s,] <- compute_beta(Ds, Ys)
  }
  return(beta_hats)
}

bootstrap_monte <- function(S, N, Y, X, plot=TRUE, ret=FALSE){
  
  betas = bootstrap_function(N, S, Y, X)
  print(median(betas[,2]))
  print(mean(betas[,2]))
  
  betas_error = sqrt(colMeans(betas^2) - colMeans(betas)^2)
  #print(betas_error)
  
  #vhat = (1/N)*colSums(betas^2) - ((1/N)*colSums(betas))^2
  #betas_std = sqrt((1/N)*diag(vhat))
  
  print(paste("The standard deviation of beta_hat[1] from MC is ", betas_error[1]))
  print(paste("The standard deviation of beta_hat[2] from MC is ", betas_error[2]))
  
  # Plot histogram
  if (plot==TRUE){
    hist(betas[,1], breaks=100)
    hist(betas[,2], breaks=100)
  }
  
  # Bonus: you can even get the vector of beta_hats
  if (ret==TRUE){
    return(betas)
  }
}

N = 10000
S = 10000
bootstrap_monte(S, N, Y, D)




