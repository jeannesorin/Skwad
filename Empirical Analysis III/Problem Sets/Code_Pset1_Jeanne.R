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

### Monte Carlo Simulations
N <- 10000
beta <- c(2,3)
sigma <- 2

########
# PART a
########

draw_x <- function(N, b){
  x <- mvrnorm(n=N, c(1, 0), matrix(c(0,0,0,100), 2, 2))
  return(x)
}

draw_u <- function(N, mean=0, sigma){
  u <- mvrnorm(n=N, mean, sigma)
  return(u)
}

compute_y <- function(x, beta, u){
  y <- dot(t(x), beta) + u
  #y <- x[,1] + beta[2]*x[,2] + u
  return(y)
}

# Draw data
x = draw_x(N, beta[1])
u = draw_u(N, sigma)
y = compute_y(x, beta, u)

# Estimate beta and its standard errors
# Manually (nb t(x) and x inverted compared to theory bc x is a row vector here)
# y = x*beta + ui
# x'y = x'x*beta + x'ui
# E(x'y) = E(x'x)*beta + E(x'ui) = E(x'x)*beta
# E(x'x)^{-1} E(x'y) = beta_hat

beta_hat = solve(t(x) %*% x) %*% (t(x) %*% y)

# Check using OLS
#regression <- lm(y ~ x + 0)
#regression


########
# PART b
########
make_monte_carlo<- function(S, x, beta, sigma, plot=TRUE, ret=FALSE){
  beta_hats = matrix(replicate(S*2,0), S)
  # Compute beta_hats for each s
  for (s in 1:S){
    us = draw_u(dim(x)[1], sigma)
    ys = compute_y(x, beta, us)
    beta_hats[s,] = t(solve(t(x) %*% x) %*% (t(x) %*% ys))
  }
  # Plot histogram of the first component of beta_hats
  if (plot==TRUE){hist(beta_hats[,1])}
  
  # Compute empirical std_dev of beta_hats
  sum = ((1/S)*colSums(beta_hats))^2
  std = ((1/S)*colSums(beta_hats*beta_hats) - sum)^0.5
  print(paste("The standard deviation of beta_hat[1] from MC is ", std[1]))
  print(paste("The standard deviation of beta_hat[2] from MC is ", std[2]))
  
  # Return beta_hats vector (option)
  if (ret==TRUE){return(beta_hats)}
}


make_monte_carlo(10000, x, beta, sigma, plot=TRUE)
# [1] "The standard deviation of beta_hat[1] from MC is  0.0143377288638622"
# [1] "The standard deviation of beta_hat[2] from MC is  0.00142616781194698"







### Non-parametric Bootstrap

# PART a

n = 10000
u = draw_u(N,mean=c(0,0), sigma = matrix(c(1, 0, 0, 1), 2, 2))
y = u
y[,1] = y[,1] + 5
y[,2] = y[,2] + 2
D = rbinom(N, size=1, prob=0.5)

Y = y[,1] + D*(y[,2] - y[,1])

dataset_boot = data.frame(Y = Y, y0 = y[,1], y1 = y[,2], D = D)

reg = lm(Y ~ D)
std = ((1/(N-2))*sum(reg$residuals*reg$residuals))
print(paste("Beta[1] estimated from OLS is ", reg$coefficients[1]))
print(paste("Beta[2] estimated from OLS is ", reg$coefficients[2]))
print(paste("The estimated standard deviation of beta is ", std))

# V(beta) = V((beta*D + ui)*D) / V(D^22)
# V(beta) = V(beta) + V(ui*D)/ V(D) because D = D^2
# V(beta) = V(ui*D)/ V(D)
dataset_boot$u = reg$residuals
var_beta = var(dataset_boot$u*dataset_boot$D)/var(dataset_boot$D) # = (mean((ui*D)^2) - mean(ui*D)^2) / (0.25)
std_beta = var_beta^0.5
# Argue that OLS gives consistent estimates of beta


# PART b

pick_replacement <- function(N, dataset_original){
  # Initiate dataset
  final = data.frame(Y = double(),
                     y0 = double(),
                     y1 = double(),
                     D = integer(),
                     u = double())
  # Add N picks from the original dataset
  for (i in 1:N){final = rbind(final, dataset_original[sample(nrow(dataset_original), 1), ])}
  # Return final dataset
  return(final)
}

N = 10000
S = 10000

bootstrap_function = function(N, S, dataset_original){
  list_datasets = list()
  for (s in 1:S){
    final =  pick_replacement(N, dataset_boot)
    list_datasets = list.append(list_datasets, final)
  }
  return(list_datasets)
}

final = bootstrap_function(N, S, dataset_boot)

bootstrap_monte <- function(final, S, plot=TRUE, ret=FALSE){
  beta_hats = matrix(replicate(S*2,0), S)
  for (i in 1:S){
    data_sub = data.frame(final[i])
    reg = lm(Y ~ D, data_sub)
    beta_hats[i,] = reg$coefficients
  }
  
  sum = ((1/S)*colSums(beta_hats))^2
  std = ((1/S)*colSums(beta_hats*beta_hats) - sum)^0.5
  print(paste("The standard deviation of beta_hat[1] from MC is ", std[1]))
  print(paste("The standard deviation of beta_hat[2] from MC is ", std[2]))
  
  if (plot==TRUE){
    hist(beta_hats[,1])
  }
  
  if (ret==TRUE){
    return(beta_hats)
  }
}

bootstrap_monte(final, S)

