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
