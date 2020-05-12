# Load packages 
packages <- c("tidyverse", "MASS", "ggplot2")
lapply(packages, library, character.only=TRUE)

# Problem 2 ----

# Function to generate data and compute parameters
# Input is a vector whose first element is rho and second element is sigma
estimate <- function(v){
  r = v[1]
  s = v[2]
  N = 10^4
  covar = matrix(c(s^2, r*s, r*s, 1),
                 nrow = 2, ncol = 2)
  
  # Generate multivariate normal 
  U = mvrnorm(n=N, c(0,0), covar,
              tol = 1e-06, empirical = FALSE)
  
  # Derived variables
  D = U[,1] > U[,2]
  Y = U[,1]*D +U[,2]*(1-D)
  U_T = U[D,] # treated subset
  U_UT = U[!D,] # untreated subset
  
  # Compute parameters:
  ATE = mean(U[,1] - U[,2])
  ATT = mean(U_T[,1] - U_T[,2])
  ATUT = mean(U_UT[,1] - U_UT[,2])
  b_OLS = coef(lm(Y ~ D))[[2]]
  b_M = mean(U_T[,1])-mean(U_UT[,2])
  
  # Return the desired output
  return(c(ATE,ATT,ATUT,b_OLS, b_M))
}

# Compute and store estimates for each value of (rho,sigma)
inputs <- matrix(c(-0.5, 0, 0.5, 0.5, 0.5, 0.5,
                  2,2,2,1,2,3), 
                nrow = 6, ncol = 2)
estimates <- t(apply(inputs, 1, estimate))


# Format results nicely as a tibble
output <- cbind(inputs,estimates)
colnames(output) <- c("rho","sigma","ATE","ATT",
                 "ATUT","b_OLS", "b_M")
output = as_tibble(output)
rm(inputs,estimates)

# Problem 3: Monte Carlo----

# Single Draw
s = 4 
N = 10^4
x = rnorm(N,0,100)
u = rnorm(N,0,s)
y = 2 + 3*x + u 

# Fit ols
summary(lm(y~x))
rm(s,x,u,y)

# Replications (only compute coefficients for speed)
draw_ols <- function(N){
  s = 4 
  x = rnorm(N,0,100)
  u = rnorm(N,0,s)
  y = 2 + 3*x + u 
  X = t(rbind(t(rep(1,N)),x))
  solve(t(X)%*%X)%*%(t(X)%*%y)
}

# Run N replications of S=N observations and store the coefficients
ols_beta <- t(sapply(rep(N,N), draw_ols))

# Compute the simulated variance
se_a = sqrt(sum(ols_beta[,1]^2)/N- mean(ols_beta[,1])^2)
se_b = sqrt(sum(ols_beta[,2]^2)/N- mean(ols_beta[,2])^2)

# Plot a histogram for the estimates of the intercept
jpeg("histogram.jpg")
hist(ols_beta[,1],xlab = "Intercept",
     main = "Histogram for intercept")
dev.off()

  
# Problem 3: Bootstrap---- 
N = 10^4
U_1 = rnorm(N,0,1)
U_2 = rnorm(N,0,1)
D = rbinom(N,1,0.5)
Y_1 = 5 + U_1
Y_0 = 2 + U_2
Y = Y_0 +D*(Y_1-Y_0)

# Standard OLS
summary(lm(Y~D))

# Bootstrap
s = 1
while (s<=N){
  #Draw a sample with replacement
  draw = sample(N,size =N, replace=TRUE) # indices for sample
  Y_s = Y[draw]
  D_s = D[draw]
  X = t(rbind(t(rep(1,N)),D_s))
  # Estimate the coefficients and append them 
  b_s = solve(t(X)%*%X)%*%(t(X)%*%Y_s)
  if (s ==1){
    beta = b_s
  }
  else{
    beta = cbind(beta,b_s)
  }
  s = s+1
}

# Plot and save the histogram
jpeg("bootstrap_histogram.jpg")
hist(beta[1,],xlab = "Intercept",
     main = "Histogram for intercept")
dev.off()

# Variance calculation
se_a = sqrt(sum(beta[1,]^2)/N- mean(beta[1,])^2)
se_b = sqrt(sum(ols_beta[2,]^2)/N- mean(ols_beta[2,])^2)

# Bootstrap with Y and D drawn independently
s = 1
while (s<=N){
  #Draw a sample with replacement
  draw_1 = sample(N,size =N, replace=TRUE) # indices for sample
  draw_2 = sample(N,size =N, replace=TRUE)
  Y_s = Y[draw_1]
  D_s = D[draw_2]
  X = t(rbind(t(rep(1,N)),D_s))
  # Estimate the coefficients and append them 
  b_s = solve(t(X)%*%X)%*%(t(X)%*%Y_s)
  if (s ==1){
    beta_indep = b_s
  }
  else{
    beta_indep = cbind(beta_indep,b_s)
  }
  s = s+1
}

# Plot and save the histogram
jpeg("bootstrap_independent_histogram.jpg")
hist(beta[1,],xlab = "Intercept",
     main = "Histogram for intercept")
dev.off()
