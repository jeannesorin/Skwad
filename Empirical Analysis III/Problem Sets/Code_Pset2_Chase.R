# Empirical Analysis III
# Mogstad
# Problem Set 2
# 
# Chase Abram, Tom Hierons, Jeanne Sorin


library(haven)
library(MatchIt)
# install.packages("dplyr")
library(dplyr)

# Read in data, subset appropriately
data <- read_dta(file = "lalonde2.dta")
data$treated[is.na(data$treated)] <- 0
nsw <- subset(data, sample == 1)
cps <- subset(data, sample == 2)
psid <- subset(data, sample == 3)

# a) If treatment is random, then E[Y(1) - Y(0)] = E[Y(1) | D = 1] - E[Y(0) | D = 0]
nswT <- subset(nsw, treated == 1)
nswUT <- subset(nsw, treated == 0)

# demographic vars should be independent of treatment
# ols <- function(X, Y) {
#   return(solve(t(X)%*%X)%*%t(X)%*%Y)
# }
# 
# olsindeptest <- function(X,Y) {
#   beta = solve(t(X)%*%X)%*%t(X)%*%Y
#   return(beta[2])
# }
# 
# aug <- function(X) {
#   return(matrix(c(rep(1,len=length(X)), X), ncol = 2))
# }
# 
# re74test <- olsindeptest(aug(nsw$re74), nsw$treated)
# agetest <- olsindeptest(aug(nsw$age), nsw$treated)
# eductest <- olsindeptest(aug(nsw$educ), nsw$treated)
# blacktest <- olsindeptest(aug(nsw$black), nsw$treated)
# hisptest <- olsindeptest(aug(nsw$hisp), nsw$treated)
# marriedtest <- olsindeptest(aug(nsw$married), nsw$treated)
# nodegreetest <- olsindeptest(aug(nsw$nodegree), nsw$treated)
# w76test <- olsindeptest(aug(nsw$w76), nsw$treated)

vars = c("age", "educ", "black",
         "hisp", "kidmiss", "kids18",
         "married", "nodegree", "re74")

for(a in vars) {
  atest <- t.test(nswT[,a], nswUT[,a])
  cat(a, ": ", atest$p.value, "\n")
}

indep_lm <- glm(treated ~ age + educ + black + hisp
                + hisp + kidmiss + kids18
                + married + nodegree + metro + re74, data = nsw)


# b) If treatment is random, then E[Y(1) - Y(0)] = E[Y(1) | D = 1] - E[Y(0) | D = 0]
# nswT <- subset(nsw, treated == 1)
# nswUT <- subset(nsw, treated == 0)
betaindep <- mean(nswT$re78) - mean(nswUT$re78)
# betaols <- ols(aug(nsw$treated), nsw$re78)

betaols <- glm(re78 ~ treated, data = nsw)

# c) 

nswcps <- subset(data, (sample == 1 & treated == 1) | sample == 2)
# betanswcps <- ols(aug(nswcps$treated), nswcps$re78)

# naive model only looking at treatment
my_simple_model <- glm(re78 ~ treated, data = nswcps)

# less naive model, includes other demographic factors
my_standard_model <- glm(re78 ~ treated + age + educ + black
                         + hisp + kidmiss + kids18
                         + married + nodegree + metro + re74, data = nswcps)


# d)

# b = 20

for(a in vars) {
  temp_nsw_a <- as.numeric(unlist(nsw[,a]))
  nsw_hist <- hist(temp_nsw_a)
  
  temp_cps_a <- as.numeric(unlist(cps[,a]))
  cps_hist <- hist(temp_cps_a,xlim = c(0,30))
  
  xmin <- min(temp_nsw_a,temp_cps_a)
  ymin <- min(nsw_hist$density, cps_hist$density)
  xmax <- max(temp_nsw_a,temp_cps_a)
  ymax <- max(nsw_hist$density, cps_hist$density)
  
  plot(nsw_hist, col=rgb(0,0,1,1/4),freq = FALSE, 
       main = c("nsw vs. cps for ",a),
       xlab = a,
       xlim = c(xmin,xmax), ylim = c(ymin,ymax))
  plot(cps_hist, col=rgb(1,0,0,1/4),freq=FALSE, add=T)
  
}




# Probit regression on
# prop_model <- glm(treated ~ age + educ +
#                     black + nodegree + re74, family = binomial(link = "probit"),
#                   data = nswcps)
# 
# 
# # prop_df <- data.frame(prop_score = predict(prop_model, 
# #                                            type = "response"),
# #                       treated = nswcps$treated)
# 
# # add propensity scores to nsw + cps data rows
# prop_df <- cbind(prop_score = predict(prop_model,
#                                               type = "response"),
#                  nswcps)
# 
# # subset back into treated/untreated (nsw/cps)
# prop_df_tr <- subset(prop_df, treated == 1)
# prop_df_untr <- subset(prop_df, treated == 0)
# 
# # Occular inspection of propensity score support
# hist(prop_df_tr$prop_score, breaks = 100)
# hist(prop_df_untr$prop_score, breaks = 100)

#e)

# drop the NA columns
drops <- c("dwincl", "early_ra")
# nswcps_m <- data.frame(nswcps)
nswcps_m <- nswcps[,!(names(nswcps) %in% drops)]
nswcps_m <- na.omit(nswcps_m)


nn_match <- matchit(treated ~ age + educ + black
                    + hisp + kidmiss + kids18
                    + married + nodegree + metro + re74,
                    method = "nearest",
                    data = nswcps_m,
                    distance = "probit",
                    discard = "both")
# summary(nn_model)
# plot(nn_model)
plot(nn_match, type = 'jitter', interactive = FALSE)

# get matched dataset
nswcps_match <- match.data(nn_match)

# check for propensity score support and balancing
nn_prop_model <- glm(treated ~ age + educ + black 
                     + hisp + kidmiss + kids18
                     + married + nodegree + metro + re74, 
                     family = binomial(link = "probit"),
                     data = nswcps_match)


# naive model only looking at treatment
nn_simple_model <- glm(re78 ~ treated, data = nswcps_match)


# less naive model, includes other demographic factors
nn_standard_model <- glm(re78 ~ treated + age + educ + black 
                         + hisp + kidmiss + kids18
                         + married + nodegree + metro + re74, 
                         data = nswcps_match)

# add propensity scores to nsw + cps data rows
nn_prop_df <- cbind(prop_score = predict(nn_prop_model,
                                      type = "response"),
                 nswcps_match)

# subset back into treated/untreated (nsw/cps)
nn_prop_df_tr <- subset(nn_prop_df, treated == 1)
nn_prop_df_untr <- subset(nn_prop_df, treated == 0)

# Occular inspection of propensity score support
hist(nn_prop_df_tr$prop_score, freq = FALSE)
hist(nn_prop_df_untr$prop_score, add=T)

# ATT
ATT_match <- mean(nn_prop_df_tr$re78) - mean(nn_prop_df_untr$re78)

# f)

nn_prop_df_tr$re78cf <-rep(0,len = length(nn_prop_df_tr[,1]))
h <- 0.1

for(i in 1:length(nn_prop_df_tr[,1])) {
  # construct weights
  difs <- (nn_prop_df_tr$prop_score[i] - nn_prop_df_untr$prop_score)/h
  p_weights <- dnorm(difs/h)
  i_lm <- glm(nn_prop_df_untr$re78 ~ difs, weights = p_weights)
  nn_prop_df_tr$re78cf[i] <- i_lm$coefficients["(Intercept)"]
}

ATT_ll <- mean(nn_prop_df_tr$re78) - mean(nn_prop_df_tr$re78cf)









# 
# nn_model <- matchit(re78 ~ treated + age + educ +
#                        black + nodegree + re74, 
#                     method = "nearest",
#                     data = nswcps)












# get_matches(cps, id_cols = "age", newdata = nsw)

# get support of cps
# match_support <- function(X,Y, attribs) {
#   support = matrix(,nrow = 0, ncol = ncol(Y))
#   # check each row of "larger" data frame
#   for(i in 1:nrow(X)) {
#     if(i %% 10 == 0) {
#       cat("i: ", i, "\n")
#     }
#     # check until found in "smaller" data frame
#     for(j in 1:nrow(Y)) {
#       # if(j %% 100 == 0) {
#       #   cat("j: ", j, "\n")
#       # }
#       # check all attributes
#       currentmatch = TRUE
#       for(a in attribs) {
#         currentmatch = currentmatch & X[i,a] == Y[j,a]
#         if(!currentmatch) {
#           break
#         }
#       }
#       if(currentmatch) {
#         # print("match")
#         # print(X[i,])
#         # print(Y[j,a])
#         support <- rbind(support,X[i,])
#         break
#       }
#     }
#   }
#   return(support)
# }




# cpsmatchnsw <- match_support(cps,nsw, 
#                              c("age", "educ", "black",
#                                "nodegree", "hisp"))

# attribs <- c("age", "black")
# 
# for(a in attribs) {
#   print(nsw[1,a])
# }

# mutsupp <- merge(nsw, cps, by = c("black"))
# mutsupp <- intersect(nsw$age, cps$age)
# mutsupp <- inner_join(nsw, cps, by = "age")

