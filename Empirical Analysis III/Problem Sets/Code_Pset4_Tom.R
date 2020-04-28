# Setup: Load packages and prepare data ----------------------------------------
packages <- c("tidyverse", "foreign", "plm", "rdrobust", 
              "multiwayvcov","lfe", "stargazar", "rdd")
set.seed(152427042020)
lapply(packages, library, character.only = TRUE)

data = as_tibble(read.dta("final5.dta"))%>%
  mutate(schlcode = as.factor(schlcode),
         p_classize = c_size/(floor((c_size-1)/40)+1) 
  )

subsample = filter(data, c_size>=20 & c_size<=60) %>%
  mutate(small_class = c_size>40)

# ASIDE ON CLUSTERED STANDARD ERRORS -------------------------------------------
# Three ways to get clustered standard errors
# See MHE Ch8, R package documentation and STATA manual 
# These reproduce STATA's standard errors using the cluster command

# Method 1: lfe felm
m1 <- felm(avgmath ~classize +c_size + tipuach|0|0|schlcode, data = data)
summary(m1)

# Method 2: plm with small sample degrees of freedom correction 
m2 <- plm(avgmath ~classize +c_size + tipuach, 
          model='pooling', index=c('schlcode'), data = data)
G <- length(unique(data$schlcode)) # number of clusters
N <- length(data$schlcode) # number of observations
dfa <- (G/(G - 1)) * (N - 1)/m2$df.residual
coeftest(m2, vcov=function(x) dfa*vcovHC(x, cluster="group", type="HC0"))

# Method 3: multiwaycov 
m3 <- lm(avgmath ~classize +c_size + tipuach, data = data)
cluster_se <- cluster.vcov(m3, data$schlcode)
coeftest(m3,cluster_se)

rm(list=c("m1","m2","m3","G","N","dfa","cluster_se"))

# 1. Simple OLS ----------------------------------------------------------------
# OLS without controls, clustering standard errors at the school level 
simple_ols <- lm(avgmath ~ classize, data = data)
ols_se <- cluster.vcov(simple_ols, subsample$schlcode)
stargazer(simple_ols,title = "OLS without controls",
          se = ols_se, digits = 3, header = F,  out = "tables/simple_ols.tex")  
summary(simple_ols)

# OLS with controls
contr_ols <- lm(avgmath ~classize + c_size + tipuach, data = data)
contr_se <- cluster.vcov(contr_ols, subsample$schlcode)
stargazer(simple_ols,title = "OLS with controls",
          se = contr_se, digits = 3, header = F,  out = "tables/contr_ols.tex")
summary(contr_ols)

# 2. OLS RDD -------------------------------------------------------------------
lin_rdd_subs <- lm(avgmath ~ c_size + small_class +tipuach, data = subsample )
lin_se <- cluster.vcov(lin_rdd_subs, subsample$schlcode)
stargazer(simple_ols,title = "OLS with controls",
          se = contr_se, digits = 3, header = F,  out = "tables/lin_rdd.tex")
summary(lin_rdd_subs)

# 3. Local Linear Regression ---------------------------------------------------

# Local polynomial, with and without controlling for tipuach
poly_rdd_sub <- RDestimate(avgmath ~ c_size| tipuach, data = subsample,
                           cutpoint = 40)
summary(poly_rdd_sub)
poly_rdd_nocon <- RDestimate(avgmath ~ c_size, data = subsample,
                                     cutpoint = 40)
summary(poly_rdd_nocon)

# Bootstrapped Standard Errors
reps <- 1000 # number of repetitions
est <- est_nocon <- rep(0,reps)
for (i in 1:reps){
  draw = sample_n(subsample,nrow(subsample),replace = TRUE)
  est[i]<- RDestimate(avgmath ~ c_size| tipuach, data = draw,
                      cutpoint = 40)[[3]][1]
  est_nocon[i] <- RDestimate(avgmath ~ c_size, data = draw,
                             cutpoint = 40)[[3]][1]
}

b_se <- sqrt(var(est))
b_se_nocon <- sqrt(var(est_nocon))


# 4. Fuzzy RDD -----------------------------------------------------------------
# Clustering at school level
fuzzy_rd <- felm(avgmath ~c_size + tipuach|0|(classize~small_class)|schlcode,
                 data = subsample)
stargazer(fuzzy_rd, title = "Fuzzy RD", digits = 3, header = F,
          out = "tables/fuzzy_rd.tex")
summary(fuzzy_rd)
# Check against IV reg to compare standard errors 
iv_rd <- ivreg(avgmath ~classize + c_size + tipuach| 
                 tipuach + c_size +large_class, data = subsample)
summary(iv_rd)


# 5. rdrobust ------------------------------------------------------------------
robust_fuzzy <- rdrobust(y = subsample$avgmath, x = subsample$c_size, c = 40, 
                         fuzzy = subsample$classize, 
                         cluster = subsample$schlcode)
summary(robust_fuzzy)

# 6. Class Size Plot -----------------------------------------------------------
# Make a nice plot:
ggplot(data) +
  coord_fixed()+
  geom_point(aes(c_size,classize, colour = "grey"), size = 0.8)+
  geom_smooth(method = "loess", span = 0.05, 
              formula = y~x, mapping = aes(c_size,classize,colour="blue"), size = 0.5)+
  geom_line(aes(c_size,p_classize,colour = "red"))+
  labs(title = "Predicted agains actual class size", 
       x = "Enrollment", y = "Class Size")+ 
  scale_colour_manual(name = "",
                        values = c("blue","grey","red"),
                        labels=c("Polynomial Fit","Data", "Maimonides' Rule"))+
  ggsave("figs/class_size.png", height = 8, width = 12)

# 7. IV Estimates (with controls)-----------------------------------------------
iv_con <- felm(avgmath ~c_size + tipuach|0|(classize~p_classize)|schlcode,
     data = data)
summary(iv_con)

# 8. IV +/- Controls -----------------------------------------------------------
iv_nocon <- felm(avgmath~1|0|(classize~p_classize)|schlcode,
                data = data)
summary(iv_nocon)

# Double check using ivreg
iv_cov <- ivreg(avgmath ~ classize + tipuach + c_size|
                  p_classize + tipuach + c_size, data=data)
summary(iv_cov)
iv_nocov <-ivreg(avgmath ~ classize| p_classize, data=data)
summary(iv_nocov)

# 9. Manipulation --------------------------------------------------------------
ggplot(data=data, aes(x=weight, color=sex, fill=sex)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) 
# 10. Miscpecification 1 -------------------------------------------------------

# 11. Miscpecification 2 -------------------------------------------------------

# 12. Miscpecification 3 -------------------------------------------------------

# 13. Placebo Check ------------------------------------------------------------
