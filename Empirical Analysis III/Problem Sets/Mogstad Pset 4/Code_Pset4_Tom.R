# Setup: Load packages and prepare data ----------------------------------------
packages <- c("tidyverse", "foreign", "plm", "rdrobust", "AER",
              "multiwayvcov","lfe", "stargazer", "rdd")
set.seed(15242704)
lapply(packages, library, character.only = TRUE)

data = as_tibble(read.dta("final5.dta"))%>%
  mutate(schlcode = as.factor(schlcode),
         p_classize = c_size/(floor((c_size-1)/40)+1),
         enroll_cat = cut(c_size, breaks = seq(from = 0, to = 250, by = 20))
  )

subsample = filter(data, c_size>20 & c_size<60) %>%
  mutate(large_class = c_size<=40)

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
lin_rdd_subs <- lm(avgmath ~ c_size + large_class +tipuach, data = subsample )
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
fuzzy_rd <- felm(avgmath ~c_size + tipuach|0|(classize~large_class)|schlcode,
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
                         fuzzy = subsample$large_class)
robust_sharp <- rdrobust(y = subsample$avgmath, x = subsample$c_size, c=40)
summary(robust_sharp)
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
iv_nocon <- felm(avgmath~1|0|(classize~p_classize)|schlcode,
                 data = data)
summary(iv_nocon)



# 8. IV +/- Controls -----------------------------------------------------------
iv_con <- felm(avgmath ~c_size + tipuach|0|(classize~p_classize)|schlcode,
               data = data)
summary(iv_con)

# Double check using ivreg
iv_cov <- ivreg(avgmath ~ classize + tipuach + c_size|
                  p_classize + tipuach + c_size, data=data)
summary(iv_cov)
iv_nocov <-ivreg(avgmath ~ classize| p_classize, data=data)
summary(iv_nocov)

# 9. Manipulation --------------------------------------------------------------
ggplot(data=data, aes(x=c_size)) + 
  geom_histogram(aes(y=..density..), alpha=.3, colour = NA, fill = "blue",
                 position="identity", bins =60)+
  geom_density()+
  labs(x="Enrollment", y="Density")+
  scale_x_continuous(breaks=c(40,80,120,160,200))+
  scale_y_continuous(breaks=c(0.015))
  ggsave("figs/manipulation_check.png", width = 7, height = 7)
  
# 10.& 11. Miscpecification 1 & 2 ----------------------------------------------
# Aggregate up
binned <- data %>% group_by(enroll_cat)%>%
    summarise_at(c("avgmath", "classize"), mean, na.rm=TRUE)
binned$midpoint = seq(from = 10, to = 230, by = 20)

# Plot binned data for maths
ggplot(data = binned, aes(x = midpoint,y=avgmath))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x, se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  ggsave("figs/maths_linear_fit.png", width = 7, height = 7)

ggplot(data = binned, aes(x = midpoint,y=avgmath))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x+I(x^2), se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  ggsave("figs/maths_quadratic_fit.png", width = 7, height = 7)

ggplot(data = binned, aes(x = midpoint,y=avgmath))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x+I(x^2)+I(x^3) +I(x^4), se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  ggsave("figs/maths_poly_fit.png", width = 7, height = 7)

# And for class size
ggplot(data = binned, aes(x = midpoint,y=classize))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x, se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  ggsave("figs/class_linear_fit.png", width = 7, height = 7)

ggplot(data = binned, aes(x = midpoint,y=classize))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x+I(x^2), se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  ggsave("figs/class_quadratic_fit.png", width = 7, height = 7)

ggplot(data = binned, aes(x = midpoint,y=classize))+
  geom_point()+
  geom_smooth(method=lm, formula = y~x+I(x^2)+I(x^3) +I(x^4), se = FALSE)+
  scale_x_continuous(breaks=seq(from=40,to=200,by=40), minor_breaks = NULL)+
  scale_y_continuous(breaks=NULL)+
  ggsave("figs/class_poly_fit.png", width = 7, height = 7)


# 12. Miscpecification 3 -------------------------------------------------------
# 1) Bandwidth 
for (b in seq(from=2,to=20,by=2)){
  trimmed <- filter(data, 
                    (abs(c_size-40)<b)|(abs(c_size-80)<b)|
                      (abs(c_size-120)<b)|(abs(c_size-180)<b)|
                      (abs(c_size-220)<b))
  trim_iv <- felm(avgmath ~c_size + tipuach|0|(classize~p_classize)|schlcode,
                  data = trimmed)
  print(trim_iv[[7]][4])
}


# 2) Controlling for enrollment with a quadratic term and a polynomial
iv_quad <- felm(avgmath ~c_size + I(c_size^2) + tipuach|0|(classize~p_classize)|schlcode,
               data = data)
summary(iv_quad)

iv_poly <- felm(avgmath ~c_size + I(c_size^2)+I(c_size^3)+I(c_size^4) + tipuach|0|(classize~p_classize)|schlcode,
                data = data)
summary(iv_poly)

# 13. Placebo Check ------------------------------------------------------------
# IV placebo test
iv_plac <- felm(tipuach ~c_size|0|(classize~p_classize)|schlcode,
               data = data)
iv_plac <- ivreg(tipuach ~ classize + c_size|
        p_classize + c_size, data=data)
summary(iv_plac)
