# Setup: Load packages and prepare data
packages <- c("tidyverse", "foreign", "AER", "stargazer")
lapply(packages, library, character.only = TRUE)
data = as_tibble(read.dta("lottery.dta")) %>%
  mutate(lotcateg = as.factor(lotcateg),
         year = as.factor(year),
         yd = lnw*d,
         ynd = lnw*(1-d),
         nd = 1-d)

# (b) Instrument Relevance -----------------------------------------------------
# Run a first stage with (HC) robust standard errors 
first_stage <- lm(d ~ z, data)
rob_se <- list(sqrt(diag(vcovHC(first_stage, type = "HC1"))))
# Note: HC1 matches STATA output, some evidence it is worse than R's default
# in finite sample (R:1, STATA:0)

# Nicely formatted TeX table:
stargazer(first_stage,title = "First Stage Regression", se = rob_se, digits = 3, 
          header = F,  out = "tables/first_stage.tex")             

# (c) Simple IV Estimate -------------------------------------------------------
# Fit an IV model with robust standard errrors and output table
iv_model <- ivreg(lnw ~ d | z, data=data)
iv_rob_se <-list(sqrt(diag(vcovHC(iv_model, type = "HC1"))))
stargazer(iv_model,title = "Instrument Variables Estimates", se = rob_se, 
          digits = 3, header = F,  out = "tables/iv_model.tex")   

# (d) Complier Comparisons -----------------------------------------------------
# Following notation from lecture define the following:
p_a = mean(filter(data, z==0)$d)
p_n = 1-mean(filter(data, z==1)$d)
p_c = 1 - p_a - p_n 

# Number of compliers
round(p_c*nrow(data))

# Check with regression:
first_stage[[1]][2]

# Proportion of male and female compliers using regression
f_compliers <- 
  (lm(d ~ z, filter(data,female==1))[[1]][2]*mean(data$female))/
  first_stage[[1]][2]

m_compliers <- 
  (lm(d ~ z, filter(data,female==0))[[1]][2]*(1-mean(data$female)))/
  first_stage[[1]][2]

# Proportion of women in the population
mean(data$female)

# See write up for details and argument

# (f) Y_0 & Y_1 Distributions for Compliers ------------------------------------

# Means 
# IV regressions to get the means (see write up for argument)
summary(ivreg(yd ~ d|z, data = data))  
summary(ivreg(ynd ~ nd|z, data = data)) 

# Full distributions:

# Estimate kernel densities
f_00 = density(filter(data,z==0&d==0)$lnw, n = 1000, 
               from= min(data$lnw), to = max(data$lnw))
f_01 = density(filter(data,z==0&d==1)$lnw, n = 1000, 
                      from= min(data$lnw), to = max(data$lnw))
f_10 = density(filter(data,z==1&d==0)$lnw, n = 1000, 
               from= min(data$lnw), to = max(data$lnw))
f_11 = density(filter(data,z==1&d==1)$lnw, n = 1000, 
               from= min(data$lnw), to = max(data$lnw))

# Y_0 density for compliers
ggplot()+
  geom_line( aes(x = f_00$x, # note: x is the same for all densities
                 y = (f_00$y *((p_c+p_n)/p_c)) - (f_10$y *(p_n/p_c)) ))+
  labs(x = "Log Wage", y = "Density")+
  ylim(-0.1,1.5)+
  ggsave("figs/g_c0_density.png", width = 7, height = 7)
  

# Y_1 density for compliers
ggplot()+
  geom_line( aes(x = f_00$x,
                 y = (f_11$y *((p_c+p_a)/p_c)) - (f_01$y *(p_a/p_c)) ))+
  labs(x = "Log Wage", y = "Density")+
  ylim(-0.1,1.5)+
  ggsave("figs/g_c1_density.png", width = 7, height = 7)

# (g) Always and Never Takers --------------------------------------------------
# Always takers- can get distribution of Y_1
ggplot()+
  geom_line( aes(x = f_01$x,
                 y = f_01$y))+
  labs(x = "Log Wage", y = "Density")+
  ylim(-0.1,1.5)+
  ggsave("figs/g_a_density.png", width = 7, height = 7)

# Never takers- can get distribution of Y_0 
ggplot()+
  geom_line( aes(x = f_10$x,
                 y = f_10$y))+
  labs(x = "Log Wage", y = "Density")+
  ylim(-0.1,1.5)+
  ggsave("figs/g_n_density.png", width = 7, height = 7)


# (h) Category x Year Specific LATE's ------------------------------------------
# TSLS with Category x Year controls and interactions
tsls_fit <- ivreg(lnw ~ d + lotcateg*year|lotcateg*year*z + lotcateg*year,
                  data = data)
tsls_fit[[1]][2]


# Initialise matrices to store estimates and weights and aggregates
late_out<-pop_weight<-comp_weight<-matrix(nrow = 4,ncol = 2)

rownames(late_out)<-rownames(pop_weight)<-rownames(comp_weight)<-
  levels(data$lotcateg)

colnames(late_out)<-colnames(pop_weight)<-colnames(comp_weight)<-
  levels(data$year)

pop_agg <- comp_agg <- 0

# Loop and populate with estimates, weights and compute aggregates
for (y in levels(data$year)){
  for (c in levels(data$lotcateg)){
    # Slope estimate:
    model <- ivreg(lnw ~ d|z, data = filter(data, year == y, lotcateg == c))
    late_out[c,y]<- model[[1]][2] 
    
    # Compute weights:
    pop_weight[c,y] <- nrow(filter(data, year == y, lotcateg==c))/nrow(data)
  
    comp_weight[c,y] <- 
      (lm(d ~ z, filter(data,female==1))[[1]][2]*pop_weight[c,y])/
      first_stage[[1]][2]

    # Update aggregates:
    pop_agg <- pop_agg + (pop_weight[c,y]*late_out[c,y])
    comp_agg <- comp_agg + (comp_weight[c,y]*late_out[c,y])
  }
}
rm(model)
stargazer(late_out,title = "LATE Estimates", digits = 3, 
          header = F,  out = "tables/late_out.tex")  


