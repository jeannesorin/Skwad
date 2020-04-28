# Empirical Analysis III
# Mogstad
# Problem Set 4
# Spring 2020
# 
# Chase Abram, Tom Hierons, Jeanne Sorin

# DISCLAIMER: I am basically following and replicating Jeanne's code, 
# and checking along the way.

###############
# Get started

# For Chase' Mac
setwd("~/UChiGit/Skwad/Empirical Analysis III/Problem Sets")

# Load packages
packages <- c("tidyverse", "data.table", "foreign", "locpol", "ivpack",
              "stargazer", "rdd", "ggplot2", "lfe", "rdrobust")
lapply(packages, library, character.only = TRUE)


# Seed in case RNG needed
# set.seed(23)

# Read in data
my_data <- read.dta(file = "data_pset4/final5.dta")

##################
# 1) We use felm() to cluster errors as in A&L (1999)

# Naive OLS of class size on math scores (no controls)
naive_ols_no_controls <- felm(avgmath ~ classize | 0 | 0 | schlcode, 
                              data = my_data)

# Add disadvantaged as controls
naive_ols_tipuach <- felm(avgmath ~ classize + tipuach | 0 | 0| schlcode, 
                         data = my_data)

# Add enrollment as control
naive_ols_tipuach_c_size <- felm(avgmath ~ classize + tipuach + c_size | 0 | 0| schlcode, 
                                data = my_data)

# Pretty output goes here - Needs to be updated to work with above
# data analysis. Don't forget to include se!
# stargazer()


##################
# 2)

# Get data with between 20 and 60 students (exclusive), and add dummy
my_data_20_60 <- my_data %>% 
  filter(c_size > 20 & c_size < 60) %>% 
  mutate(large = ifelse(c_size > 40,1,0))

# Sharp RDD at 40 student cutoff
sharp_rdd_20_60_ols <- felm(avgmath ~ large + tipuach + c_size | 0 | 0 | schlcode, 
                           data = my_data_20_60)


##################
# 3) Note: se.type = "HC1" makes the se calculations hetskad robust

# Uses full sample in bandwidth
my_bw = max(my_data_20_60$c_size)

# Run Sharp RDD with loc lin regression
sharp_rdd_20_60_loc_lin <- RDestimate(avgmath ~ c_size,
                              data = my_data_20_60,
                              cutpoint = 40,
                              bw = my_bw,
                              se.type = "HC1")

# Bootstrap to find standard errors
S = 10
N = nrow(my_data_20_60)

betas = rep(0,S)
for(s in 1:S) {
  sample_num <- sample(1:N, N, replace=TRUE)
  sample_data <- my_data_20_60[sample_num,]
  rdd_samp <- RDestimate(avgmath ~ c_size,
                         data = sample_data,
                         cutpoint = 40,
                         bw = my_bw,
                         se.type = "HC1")
  betas[s] <- rdd_samp$est["LATE"]
}

print(mean(betas))
hist(betas)

beta_se_boot <- sqrt(mean(betas^2) - mean(betas)^2)

################
# 4)

# Uses full sample in bandwidth
my_bw = max(my_data_20_60$c_size)

# Run Sharp RDD with loc lin regression
fuzzy_rdd_20_60 <- RDestimate(avgmath ~ c_size + classize | tipuach + c_size,
                                      data = my_data_20_60,
                                      cutpoint = 40,
                                      bw = my_bw,
                                      se.type = "HC1")


##############
# 5)

# check what is going on here
rdr_20_60 <- rdrobust(y = my_data_20_60$avgmath, x = my_data_20_60$c_size,
                      fuzzy = my_data_20_60$large,
                      c = 40)

##############
# 6)

# Add predicted class size
my_data <- my_data %>% mutate(predcs = c_size/(floor((c_size - 1)/40) + 1))

# Directly from Jeanne
ggplot() + theme_classic() +
  geom_point(data = my_data, aes(c_size, classize), pch=20, color="grey") +
  geom_hline(yintercept=c(20, 27, 30, 40), lty=3, color="magenta") +
  geom_vline(xintercept = c(40, 80, 120, 160), lty=3, color="blue") +
  geom_line(data = my_data, aes(c_size, predcs), color="red") + xlab("Enrollment Count") + ylab("Class Size") + 
  theme(legend.position = c(0.75, 0.15),
        legend.justification = c("right", "top"))
  # geom_line(data = spline.d, aes(x = x, y = y)) 
dev.off()

last_plot()

################
# 7)

# Some NAs will mess with standard errors function
new_data = my_data[!(is.na(my_data$schlcode) | is.na(my_data$avgmath)
                     | is.na(my_data$classize) | is.na(my_data$predcs)),]

iv_lm <- ivreg(avgmath ~ classize | predcs, data=new_data)
iv_cluster_se <- cluster.robust.se(iv_lm, new_data$schlcode)

###############
# 8)

###############
# 9)

hist(my_data$c_size)


###############
# 10)

###############
# 11)

###############
# 12)


###############
# 13)




