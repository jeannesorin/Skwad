# Empirical Analysis III
# Mogstad
# Problem Set 4
# 
# Chase Abram, Tom Hierons, Jeanne Sorin


library(haven)
library(MatchIt)
library(dplyr)
library(foreign)
library(stargazer)
library(locpol)
# install.packages("rdd")
library(rdd)
# set.seed(23)

# Read in data
my_data <- read.dta(file = "final5.dta")

##################
# 1)

# Naive OLS of class size on math scores (no controls)
naive_ols_no_controls <- glm(avgmath ~ classize, data = my_data)

# Add disadvantaged as controls
naive_ols_tipuach <- glm(avgmath ~ classize + tipuach, 
                         data = my_data)

# Add enrollment as control
naive_ols_tipuach_c_size <- glm(avgmath ~ classize + tipuach + c_size, 
                                data = my_data)

stargazer(naive_ols_no_controls, naive_ols_tipuach, naive_ols_tipuach_c_size, 
          title = "Doin' dumb stuff", align = TRUE)

##################
# 2)

# Get data with between 20 and 60 students (exclusive)
my_data_20_60 <- subset.data.frame(my_data, c_size > 20 & c_size < 60)

# Make dummy for predicted "large class" around 40
first_dis <- as.integer(my_data_20_60$c_size < 40)

# Add dummy to table (1 if > 40)
my_data_20_60_w_dummy <- cbind(my_data_20_60, first_dis)

# Sharp RDD at 40 student cutoff
sharp_rdd_20_60_ols <- glm(avgmath ~ first_dis + tipuach + c_size, 
                           data = my_data_20_60_w_dummy)

##################
# 3)

# Run Sharp RDD with loc lin regression
sharp_rdd_20_60_loc_lin <- RDestimate(avgmath ~ c_size,
                              data = my_data_20_60_w_dummy,
                              cutpoint = 40)

# Bootstrap to find standard errors


