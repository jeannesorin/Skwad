setwd("~/Documents/Empirical Analysis III - Mogstad/PSets")
library(foreign)
library(xtable)
library(sandwich)
library(lmtest)
library(MatchIt)
library(dplyr)
library(ggplot2)
library(magrittr)
library(Hmisc)
rm(list=ls())

#Read in data set
data = read.dta("lalonde2.dta")
#Define set of variables for use in parts a & b
vars=c(2:6,10,13,15:17)
#Perform one-way ANOVA on each observable variables between treatment and control groups
test_randomization=function(var){
  t.test(data[,var]~data$treated)
}
tt_results=lapply(vars,test_randomization)
tt_results

#Regress outcome of interest on treatment with and without controls
RCT_no_contr=lm(re78~treated, data=data)
coeftest(RCT_no_contr, vcov=vcovHC(RCT_no_contr,tpye="HC1"))
latex(coeftest(RCT_no_contr, vcov=vcovHC(RCT_no_contr,tpye="HC1")), file="PS2_Q3_B_no_cont.tex", digits=3, where="h")
RCT_control=lm(re78~treated+age+educ+black+married+nodegree+re74+hisp+kids18+kidmiss+early_ra, data=data)
coeftest(RCT_control, vcov=vcovHC(RCT_control,tpye="HC1"))
latex(coeftest(RCT_control, vcov=vcovHC(RCT_control,tpye="HC1")), file="PS2_Q3_B_cont.tex", digits=3, where="h")

#Modify data to represent members of CPS group as untreated and exclude untreated from NSW from future analyses
data_c_thru_f=data[data$sample=="CPS"|(data$sample=="NSW"&data$treated==1),]
data_c_thru_f[is.na(data_c_thru_f)]=0

#Regress outcome on treated from NSW vs CPS control with and without controls
NSW_v_CPS_no_contr=lm(re78~treated, data=data_c_thru_f)
coeftest(NSW_v_CPS_no_contr, vcov=vcovHC(NSW_v_CPS_no_contr,tpye="HC1"))
latex(coeftest(NSW_v_CPS_no_contr, vcov=vcovHC(NSW_v_CPS_no_contr,tpye="HC1")), file="PS2_Q3_C_no_cont.tex", digits=3, where="h")
NSW_v_CPS_control=lm(re78~treated+age+educ+black+married+nodegree+metro+re74+hisp+kids18+kidmiss, data=data_c_thru_f)
coeftest(NSW_v_CPS_control, vcov=vcovHC(NSW_v_CPS_control,tpye="HC1"))
latex(coeftest(NSW_v_CPS_control, vcov=vcovHC(NSW_v_CPS_control,tpye="HC1")), file="PS2_Q3_C_cont.tex", digits=3, where="h")

#Perform one-way ANOVA on each observable variables between NSW treatment and CPS samples
vars=c(2:6,10,13,15:16)#CPS data is missing the early_ra variable
test_randomization=function(var){
  t.test(data_c_thru_f[,var]~data_c_thru_f$treated)
}
tt_results_out_samp=lapply(vars,test_randomization)
tt_results_out_samp

#Utilizing nearest neighbor propensity score matching
##First run a probit on being treated with the desired matching vars to "manually" obtain p-scores
prop_logit = glm(treated ~ age+educ+black+married+nodegree+metro+re74+hisp+kids18+kidmiss,
            family = binomial(link = "probit"), data = data_c_thru_f)
prop_score = data.frame(pr_score = predict(prop_logit, type = "response"),
                     treated = prop_logit$model$treated,age = prop_logit$model$age,educ = prop_logit$model$educ,
                     black = prop_logit$model$black,married = prop_logit$model$married,nodegree = prop_logit$model$nodegree,
                     re74 = prop_logit$model$re74,hisp = prop_logit$model$hisp,kids18 = prop_logit$model$kids18,
                     kidmiss = prop_logit$model$kidmiss, metro = prop_logit$model$metro)

##Make histograms of pscore vs being treated
jpeg("PS2_Q3_e_treated.jpg")
hist(prop_score[prop_score$treated==1,]$pr_score, main="Pr Scores For Treated Group", xlab="Pr Score", col="orange")
dev.off()
jpeg("PS2_Q3_e_untreated.jpg")
hist(prop_score[prop_score$treated==0,]$pr_score, main="Pr Scores For Untreated Group", xlab="Pr Score", col="pink")
dev.off()

##Idenitfy nearest neighbors based on p scores, without replacement (default option for MatchIt)
near_match = matchit(treated ~ age+educ+black+married+nodegree+re74+hisp+kids18+kidmiss+metro,
                     method = "nearest", data = data_c_thru_f, distance="probit", discard="both")
near_match_data=match.data(near_match)

##Perform regressions with & without controls on the matched up (without replacement) data
near_match_no_contr=lm(re78~treated, data=near_match_data)
coeftest(near_match_no_contr, vcov=vcovHC(near_match_no_contr,tpye="HC1"))
latex(coeftest(near_match_no_contr, vcov=vcovHC(near_match_no_contr,tpye="HC1")), file="PS2_Q3_E_no_cont.tex", digits=3, where="h")
near_match_control=lm(re78~treated+age+educ+black+married+nodegree+re74+hisp+kids18+kidmiss, data=near_match_data)
coeftest(near_match_control, vcov=vcovHC(near_match_control,tpye="HC1"))
latex(coeftest(near_match_control, vcov=vcovHC(near_match_control,tpye="HC1")), file="PS2_Q3_E_cont.tex", digits=3, where="h")

##Idenitfy nearest neighbors based on p scores, with replacement
near_match_wr = matchit(treated ~ age+educ+black+married+nodegree+re74+hisp+kids18+kidmiss+metro,
                     method = "nearest", data = data_c_thru_f, distance="probit", discard="both",
                     replace=TRUE)
near_match_data_wr=match.data(near_match_wr)

##Perform regressions with & without controls on the matched up (with replacement) data
near_match_no_contr_wr=lm(re78~treated, data=near_match_data_wr, weights=(near_match_data_wr$weights))
coeftest(near_match_no_contr_wr, vcov=vcovHC(near_match_no_contr_wr,tpye="HC1"))
latex(coeftest(near_match_no_contr_wr, vcov=vcovHC(near_match_no_contr_wr,tpye="HC1")), file="PS2_Q3_E_no_cont_wr.tex", digits=3, where="h")
near_match_control_wr=lm(re78~treated+age+educ+black+married+nodegree+re74+hisp+kids18+kidmiss, data=near_match_data_wr, weights=(near_match_data_wr$weights))
coeftest(near_match_control_wr, vcov=vcovHC(near_match_control_wr,tpye="HC1"))
latex(coeftest(near_match_control_wr, vcov=vcovHC(near_match_control_wr,tpye="HC1")), file="PS2_Q3_E_cont_wr.tex", digits=3, where="h")

