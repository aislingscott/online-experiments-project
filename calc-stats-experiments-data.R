#Aisling Scott
#June 28 2016 
#Last updated: Jan 20 2017
#Takes shortened exps data and generates stats we're interested in 

setwd("/Users/aislingscott/Dropbox/online-experiments/data/")
library(data.table)
require(bit64) 
library(foreign)

#Import data
All <- read.dta("shorten_exps-pt2.dta")

#View Variable Names
names(All)
#rename variation_id_x 
names(All)[names(All)=="variation_id_x"] <- "variation_id"

#Change format to data tables to easily calculate the stats
require(data.table) 
setDT(All)

#Calculate cumulative sums of visitors and converters 
#Note for converters only exp and variation 
All[, cum_convert_var := cumsum(conversions), by=list(experiment_id, variation_id)]
All[, cum_visit_var := cumsum(visitors), by=list(experiment_id, variation_id, goal_id)]
All[, cum_convert_base := cumsum(conversions_base), by=list(experiment_id, variation_id, goal_id)]
All[, cum_visit_base := cumsum(visitors_base), by=list(experiment_id, variation_id, goal_id)]

#Calculate Conversion Rates
All$conversion_rate_var <- (All$cum_convert_var/All$cum_visit_var) 
All$conversion_rate_base <- (All$cum_convert_base/All$cum_visit_base) 

#Conversation rate becomes NA when visitors and conversions is zero
# Replace CR with 0 if this is the case
All$conversion_rate_var[which(All$cum_visit_var==0)] <-0
All$conversion_rate_base[which(All$cum_visit_base==0)] <-0

#Calculate Effect Size 
All$effect_size <- (All$conversion_rate_var-All$conversion_rate_base)
All$improvement <- (All$effect_size/All$conversion_rate_base)
#If cr_base is zero replace improvement with effect size
All$improvement[which(All$conversion_rate_base==0)] <- All$effect_size[which(All$conversion_rate_base==0)] 

#Calculate Sample Variance
All$s2_var <- (All$conversion_rate_var*(1-All$conversion_rate_var))/All$cum_visit_var
All$s2_base <- (All$conversion_rate_base*(1-All$conversion_rate_base))/All$cum_visit_base
#Calc standard error
All$se <- sqrt(All$s2_var+All$s2_base)
#Calc Sample Variance 
All$sample_variance <- All$s2_var+All$s2_base

#check if any are missing 
nrow(All[which(is.na(All$se)==TRUE)])
#if clear move on

#Calculate the test statistic
All$tstat <-All$effect_size/All$se
#check missings
nrow(All[which(is.na(All$tstat)==TRUE)])
#11772
#All of these are ones with se=NA or se=0 


#Calculate degrees of freedom in different parts (using Welch–Satterthwaite) 
All$df2 <- All$se^4
All$df4 <- (All$s2_var^2/(All$cum_visit_var-1)) 
All$df5 <- (All$s2_base^2/(All$cum_visit_base-1))
All$df5[is.na(All$df5)] <- 0
All$df4[is.na(All$df4)] <- 0
All$df <- All$df2/((All$df4)+ (All$df5))
df <- All$df

#Convert t-stat
All$t <- abs(All$tstat)
t <- abs(All$tstat)
#Calculate Confidence
All$confidence = pt(All$t, df=df)

#calculate pvalue 
#p-value calc the correct way 
All$real_pval <- 2*pt(-abs(t),df=df)
#Calc z-score 
All$zscore = pnorm(t)
#Count pvalues of 0
nrow(All[which(All$real_pval==0)])
#15947 observations with 0
nrow(All[is.na(All$real_pval)])


## Calculate Adjusted Pvalues
All$tau2 = (All$conversion_rate_base * (1-All$conversion_rate_base)  + All$conversion_rate_var * (1-All$conversion_rate_var) ) /100 
All$t = All$effect_size/All$se
All$SE_pval = (All$sample_variance / (All$sample_variance + All$tau2))^(-1/2) * exp(- All$tau2 / 2 / (All$sample_variance + All$tau2) * All$t^2)
#Calculate visitors remaining 
All$thet = abs(All$conversion_rate_var-All$conversion_rate_base)

All$V=(All$conversion_rate_base * (1-All$conversion_rate_base)  + All$conversion_rate_var * (1-All$conversion_rate_var) )


###Also calculate Power Calcs
#Za is the z score for two sided test at .05 significance
za=1.96
#zb is a constant for 80% power
zb=.8416 
All$crs <- (All$conversion_rate_base * (1-All$conversion_rate_base)) +(All$conversion_rate*(1-All$conversion_rate))
All$n_needed <- ((za+zb)^2*All$crs)/(All$effect_size)^2

#Save Data as CSV 
write.csv(All, "all-final.csv", row.names=F)