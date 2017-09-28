#Aisling Scott
# July 8 2016
# Make histograms for before and after platform change 

setwd("/Users/aislingscott/Dropbox/Upwork/")
library(data.table)
require(bit64) 
library(foreign)

All <- fread("all-final.csv")


#subset data
before <- All[which(All$before==0),]
after <- All[which(All$before==1),]



#Analysis 
#p-values 
hist(after$p.value, col=rgb(0, 1, 0, 0.5), breaks=20)
hist(before$p.value,  add=T,  col=rgb(0,0,1,0.5), breaks=20)
library(pastecs)
mean(after$p.value)
mean(before$p.value)
#Regression
fit1 <- lm(All$p.value ~ All$before , data=All)
fit1
summary(fit1)

#total visitors 
hist(after$total_vis, freq=FALSE, col=rgb(0, 1, 0, 0.5))
hist(before$total_vis, freq=FALSE,  add=T,  col=rgb(0,0,1,0.5))
mean(after$total_vis)
mean(before$total_vis)
