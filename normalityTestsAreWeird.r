#a toy example showing how to implement boxcox transformations and normality tests using the 
#COND variable from the nla dataset

#notice how even transformed data that *look* normal still yield highly significant results when tested for departures from normality.

setwd('c:/users/brad griffith/dropbox/nla analyses/data')
nla<-read.csv('NLA.csv')

COND=nla$COND
par(mfrow=c(1,2))
#visual inspection
hist(COND)
qqnorm(COND)
qqline(COND)

library(fBasics)
library(nortest)
#normality tests
	shapiro.test(COND)
	jarqueberaTest(COND)
	dagoTest(COND)
	lillieTest(COND)
	pchiTest(COND)
	
#to output a p-value for one of these tests:
#could be worked into an automated script for using box-cox on all variables that fail the test
	lillieTest(COND)@test$p.value


logdat=log(nla$COND)
hist(logdat)
qqnorm(logdat)
qqline(logdat)
	shapiro.test(logdat)
	jarqueberaTest(logdat)
	dagoTest(logdat)
	lillieTest(logdat)
	pchiTest(logdat)

##boxcox transformations
library(MASS)
library(car)

#1st) get exponent for boxcox dist that minimizes deviation from normality
#for lambda=0, boxcox is same as log transformation

boxcox(COND~1) #I think the proper way might be to fit the actual glm like (COND~all landcover variables) and apply the boxcox function to that, but doing so didn't change the lambda estimate much

boxcox.res<-boxcox(COND~1,plotit=F)
max.lam=boxcox.res$x[boxcox.res$y==max(boxcox.res$y)] #extract best lambda value

#created transformed data
COND.boxcox<-bcPower(COND,lambda=max.lam)

#plot
hist(COND.boxcox)
qqnorm(COND.boxcox)
qqline(COND.boxcox)
	shapiro.test(COND.boxcox)
	jarqueberaTest(COND.boxcox)
	dagoTest(COND.boxcox)
	lillieTest(COND.boxcox)
	pchiTest(COND.boxcox)

