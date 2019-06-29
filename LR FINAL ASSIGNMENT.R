WCGS<-read.csv("WCGSdata1.csv")
wcgs<-WCGS[,c(1:5)]
wcgs
nrow(wcgs)
nontypeA<-wcgs[which(wcgs$typeA==0),]
typeA<-wcgs[which(wcgs$typeA==1),]
## boxplot to determine relationship between sbp and personality types
boxplot(typeA$sbp,nontypeA$sbp,main="boxplots of sbp(mmhg) for type A",xlab="Type A",ylab="Systolic BP",col=c("red","purple"))

## scatterplots 
plot(wcgs$age,wcgs$sbp,main="scatterplot between SBP(mmhg) and Age(in years)",xlab = "AGE",ylab="SBP",col="green")
lines(lowess(wcgs$age,wcgs$sbp))
plot(wcgs$weight,wcgs$sbp,main="scatterplot between SBP(mmhg) and weight(in pounds)",xlab = "WEIGHT",ylab="SBP",col="blue")
lines(lowess(wcgs$weight,wcgs$sbp))

## linear regression models
mod1<-lm(sbp~+age+weight+typeA*age+typeA*weight+age*weight,data=wcgs)
summary(mod1)

mod2<-lm(sbp~typeA+age+weight+typeA*age+typeA*weight,data=wcgs)
summary(mod2)

mod3<-lm(sbp~typeA+age+weight+typeA*weight,data=wcgs)
summary(mod3)

mod4<-lm (sbp~typeA+age+weight,data=wcgs)
summary(mod4)

##checking model assumptions
par(mfrow=c(2,2)) 
plot(resid(mod4)~fitted(mod4),col=50,main="residual vs fitted values")
plot(resid(mod4)~weight,data=wcgs,col=99,main="residual vs weight")
plot(resid(mod4)~age,data=wcgs,col=65,main="residual vs age")
ad.test(resid(mod4),"pnorm")

qqnorm( residuals(mod4),col=27)
qqline( residuals(mod4), col = 18)

library(gmodels)
library(nortest)
library(lmtest)
estimable(mod4,c("typeA"=1),conf.int=0.95)

estimable(mod4,c("(Intercept)"=1,"typeA"=1,"age"=mean(wcgs$age),"weight"=mean(wcgs$weight)),conf.int=0.95)
estimable(mod4,c("(Intercept)"=1,"typeA"=0,"age"=mean(wcgs$age),"weight"=mean(wcgs$weight)),conf.int=0.95)

