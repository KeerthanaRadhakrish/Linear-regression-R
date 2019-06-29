chem<-read.csv("hw5.data.csv")
chem

#1.	Plot concentration (Y) vs. time (X).
plot(chem$X,chem$Y,main="plot of concentration(Y) vs time(X)",xlab="time",ylab = "concentration",col=454)

#2.	Plot log of concentration ( log(Y) ) vs. time (X).
log(chem$Y)
plot(chem$X,log(chem$Y),main="plot of log(concentration)(Y) vs time(X)",xlab="time",ylab = "concentration",col=244)

#	Fitting the model M1and generating various plots to assess the regression assumptions to explain if any of the assumption(s) is violated.
M1<-lm(Y~X,data=chem)
summary(M1)
##there is no linear trend between Y and X.

##histogram
hist(resid(M1),col=8,breaks=15,freq=TRUE)

##plot the model
plot(chem$X,chem$Y,main="plot of concentration(Y) vs time(X)",xlab="time",ylab = "concentration",col=454)

##qq plot
qqnorm( residuals(M1),col=84)
qqline( residuals(M1), col = 14)
dev.off()

##residual vs predicted
plot(resid(M1)~fitted(M1),col=70,main="residual vs fitted values")

##residual vs X
plot(resid(M1)~X,data=chem,col=90,main="residual vs X")

##to check variance
plot(M1,3)
##the plot doesn't look like it has a random scatter.

#	Fitting model M2 and then, generate various plots to assess the regression assumptions to explain if any of the assumption(s) is violated.
M2<-lm(Y~X+I(X^2),data=chem)
summary(M2)

##histogram
hist(resid(M2),col=99)

##qq plot
qqnorm( residuals(M2),col=27)
qqline( residuals(M2), col = 18)

##Y vs X and Y vs I(X^2)
par(mfrow=c(1,2))
plot(Y~X,data=chem,col=490,main="Y vs X")
plot(Y~I(X^2),data=chem,col=579,main="Y vs X^2")

##residual vs predicted
plot(resid(M2)~fitted(M2),col=124)

##resid vs X and resid vs X^2
par(mfrow=c(1,2))
plot(resid(M2)~X,data=chem,col=76)
plot(resid(M2)~I(X^2),data=chem,col=51)
dev.off()

##variance
plot(M2,3)

##Fitting model M3 and then, generate various plots to assess the regression assumptions to explain if any of the assumption(s) is violated.
M3<-lm(log(Y)~X,data=chem) 
summary(M3)

##histogram
hist(resid(M3),col=64)

##qqplot
qqnorm( residuals(M3),col=108)
qqline( residuals(M3), col = 29)

##log(Y)vs X
plot(log(Y)~X,data=chem,col=459)

##residual vs predicted
plot(resid(M3)~fitted(M3))

##variance
plot(M3,3)

