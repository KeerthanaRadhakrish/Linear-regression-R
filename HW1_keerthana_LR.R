bp<-read.csv("hw.data.csv")
bp
#1 SBP and QUETELET INDEX
#x-variable= quet
#y-variable= sbp
## using scatterplot to determine the relationship between quetlet index and SBP

plot(bp$QUET,bp$SBP,main="plot showing correlation between quetelet index and SBP(in mmhg.)",xlab="quetlet index",ylab="systolic BP in mmhg.",pch=19,col="dark green")
abline(lm(bp$SBP~bp$QUET))
##finding correlation coefficient 

cor.test(bp$QUET,bp$SBP,alternative = "two.sided",method="pearson")
#correlation coefficient (r) = 0.7420041

## using lm function
mod<-lm(SBP~QUET,data=bp)
summary(mod)
##
plot(bp$QUET,bp$SBP,main="plot showing correlation between quetelet index and SBP(in mmhg.)",xlab="quetlet index",ylab="systolic BP in mmhg.",pch=19,col="dark green")
abline(mod)
#(or)
abline(lm(bp$SBP~bp$QUET,data=bp))
## prediction lines 
newx <- data.frame(QUET=seq(2,5,1))
plot(SBP~QUET, data=bp, ylim=c(120,180),col="dark green",pch=19)
lines(newx$QUET, predict(mod, newx, interval="predict")[,2], type="l", col="blue", lwd=2)
lines(newx$QUET, predict(mod, newx, interval="predict")[,3], type="l", col="blue", lwd=2)
