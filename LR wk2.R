### creating linear regression models for different variables in R and finding correlation among variables
## y variable : SBP

BP<-read.csv("hw2.data.csv")

par(mfrow=c(1,3))
plot(BP$AGE,BP$SBP,main="AGE vs SBP",xlab="AGE(yrs)",ylab="SBP",col="green")
plot(BP$SMK,BP$SBP,main="SMK vs SBP",xlab="SMK",ylab="SBP",col="red")
plot(BP$QUET,BP$SBP,main="QUET vs SBP",xlab="QUET",ylab="SBP",col="blue")

mod.full <- lm(SBP~ AGE + SMK + QUET, data=BP)
summary(mod.full)

mod.one<-lm(SBP~SMK+QUET,data=BP)
anova(mod.full,mod.one)

mod.two<-lm(SBP~AGE+QUET,data = BP)
anova(mod.full,mod.two)

mod.three<-lm(SBP~AGE+SMK,data=BP)
anova(mod.full,mod.three)

