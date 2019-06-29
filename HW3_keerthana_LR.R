bp<-read.csv("hw.data.csv")
bp
#M1: SBP ~ AGE
#M2: SBP ~ AGE + QUET + SMK
##1
plot(SBP~AGE,data=bp)
M1<-lm(SBP~AGE,data=bp)
summary(M1)
abline(M1)
par(mfrow=c(1,3))
plot(SBP~AGE,data=bp,main="SBP vs AGE",col="blue")
plot(SBP~QUET,data=bp,main="SBP vs QUET",col="red")
plot(SBP~SMK,data=bp,main="SBP vs SMK",col="green")
M2<-lm(SBP ~ AGE + QUET + SMK,data=bp)
summary(M2)
M2<- lm(SBP ~ AGE + QUET + SMK,data=bp)
M1<-lm(SBP~AGE,data=bp)
anova(M1,M2)
M1<-lm(SBP~AGE,data=bp)
summary(M1)
M2<-lm(SBP~AGE+QUET+SMK,data=bp)
summary(M2)
1.6045-1.2127
0.3918*100
#4.Is the relationship between SBP and AGE confounded by SMK if QUET is dropped from M2?  Name this model M3.
M3<-lm(SBP~AGE+SMK,data=bp)
M1<-lm(SBP~AGE,data=bp)
summary(M3)
1.7092 - 1.6045
M4<-lm(SBP~AGE+QUET,data=bp)
summary(M4)
1.6045- 1.0452 

#Create an age group variable (agegrp) according to the following classification:
#agegrp=1	if the subject is in his/her 40's
#agegrp=2	if the subject is in his/her 50's
#agegrp=3	if the subject is in his/her 60's

bp$agegrp <- NA
bp$agegrp[which(bp$AGE<50)] <- "agegrp1" 
bp$agegrp[which(bp$AGE>=50 & bp$AGE<60)]<-"agegrp2"
bp$agegrp[which(bp$AGE>=60)]<-"agegrp3"
m5 <-lm(SBP ~ agegrp , data=bp)
summary(m5)

la<-read.csv("lab.data.csv")
la
data$urban.cat <- NA
data$urban.cat[which(data$URBAN <0.75)] <- "low"
data$urban.cat[which(data$URBAN >=0.75 & data$URBAN <0.85)] <- "middle"
data$urban.cat[which(data$URBAN >=0.85)] <- "high"
high
data$urban.cat <- relevel(factor(data$urban.cat), ref="low")
m1 <- lm(OWNEROCC ~ OWNCOST, data=subset(data, urban.cat=="low"))
m2 <- lm(OWNEROCC ~ OWNCOST, data=subset(data, urban.cat=="middle"))
m3 <- lm(OWNEROCC ~ OWNCOST, data=subset(data, urban.cat=="high"))