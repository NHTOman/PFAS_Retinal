#RCS
library(survival)
library(splines)
library(readxl)
library(rms)
library(ggpubr)
library(ggplot2)

mydata <- read.csv("D:/A_Science/Yunnan/【1686RCS】血常规污染物合并新代谢物数据(无QC)合并OCT.csv")
head(mydata)
names(mydata)

dd <- datadist(mydata)
options(datadist='dd')

head(mydata)
mydata$gender <- as.factor(mydata$gender)
mydata$ethnic <- as.factor(mydata$ethnic)
mydata$family <- as.factor(mydata$family)
mydata$activity <- as.factor(mydata$activity)
mydata$smoke <- as.factor(mydata$smoke)
mydata$drink <- as.factor(mydata$drink)
mydata$age <- as.numeric(mydata$age)
mydata$BMI <- as.numeric(mydata$BMI)
mydata$time <- as.numeric(mydata$time)
mydata$PFOA <- as.numeric(mydata$PFOA)
mydata$PFOS <- as.numeric(mydata$PFOS)
mydata$PFHxS <- as.numeric(mydata$PFHxS)
str(mydata)
names(mydata)
#PFOA
fit.rcs <- ols(RNFL_OD补 ~ rcs(PFOA, 3)
               + gender + ethnic + family + activity + smoke + drink
               + age + BMI + time
               , data = mydata)

fit.rcs
BIC(fit.rcs) 
anova(fit.rcs)

Beta <- Predict(fit.rcs, PFOA, type="predictions", ref.zero = T)
ggplot(Beta)
dd$limits$PFOA[2]


#PFOS
fit.rcs <- ols(RNFL_OD补 ~ rcs(PFOS, 3)
               + gender + ethnic + family + activity + smoke + drink
               + age + BMI + time
               , data = mydata)

fit.rcs

BIC(fit.rcs) 
anova(fit.rcs)

Beta <- Predict(fit.rcs, PFOS, type="predictions", ref.zero = T)
ggplot(Beta)
dd$limits$PFOS[2]

#PFHxS
fit.rcs <- ols(RNFL_OD补 ~ rcs(PFHxS, 3)
               + gender + ethnic + family + activity + smoke + drink
               + age + BMI + time
               , data = mydata)

fit.rcs
BIC(fit.rcs) 
anova(fit.rcs)

Beta <- Predict(fit.rcs, PFHxS, type="predictions", ref.zero = T)
ggplot(Beta)
dd$limits$PFHxS[2]