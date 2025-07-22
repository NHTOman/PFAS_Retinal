library(data.table)
library(MASS)

mydata <- read.csv("D:/A_Science/Yunnan/Data.csv")
head(mydata)
names(mydata)
####RNFL####
result1=c()
for (i  in  c(728,729,727,731,724,730,723,726,732)){
  fit=lm(RNFL_OD补~mydata[,i]+factor(gender)+age+factor(ethnic)+BMI+factor(family)+factor(activity)+factor(smoke)+factor(drink)+time,data=mydata)
  result1=rbind(result1,c(colnames(mydata)[i],coef(summary(fit))[2,c(1,2,4)]))
}
result1
write.table(result1,file = paste("D:/A_Science/PFAS/Pre_Res/PFAS_RNFL.csv"), sep =",", row.names =F, col.names =TRUE, quote =TRUE)

####SRNFL####
result1=c()
for (i  in  c(728,729,727,731,724,730,723,726,732)){
  fit=lm(SupOD~mydata[,i]+factor(gender)+age+factor(ethnic)+BMI+factor(family)+factor(activity)+factor(smoke)+factor(drink)+time,data=mydata)
  result1=rbind(result1,c(colnames(mydata)[i],coef(summary(fit))[2,c(1,2,4)]))
}
result1
write.table(result1,file = paste("D:/A_Science/PFAS/Pre_Res/PFAS_SRNFL.csv"), sep =",", row.names =F, col.names =TRUE, quote =TRUE)
####TRNFL####
result1=c()
for (i  in  c(728,729,727,731,724,730,723,726,732)){
  fit=lm(TempOD~mydata[,i]+factor(gender)+age+factor(ethnic)+BMI+factor(family)+factor(activity)+factor(smoke)+factor(drink)+time,data=mydata)
  result1=rbind(result1,c(colnames(mydata)[i],coef(summary(fit))[2,c(1,2,4)]))
}
result1
write.table(result1,file = paste("D:/A_Science/PFAS/Pre_Res/PFAS_TRNFL.csv"), sep =",", row.names =F, col.names =TRUE, quote =TRUE)
####IRNFL####
result1=c()
for (i  in  c(728,729,727,731,724,730,723,726,732)){
  fit=lm(InfOD~mydata[,i]+factor(gender)+age+factor(ethnic)+BMI+factor(family)+factor(activity)+factor(smoke)+factor(drink)+time,data=mydata)
  result1=rbind(result1,c(colnames(mydata)[i],coef(summary(fit))[2,c(1,2,4)]))
}
result1
write.table(result1,file = paste("D:/A_Science/PFAS/Pre_Res/PFAS_IRNFL.csv"), sep =",", row.names =F, col.names =TRUE, quote =TRUE)
####NRNFL####
result1=c()
for (i  in  c(728,729,727,731,724,730,723,726,732)){
  fit=lm(NasOD~mydata[,i]+factor(gender)+age+factor(ethnic)+BMI+factor(family)+factor(activity)+factor(smoke)+factor(drink)+time,data=mydata)
  result1=rbind(result1,c(colnames(mydata)[i],coef(summary(fit))[2,c(1,2,4)]))
}
result1
write.table(result1,file = paste("D:/A_Science/PFAS/Pre_Res/PFAS_NRNFL.csv"), sep =",", row.names =F, col.names =TRUE, quote =TRUE)
