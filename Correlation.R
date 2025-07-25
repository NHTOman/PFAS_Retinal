#install.packages("corrplot", dependencies = FALSE)
library(corrplot)
df <- read.csv("D:/A_Science/Yunnan/PFAS.csv")
df <- na.omit(df)
df1 <- log10(df)
M <- cor(df1,method = "spearman")

testRes <- cor.mtest(df,conf.level = 0.95)
testRes1 <- cor.mtest(df,alternative = "two.side",method = "spearman",conf.level = 0.95)
corrplot.mixed(M, p.mat = testRes1$p, 
               lower = "number", 
               upper = 'color', 
               tl.pos = 'lt',
               tl.col = 'black', 
               sig.level = c(0.001, 0.01, 0.05), 
               pch.cex = 1, 
               insig = 'label_sig', 
               pch.col = 'black',
               diag = 'u',
               bg = "LightGray",
)
