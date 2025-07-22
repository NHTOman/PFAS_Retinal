####Baseline information####
mydata <- read.csv("D:/A_Science/Yunnan/Data.csv")
head(mydata)
str(mydata)
mymydata <- mydata %>% 
  transmute(Age = age, 
            Gender = factor(gender, levels = c(1, 2), labels = c("Men", "Women")),
            BMI = BMI,
            Ethnic = factor(ethnic, levels = c(1, 0),
                            labels = c("Han",
                                       "Non-Han")),
            Homeaddress = factor(home, levels = c(1, 2, 3), 
                                 labels = c("City",
                                            "Town",
                                            "Country")),
            Familycondition = factor(family, levels = c(1, 2, 3),
                                     labels = c("Good",
                                                "Moderate",
                                                "Poor")),
            Smokestatus = factor(smoke, levels = c(2, 1), labels = c("No", "Yes")),
            Alcoholdrinkingstatus = factor(drink, levels = c(0, 1), labels = c("No", "Yes")),
            Screentime = time,
            GlobalRNFLthickness = RNFL_OD,
            SuperiorRNFLthickness = SupOD,
            InferiorRNFLthickness = InfOD,
            NasalRNFLthickness = NasOD,
            TemporalRNFLthickness = TempOD,
            RNFLsymmetry = RNFLsymmetry,
            Rimarea = RV_OD,
            Discarea = DA_OD,
            Averagecupdiscarearatio = A_CD_OD,
            Verticalcupdiscarearatio = V_CD_OD,
            Cupvolume = CV_OD)

str(mymydata)
mymydata$Screentime <- as.numeric(mymydata$Screentime)
mymydata %>% tbl_summary(by = Gender) %>% add_p()

BB <- mymydata %>% 
  tbl_summary(by = Gender,
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 2),
              type = list(all_categorical() ~ "categorical"),
  ) %>% 
  add_p(test = list(all_continuous() ~ "t.test"),
        pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  bold_labels()
BB 
mytable <- as_flex_table(BB)

save_as_docx(mytable, path = "D:/A_Science/PFAS/Pre_Res/Basictable.docx")
reset_gtsummary_theme()