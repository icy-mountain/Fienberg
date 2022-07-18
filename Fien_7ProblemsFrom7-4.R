rm(list = ls(all.names = TRUE))
# data definition ####
table7_9 <- '
Proximity_Ai Norms_Bj Contact_Ck Sentiment_Dl Freq
Plus         Plus     Plus       Plus         77
Plus         Plus     Plus       Minus        32
Plus         Plus     Minus      Plus         14
Plus         Plus     Minus      Minus        19
Plus         Minus    Plus       Plus         30
Plus         Minus    Plus       Minus        36
Plus         Minus    Minus      Plus         15
Plus         Minus    Minus      Minus        27
Minus        Plus     Plus       Plus         43
Minus        Plus     Plus       Minus        20
Minus        Plus     Minus      Plus         27
Minus        Plus     Minus      Minus        36
Minus        Minus    Plus       Plus         36
Minus        Minus    Plus       Minus        37
Minus        Minus    Minus      Plus         41
Minus        Minus    Minus      Minus        118
'
# dataframe definition####
# Proximity_Ai Norms_Bj Contact_Ck Sentiment_Dl
SquashedByCol <- function(df, col) {
  colFactor <- factor(df[[col]])
  vals <- attributes(colFactor)$levels
  one <- vals[[1]]
  two <- vals[[2]]
  dfColValOne <- subset(df, colFactor == one)
  dfColValTwo <- subset(df, colFactor == two)
  ans <- dfColValOne[, !(names(df) %in% c(col))]
  ans$Freq <- dfColValOne$Freq + dfColValTwo$Freq
  return(ans)
}
df7_9 <- read.table(textConnection(table7_9),head=T)
model_ <- loglm(formula = Freq ~ ., data=df7_9)
modelAB_AC_AD_BC_BD_CD <- update(model_, .~.^2)
modelAB_AC_BC_BD_CD <- update(modelAB_AC_AD_BC_BD_CD, .~.-Proximity_Ai:Sentiment_Dl)
modelAB_AD_BC_BD_CD <- update(modelAB_AC_AD_BC_BD_CD, .~.-Proximity_Ai:Contact_Ck)
modelAC_AD_BC_BD_CD <- update(modelAB_AC_AD_BC_BD_CD, .~.-Proximity_Ai:Norms_Bj)
modelAB_AC_AD_BD_CD <- update(modelAB_AC_AD_BC_BD_CD, .~.-Norms_Bj:Contact_Ck)
modelAB_AC_AD_BC_CD <- update(modelAB_AC_AD_BC_BD_CD, .~.-Norms_Bj:Sentiment_Dl)
modelAB_AC_AD_BC_BD <- update(modelAB_AC_AD_BC_BD_CD, .~.-Contact_Ck:Sentiment_Dl)

summary(modelAB_AC_AD_BC_BD)#30.00436  6 3.923353e-05
summary(modelAB_AC_AD_BC_CD)#22.70871  6 0.0009001453
summary(modelAB_AC_AD_BD_CD)#12.31370  6 0.05532545
summary(modelAC_AD_BC_BD_CD)#14.96897  6 0.02049947
summary(modelAB_AD_BC_BD_CD)#45.91089  6 3.083984e-08
summary(modelAB_AC_BC_BD_CD)#2.530110  6 0.8650827 pick up!
summary(modelAB_AC_AD_BC_BD_CD)# 2.248026  5 0.8138694
