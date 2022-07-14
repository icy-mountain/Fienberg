source("./Fien_7-2Data.R")
library(MASS)
#A (i) -> FirstMembership
#B (j) -> FirstAttitude
#C (k) -> SecondMembership
#D (l) -> SecondAttitude
# model for ABC ####
# [AB][AC][BC]
model1 <- loglm(formula = Freq ~ FirstMembership:FirstAttitude +
                  FirstMembership:SecondMembership +
                  FirstAttitude:SecondMembership, data = df7_2ABC)
model1_logitM <- loglm(formula = (df7_2ABCMinus$Freq / df7_2ABCPlus$Freq) ~
                        SecondMembership +
                        FirstMembership:SecondMembership +
                        FirstAttitude:SecondMembership,
                      data=df7_2ABCMinus)
model1_logitP <- loglm(formula = (df7_2ABCPlus$Freq / df7_2ABCMinus$Freq) ~
                        SecondMembership +
                        FirstMembership:SecondMembership +
                        FirstAttitude:SecondMembership,
                      data=df7_2ABCPlus)
mhatij1 <- fitted(model1_logitM)
mhatij2 <- fitted(model1_logitP)

#model for ABCD ####
model5_logitM <- loglm(formula = (df7_2ABCDMinus$Freq / df7_2ABCDPlus$Freq) ~
                        SecondAttitude +
                        FirstAttitude:SecondAttitude +
                        SecondMembership:SecondAttitude,
                      data=df7_2ABCDMinus)
model5_logitP <- loglm(formula = (df7_2ABCDPlus$Freq / df7_2ABCDMinus$Freq) ~
                        SecondAttitude +
                        FirstAttitude:SecondAttitude +
                        SecondMembership:SecondAttitude,
                      data=df7_2ABCDPlus)
mhatijk1 <- fitted(model5_logitM)
mhatijk2 <- fitted(model5_logitP)
mhatijk1
