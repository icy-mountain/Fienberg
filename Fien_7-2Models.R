source("./Fien_7-2Data.R")
library(MASS)
# [AB][AC][BC]
model1 <- loglm(formula = Freq ~ FirstMembership:FirstAttitude +
                  FirstMembership:SecondMembership +
                  FirstAttitude:SecondMembership, data = df7_2ABC)
model1_logit <- loglm(formula = (df7_2ABCPlus$Freq / df7_2ABCMinus$Freq) ~
                        SecondMembership +
                        FirstMembership:SecondMembership +
                        FirstAttitude:SecondMembership,
                      data=df7_2ABCPlus)
fitted(model1_logit)

#model for ABCD
model5_logit <- loglm(formula = (df7_2ABCDPlus$Freq / df7_2ABCDMinus$Freq) ~
                        SecondAttitude +
                        FirstAttitude:SecondAttitude +
                        SecondMembership:SecondAttitude,
                      data=df7_2ABCDPlus)
