rm(list = ls(all.names = TRUE))
library(MASS)
source("./Fien_7-3Data.R")
#### for ABCD
#A->SES, B->IQ, C->Sex, D->PE, E->CP
#[ABC][ABD][ACD][BCD]
model6_glm <- glm(formula = Freq ~ 
                    SES:IQ:Sex + 
                    SES:IQ:PE +
                    SES:Sex:PE +
                    IQ:Sex:PE,
                  data = df7_3OrderedABCD, family = poisson)
model6 <- loglm(formula = Freq ~ SES:IQ:Sex + 
                  SES:IQ:PE +
                  SES:Sex:PE +
                  IQ:Sex:PE,
                data=df7_3OrderedABCD)
model6_logit <- loglm(formula = (df7_3OrderedABCDLow$Freq / df7_3OrderedABCDHigh$Freq) ~
                        PE + SES:PE + IQ:PE + Sex:PE +
                        SES:IQ:PE + SES:Sex:PE + IQ:Sex:PE,
                      data = df7_3OrderedABCDLow)
model6_logit[["param"]]
#### for ABCDE
#[ABCD][BCE][AE][DE]
model9 <- loglm(formula = Freq ~ SES:IQ:Sex:PE +
                  IQ:Sex:CP + SES:CP + PE:CP, data = df7_3Ordered)
model9_logit <- loglm(formula = (df7_3OrderedABCDEYes$Freq / df7_3OrderedABCDENo$Freq) ~
                        CP + SES:CP + IQ:CP + Sex:CP + PE:CP + IQ:Sex:CP,
                      data = df7_3OrderedABCDEYes)
model9_logit[["param"]]
