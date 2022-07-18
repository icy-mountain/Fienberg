rm(list = ls(all.names = TRUE))
# data definition ####
table7_8 <- '
FirstVoteIntention FirstCandidateOpinion SecondVoteIntention SecondCandidateOpinion Freq
Republicon         For                   Republicon          For                    129
Republicon         For                   Republicon          Against                3
Republicon         For                   Democrat            For                    1
Republicon         For                   Democrat            Against                2

Republicon         Against               Republicon          For                    11
Republicon         Against               Republicon          Against                23
Republicon         Against               Democrat            For                    0
Republicon         Against               Democrat            Against                1

Democrat           For                   Republicon          For                    1
Democrat           For                   Republicon          Against                0
Democrat           For                   Democrat            For                    12
Democrat           For                   Democrat            Against                11

Democrat           Against               Republicon          For                    1
Democrat           Against               Republicon          Against                1
Democrat           Against               Democrat            For                    2
Democrat           Against               Democrat            Against                68
'

# dataframe definition####
# A->FirstVoteIntention B->FirstCandidateOpinion 
# C->SecondVoteIntention D->SecondCandidateOpinion
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
df7_8 <- read.table(textConnection(table7_8),head=T)
df7_8ABC <- SquashedByCol(df7_8, "SecondCandidateOpinion")
df7_8AB <- SquashedByCol(df7_8ABC, "SecondVoteIntention")
df7_8ABC2ndVIRepub <- df7_8ABC[df7_8ABC$SecondVoteIntention == "Republicon",]
df7_8ABC2ndVIDemoc <- df7_8ABC[df7_8ABC$SecondVoteIntention == "Democrat",] 
df7_8ABCD2ndCOFor <- df7_8[df7_8$SecondCandidateOpinion == "For",]
df7_8ABCD2ndCOAga <- df7_8[df7_8$SecondCandidateOpinion == "Against",]
df7_8ABCD2ndCOFor[df7_8ABCD2ndCOFor$Freq == 0,]$Freq <- 1
df7_8ABCD2ndCOAga[df7_8ABCD2ndCOAga$Freq == 0,]$Freq <- 1
# vx=matrix(df7_8AB$Freq,nrow=2,byrow=T)
# chisq.test(vx)
# X^2_{0.05}(1) = 3.84 
# [AB]####
modelAB_AC_BC <- loglm(formula = Freq ~ FirstVoteIntention:FirstCandidateOpinion +
                         FirstVoteIntention:SecondVoteIntention +
                         FirstCandidateOpinion:SecondVoteIntention,
                data=df7_8ABC)
modelAB_AC <- update(modelAB_AC_BC,
                     .~.-FirstCandidateOpinion:SecondVoteIntention)
# modelAB_BC <- update(modelAB_AC_BC,
#                      .~.-FirstVoteIntention:SecondVoteIntention)
# modelAC_BC <- update(modelAB_AC_BC,
#                      .~.-FirstVoteIntention:FirstCandidateOpinion)
# summary(modelAC_BC)     #lrt=7.144975 df=2 P=0.028
# summary(modelAB_BC)     #lrt=213.8826 df=2 P=0
summary(modelAB_AC)     #lrt=0.1543353 df=2 P=0.926 pick up!!
summary(modelAB_AC_BC)  #lrt=0.0142554 df=1 P=0.905
# logit [AB]####
modelAB_AClogit <- loglm(formula = (df7_8ABC2ndVIRepub$Freq / df7_8ABC2ndVIDemoc$Freq) ~
                           SecondVoteIntention +
                           FirstVoteIntention:SecondVoteIntention,
                         data=df7_8ABC2ndVIRepub)
summary(modelAB_AClogit)
# modelAB_AC[["param"]][["FirstVoteIntention.SecondVoteIntention"]]
# [ABC]####
modelABC_AD_BD_CD <- loglm(formula = Freq ~
                             FirstVoteIntention:FirstCandidateOpinion:SecondVoteIntention +
                         FirstVoteIntention:SecondCandidateOpinion +
                         FirstCandidateOpinion:SecondCandidateOpinion +
                         SecondVoteIntention:SecondCandidateOpinion,
                       data=df7_8)
# modelABC_AD_BD <- update(modelABC_AD_BD_CD,
#                      .~.-SecondVoteIntention:SecondCandidateOpinion)
# modelABC_AD_CD <- update(modelABC_AD_BD_CD,
#                      .~.-FirstCandidateOpinion:SecondCandidateOpinion)
modelABC_BD_CD <- update(modelABC_AD_BD_CD,
                         .~.-FirstVoteIntention:SecondCandidateOpinion)
summary(modelABC_BD_CD)   #lrt=1.453260 df=5 P=0.9183997 pick up!!!
# summary(modelABC_AD_CD)   #lrt=103.2223 df=5 P=0
# summary(modelABC_AD_BD)   #lrt=16.96661 df=5 P=4.563560e-03
# summary(modelABC_AD_BD_CD)#lrt=0.709475 df=4 P=0.9501554
# logit [ABC]####
modelABC_BD_CDlogit <- loglm(formula = (df7_8ABCD2ndCOFor$Freq / df7_8ABCD2ndCOAga$Freq) ~
                               SecondCandidateOpinion +
                               FirstCandidateOpinion:SecondCandidateOpinion +
                               SecondVoteIntention:SecondCandidateOpinion,
                           data=df7_8ABCD2ndCOFor)
summary(modelABC_BD_CDlogit)
# modelABC_AD_BD_CD[["param"]][["FirstCandidateOpinion.SecondCandidateOpinion"]]
# modelABC_AD_BD_CD[["param"]][["SecondVoteIntention.SecondCandidateOpinion"]]