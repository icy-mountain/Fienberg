
# data definition####
table7_2 <- '
FirstMembership FirstAttitude SecondMembership SecondAttitude Freq
plus            plus          plus             plus           458
plus            plus          plus             minus          140
plus            plus          minus            plus           110
plus            plus          minus            minus          49

plus            minus         plus             plus           171
plus            minus         plus             minus          182
plus            minus         minus            plus           56
plus            minus         minus            minus          87

minus           plus          plus             plus           184
minus           plus          plus             minus          75
minus           plus          minus            plus           531
minus           plus          minus            minus          281

minus           minus         plus             plus           85
minus           minus         plus             minus          97
minus           minus         minus            plus           338
minus           minus         minus            minus          554
'
# code section####
library(dplyr)
df7_2 <- read.table(textConnection(table7_2),head=T)
# data for ABC
SecAttitude_plus <- df7_2 %>% filter(SecondAttitude == "plus")
SecAttitude_minus <- df7_2 %>% filter(SecondAttitude == "minus")
merged_SecAttitudeFreq <- SecAttitude_plus$Freq + SecAttitude_minus$Freq
df7_2ABC <- subset(SecAttitude_plus, select=-c(SecondAttitude))
df7_2ABC$Freq <- merged_SecAttitudeFreq
Extract_2ndMemship_plus <- function(my_table) {
  return(subset(my_table, SecondMembership == "plus"))
}
Extract_2ndMemship_minus <- function(my_table) {
  return(subset(my_table, SecondMembership == "minus"))
}
df7_2ABCPlus <- Extract_2ndMemship_plus(df7_2ABC)
df7_2ABCMinus <- Extract_2ndMemship_minus(df7_2ABC)
# data for ABCD
df7_2ABCDPlus <- subset(df7_2, SecondAttitude == "plus")
df7_2ABCDMinus <- subset(df7_2, SecondAttitude == "minus")
