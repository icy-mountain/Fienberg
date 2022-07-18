# data definition####
table7_3 <- '
Sex IQ CP  PE  SES Freq
M   L  Yes Low  L   4
M   L  Yes Low  LM  2
M   L  Yes Low  UM  8
M   L  Yes Low  H   4
M   L  Yes High L   13
M   L  Yes High LM  27
M   L  Yes High UM  47
M   L  Yes High H   39
M   L  No  Low  L   349
M   L  No  Low  LM  232
M   L  No  Low  UM  166
M   L  No  Low  H   48
M   L  No  High L   64
M   L  No  High LM  84
M   L  No  High UM  91
M   L  No  High H   57
M   LM Yes Low  L   9
M   LM Yes Low  LM  7
M   LM Yes Low  UM  6
M   LM Yes Low  H   5
M   LM Yes High L   33
M   LM Yes High LM  64
M   LM Yes High UM  74
M   LM Yes High H   123
M   LM No  Low  L   207
M   LM No  Low  LM  201
M   LM No  Low  UM  120
M   LM No  Low  H   47
M   LM No  High L   72
M   LM No  High LM  95
M   LM No  High UM  110
M   LM No  High H   90
M   UM Yes Low L   12
M   UM Yes Low LM  12
M   UM Yes Low UM  17
M   UM Yes Low H   9
M   UM Yes High L  38
M   UM Yes High LM 93
M   UM Yes High UM 148
M   UM Yes High H  224
M   UM No Low L  126
M   UM No Low LM 115
M   UM No Low UM 92
M   UM No Low H  41
M   UM No High L  54
M   UM No High LM 92
M   UM No High UM 100
M   UM No High H  65
M   H Yes Low L   10
M   H Yes Low LM  17
M   H Yes Low UM  6
M   H Yes Low H   8
M   H Yes High L   49
M   H Yes High LM  119
M   H Yes High UM  198
M   H Yes High H   414
M   H No Low L   67
M   H No Low LM  79
M   H No Low UM  42
M   H No Low H   17
M   H No High L   43
M   H No High LM  59
M   H No High UM  73
M   H No High H   54
F   L  Yes Low  L   5
F   L  Yes Low  LM  11
F   L  Yes Low  UM  7
F   L  Yes Low  H   6
F   L  Yes High L   9
F   L  Yes High LM  29
F   L  Yes High UM  36
F   L  Yes High H   36
F   L  No  Low  L   454
F   L  No  Low  LM  285
F   L  No  Low  UM  163
F   L  No  Low  H   50
F   L  No  High L   44
F   L  No  High LM  61
F   L  No  High UM  72
F   L  No  High H   58
F   LM Yes Low  L   5
F   LM Yes Low  LM  19
F   LM Yes Low  UM  13
F   LM Yes Low  H   5
F   LM Yes High L   14
F   LM Yes High LM  47
F   LM Yes High UM  75
F   LM Yes High H   110
F   LM No  Low  L   312
F   LM No  Low  LM  236
F   LM No  Low  UM  193
F   LM No  Low  H   70
F   LM No  High L   47
F   LM No  High LM  88
F   LM No  High UM  90
F   LM No  High H   76
F   UM Yes Low L   8
F   UM Yes Low LM  12
F   UM Yes Low UM  12
F   UM Yes Low H   12
F   UM Yes High L  20
F   UM Yes High LM 62
F   UM Yes High UM 91
F   UM Yes High H  230
F   UM No Low L  216
F   UM No Low LM 164
F   UM No Low UM 174
F   UM No Low H  48
F   UM No High L  35
F   UM No High LM 85
F   UM No High UM 100
F   UM No High H  81
F   H Yes Low L   13
F   H Yes Low LM  15
F   H Yes Low UM  20
F   H Yes Low H   13
F   H Yes High L   28
F   H Yes High LM  72
F   H Yes High UM  142
F   H Yes High H   360
F   H No Low L   96
F   H No Low LM  113
F   H No Low UM  81
F   H No Low H   49
F   H No High L   24
F   H No High LM  50
F   H No High UM  77
F   H No High H   98
'
#A->SES, B->IQ, C->Sex, D->PE, E->CP 
# code section####
df7_3 <- read.table(textConnection(table7_3),head=T)
df7_3toOrdered <- function(df7_3) {
  four_level <- gl(4, 1, 4)
  levels(four_level)[1] = "L"
  levels(four_level)[2] = "LM"
  levels(four_level)[3] = "UM"
  levels(four_level)[4] = "H"
  ordered_SES <- factor(df7_3$SES, levels=four_level)
  ordered_IQ  <- factor(df7_3$IQ, levels=four_level)
  df7_3Ordered <- df7_3
  df7_3Ordered$IQ <- ordered_IQ
  df7_3Ordered$SES <- ordered_SES
  return(df7_3Ordered)
}
df7_3Ordered <- df7_3toOrdered(df7_3)
CPPlus <- subset(df7_3Ordered, CP == "Yes")
CPMinus <- subset(df7_3Ordered, CP == "No")
CPFreqSum <- CPPlus$Freq + CPMinus$Freq 
df7_3OrderedABCD <- subset(CPPlus, select=-c(CP))
df7_3OrderedABCD$Freq <- CPFreqSum
df7_3OrderedABCDLow <- subset(df7_3OrderedABCD, PE == "Low")
df7_3OrderedABCDHigh <- subset(df7_3OrderedABCD, PE == "High")
df7_3OrderedABCDEYes <- subset(df7_3Ordered, CP == "Yes")
df7_3OrderedABCDENo <- subset(df7_3Ordered, CP == "No")

