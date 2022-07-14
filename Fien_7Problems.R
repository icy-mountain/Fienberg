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

# ####
# A->FirstVoteIntention B->FirstCandidateOpinion 
# C->SecondVoteIntention D->SecondCandidateOpinion
df7_8 <- read.table(textConnection(table7_8),head=T)
