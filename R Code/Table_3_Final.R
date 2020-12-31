#Table 3 - Positive Symptomatic vs. Positive Asymptomatic 
#Comparison of IgM, IgA, and IgG titers against SARS-CoV-2 spike 1 glycoprotein (S1 SP), spike 2 glycoprotein (S2 SP), receptor binding domain (RBD), and nucleoprotein (NP) in symptomatic and asymptomatic COVID-19-positive participants. 

######
#Libraries used 
library(plotrix)
library(plyr)

#######
#Sample Size
table(POS$Group)

#Age 
aggregate(POS$Age,by=list(POS$Group),FUN=mean,na.rm=TRUE)
aggregate(POS$Age,by=list(POS$Group),FUN=range,na.rm=TRUE)
aggregate(POS$Age,by=list(POS$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(Age ~ Group, data = POS, var.equal = TRUE)
res

#Group #'s 
table(POS$Gender[POS$Group=="Positive (Symptomatic)"])
table(POS$Gender[POS$Group=="Positive (Asymptomatic)"])

table(POS$Gender[POS$Group=="Negative (Symptomatic)"])
table(POS$Gender[POS$Group=="Negative (Asymptomatic)"])

tbl = table(POS$Group, POS$Gender) 
tbl
fisher.test(tbl)

#BMI 
aggregate(POS$BMI,by=list(POS$Group),FUN=mean,na.rm=TRUE)
aggregate(POS$BMI,by=list(POS$Group),FUN=range,na.rm=TRUE)
aggregate(POS$BMI,by=list(POS$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(BMI ~ Group, data = POS, var.equal = TRUE)
res

#Titers - Symptomatic vs. Asymptomatic 
aggregate(S1_Spike_IgM_Pos$S1_Spike_IgM_Titer,by=list(S1_Spike_IgM_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(S1_Spike_IgM_Pos$S1_Spike_IgM_Titer,by=list(S1_Spike_IgM_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(S1_Spike_IgM_Pos$S1_Spike_IgM_Titer,by=list(S1_Spike_IgM_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(S1_Spike_IgM_Titer ~ Group_Pos, data = S1_Spike_IgM_Pos, var.equal = TRUE)
res

aggregate(S2_Spike_IgM_Pos$S2_Spike_IgM_Titer,by=list(S2_Spike_IgM_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(S2_Spike_IgM_Pos$S2_Spike_IgM_Titer,by=list(S2_Spike_IgM_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(S2_Spike_IgM_Pos$S2_Spike_IgM_Titer,by=list(S2_Spike_IgM_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(S2_Spike_IgM_Titer ~ Group_Pos, data = S2_Spike_IgM_Pos, var.equal = TRUE)
res

aggregate(Nucleoprotein_IgM_Pos$Nucleoprotein_IgM_Titer,by=list(Nucleoprotein_IgM_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(Nucleoprotein_IgM_Pos$Nucleoprotein_IgM_Titer,by=list(Nucleoprotein_IgM_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(Nucleoprotein_IgM_Pos$Nucleoprotein_IgM_Titer,by=list(Nucleoprotein_IgM_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(Nucleoprotein_IgM_Titer ~ Group_Pos, data = Nucleoprotein_IgM_Pos, var.equal = TRUE)
res

aggregate(RBD_IgM_Pos$RBD_IgM_Titer,by=list(RBD_IgM_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(RBD_IgM_Pos$RBD_IgM_Titer,by=list(RBD_IgM_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(RBD_IgM_Pos$RBD_IgM_Titer,by=list(RBD_IgM_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(RBD_IgM_Titer ~ Group_Pos, data = RBD_IgM_Pos, var.equal = TRUE)
res

#IgG
aggregate(S1_Spike_IgG_Pos$S1_Spike_IgG_Titer,by=list(S1_Spike_IgG_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(S1_Spike_IgG_Pos$S1_Spike_IgG_Titer,by=list(S1_Spike_IgG_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(S1_Spike_IgG_Pos$S1_Spike_IgG_Titer,by=list(S1_Spike_IgG_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(S2_Spike_IgG_Pos$S2_Spike_IgG_Titer,by=list(S2_Spike_IgG_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(S2_Spike_IgG_Pos$S2_Spike_IgG_Titer,by=list(S2_Spike_IgG_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(S2_Spike_IgG_Pos$S2_Spike_IgG_Titer,by=list(S2_Spike_IgG_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(S2_Spike_IgG_Titer ~ Group_Pos, data = S2_Spike_IgG_Pos, var.equal = TRUE)
res

aggregate(Nucleoprotein_IgG_Pos$Nucleoprotein_IgG_Titer,by=list(Nucleoprotein_IgG_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(Nucleoprotein_IgG_Pos$Nucleoprotein_IgG_Titer,by=list(Nucleoprotein_IgG_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(Nucleoprotein_IgG_Pos$Nucleoprotein_IgG_Titer,by=list(Nucleoprotein_IgG_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

#Sig 
res <- t.test(Nucleoprotein_IgG_Titer ~ Group_Pos, data = Nucleoprotein_IgG_Pos, var.equal = TRUE)
res

aggregate(RBD_IgG_Pos$RBD_IgG_Titer,by=list(RBD_IgG_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(RBD_IgG_Pos$RBD_IgG_Titer,by=list(RBD_IgG_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(RBD_IgG_Pos$RBD_IgG_Titer,by=list(RBD_IgG_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(RBD_IgG_Titer ~ Group_Pos, data = RBD_IgG_Pos, var.equal = TRUE)
res

#IgA 
aggregate(S1_Spike_IgA_Pos$S1_Spike_IgA_Titer,by=list(S1_Spike_IgA_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(S1_Spike_IgA_Pos$S1_Spike_IgA_Titer,by=list(S1_Spike_IgA_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(S1_Spike_IgA_Pos$S1_Spike_IgA_Titer,by=list(S1_Spike_IgA_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(S2_Spike_IgA_Pos$S2_Spike_IgA_Titer,by=list(S2_Spike_IgA_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(S2_Spike_IgA_Pos$S2_Spike_IgA_Titer,by=list(S2_Spike_IgA_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(S2_Spike_IgA_Pos$S2_Spike_IgA_Titer,by=list(S2_Spike_IgA_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(Nucleoprotein_IgA_Pos$Nucleoprotein_IgA_Titer,by=list(Nucleoprotein_IgA_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(Nucleoprotein_IgA_Pos$Nucleoprotein_IgA_Titer,by=list(Nucleoprotein_IgA_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(Nucleoprotein_IgA_Pos$Nucleoprotein_IgA_Titer,by=list(Nucleoprotein_IgA_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(RBD_IgA_Pos$RBD_IgA_Titer,by=list(RBD_IgA_Pos$Group),FUN=mean,na.rm=TRUE)
aggregate(RBD_IgA_Pos$RBD_IgA_Titer,by=list(RBD_IgA_Pos$Group),FUN=range,na.rm=TRUE)
aggregate(RBD_IgA_Pos$RBD_IgA_Titer,by=list(RBD_IgA_Pos$Group),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))


#Semi-quant: Symptomatic vs. Asymptomatic 
table(POS$S1_Spike_IgM[POS$Group=="Positive (Symptomatic)"])
table(POS$S1_Spike_IgM[POS$Group=="Positive (Asymptomatic)"])

table(POS$S1_Spike_IgG[POS$Group=="Positive (Symptomatic)"])
table(POS$S1_Spike_IgG[POS$Group=="Positive (Asymptomatic)"])

table(POS$S1_Spike_IgA[POS$Group=="Positive (Symptomatic)"])
table(POS$S1_Spike_IgA[POS$Group=="Positive (Asymptomatic)"])

table(POS$RBD_IgM[POS$Group=="Positive (Symptomatic)"])
table(POS$RBD_IgM[POS$Group=="Positive (Asymptomatic)"])

table(POS$RBD_IgG[POS$Group=="Positive (Symptomatic)"])
table(POS$RBD_IgG[POS$Group=="Positive (Asymptomatic)"])

table(POS$RBD_IgA[POS$Group=="Positive (Symptomatic)"])
table(POS$RBD_IgA[POS$Group=="Positive (Asymptomatic)"])

table(POS$S2_Spike_IgM[POS$Group=="Positive (Symptomatic)"])
table(POS$S2_Spike_IgM[POS$Group=="Positive (Asymptomatic)"])

table(POS$S2_Spike_IgG[POS$Group=="Positive (Symptomatic)"])
table(POS$S2_Spike_IgG[POS$Group=="Positive (Asymptomatic)"])

table(POS$S2_Spike_IgA[POS$Group=="Positive (Symptomatic)"])
table(POS$S2_Spike_IgA[POS$Group=="Positive (Asymptomatic)"])

table(POS$Nucleoprotein_IgM[POS$Group=="Positive (Symptomatic)"])
table(POS$Nucleoprotein_IgM[POS$Group=="Positive (Asymptomatic)"])

table(POS$Nucleoprotein_IgG[POS$Group=="Positive (Symptomatic)"])
table(POS$Nucleoprotein_IgG[POS$Group=="Positive (Asymptomatic)"])

table(POS$Nucleoprotein_IgA[POS$Group=="Positive (Symptomatic)"])
table(POS$Nucleoprotein_IgA[POS$Group=="Positive (Asymptomatic)"])

#Chi-Square Tests 

tbl = table(POS$Group, POS$S1_Spike_IgM) 
tbl
fisher.test(tbl)

#Sig 
tbl = table(POS$Group, POS$S1_Spike_IgG) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$S1_Spike_IgA) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$RBD_IgM) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$RBD_IgG) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$RBD_IgA) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$S2_Spike_IgM) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$S2_Spike_IgG) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$S2_Spike_IgA) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$Nucleoprotein_IgM) 
tbl
fisher.test(tbl)

#Sig
tbl = table(POS$Group, POS$Nucleoprotein_IgG) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$Nucleoprotein_IgA) 
tbl
fisher.test(tbl)





