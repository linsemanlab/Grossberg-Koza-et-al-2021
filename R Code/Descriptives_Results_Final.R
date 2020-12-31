#Descriptive Statistics - Results
#This script contains descriptive statistics presented in the results section of the paper - each section of code is matched to the correct heading from the paper 

#####
#Libraries used 
library(plotrix)

######
#Participant Population - Denver, Colorado cohort 

summary(dataset523)#Class, Descriptive info for each column in dataset
length(dataset523)#Number of columns in data set 
nrow(dataset523)#Number of rows in data set - 107 participants 

table(dataset523$Gender)#Gender breakdown for entire population 

mean(dataset523$Age, na.rm=TRUE)#Mean age for entire population
range(dataset523$Age, na.rm=TRUE)#Age range for entire population 

table(dataset523$Ig_Test_Result) #Number of patients who tested negative or positive 

table(dataset523$Form_Response)#number of patients who responded to the questionnaire (form) in the complete dataset 
table(dataset523$Form_Response[dataset523$Ig_Test_Result=="Positive"])#number of patients who responded to the questionnaire (form) in the positive dataset 
table(dataset523$Form_Response[dataset523$Ig_Test_Result=="Negative"])#number of patients who responded to the questionnaire (form) in the negative dataset 

table(dataset523$Group)#of participants who experienced symptoms out of those who responded to the questionnaire

mean(dataset523$BMI, na.rm=TRUE)#Mean BMI for entire population 

#For Chronic illnesses, age, sex, and BMI stats - See "Table_2_Final.R" script 

#####
#Antibody titers against SARS-CoV-2 antigens 

#For titer comparisons between COVID-POS/NEG patients for each antibody antigen combo - See "Supplementary_Figure_1_Final.R" script 
#For titer comparisons between female/male COVID-POS/NEG patients - See "Supplementary_Figure_2_Final.R" script 

#####
#Loss of smell was uniquely more severe in COVID-19-positive participants 

#For symptom comparisons between COVID-POS/NEG patients and males/females - See "Supplementary_Table_1_Final.R" script

#Pearson's correlations between symptom severity, age and BMI in the entire patient population 
library(psych)
library(psychTools)
library(stats)

cor_titer_sym_ALL <- data.frame(dataset523$Age, dataset523$BMI, dataset523$S_Onset_To_T1_Days, dataset523$S_Onset_To_R_Days, dataset523$BMI, dataset523$S1_Spike_IgG_Titer, dataset523$S1_Spike_IgM_Titer, dataset523$S1_Spike_IgA_Titer,dataset523$S2_Spike_IgG_Titer,dataset523$S2_Spike_IgM_Titer,dataset523$S2_Spike_IgA_Titer,dataset523$RBD_IgG_Titer,dataset523$RBD_IgM_Titer,dataset523$RBD_IgA_Titer,dataset523$Nucleoprotein_IgG_Titer,dataset523$Nucleoprotein_IgM_Titer,dataset523$Nucleoprotein_IgA_Titer,dataset523$MF_Fever_Sev, dataset523$MF_Dry_cough_Sev, dataset523$MF_Sore_throat_Sev, dataset523$MF_Fatigue_Sev,dataset523$MF_SP_Sev,dataset523$MF_NC_Sev,dataset523$MF_Runny_Nose_Sev,dataset523$MF_Headache_Sev, dataset523$MF_LoS_Sev, dataset523$MF_LoT_Sev, dataset523$MF_Vomiting_Sev, dataset523$MF_Diarrhea_Sev, dataset523$MF_Dizziness_Sev, dataset523$MF_Chills_Sev, dataset523$MF_Body_Aches_Sev, dataset523$MF_SoB_Sev, dataset523$MF_Chest_pain_Sev, POS$Symptom_Severity_Total)
colnames(cor_titer_sym_ALL) <- c("Age", "BMI", "SO-T1", "SO-R", "BMI", "S1_IgG","S1_IgM","S1_IgA","S2_IgG","S2_IgM","S2_IgA","RBD_IgG", "RBD_IgM", "RBD_IgA", "N_IgG", "N_IgM", "N_IgA", "Fever","Cough", "ST", "Fatigue","SP", "NC", "RN", "Headache","LoS", "LoT", "Vomiting", "Diarrhea", "Dizziness", "Chills", "BA", "SoB", "CP", "Sev_Total")
str(cor_titer_sym_ALL)
cts_ALL <- corr.test(cor_titer_sym_ALL)
print(corr.test(cor_titer_sym_ALL))

write.csv(cts_ALL$r,file="coef_titer_sym.csv")#Export CSV with Pearson's r - correlation coefficient 
write.csv(cts_ALL$p,file="pvalue_titer_sym.csv")#Export CSV with p-values 
write.csv(cts_ALL$n,file="sample_size_titer_sym.csv")#Export CSV with n-values 

#Pearson's correlations between symptom severity, age and BMI with COVID-19 positive participants only  

cor_titer_sym_POS <- data.frame(POS$Age, POS$BMI, POS$S_Onset_To_T1_Days, POS$S_Onset_To_R_Days, POS$S1_Spike_IgG_Titer, POS$S1_Spike_IgM_Titer, POS$S1_Spike_IgA_Titer,POS$S2_Spike_IgG_Titer,POS$S2_Spike_IgM_Titer,POS$S2_Spike_IgA_Titer,POS$RBD_IgG_Titer,POS$RBD_IgM_Titer,POS$RBD_IgA_Titer,POS$Nucleoprotein_IgG_Titer,POS$Nucleoprotein_IgM_Titer,POS$Nucleoprotein_IgA_Titer,POS$MF_Fever_Sev, POS$MF_Dry_cough_Sev, POS$MF_Sore_throat_Sev, POS$MF_Fatigue_Sev,POS$MF_SP_Sev,POS$MF_NC_Sev,POS$MF_Runny_Nose_Sev,POS$MF_Headache_Sev, POS$MF_LoS_Sev, POS$MF_LoT_Sev, POS$MF_Vomiting_Sev, POS$MF_Diarrhea_Sev, POS$MF_Dizziness_Sev, POS$MF_Chills_Sev, POS$MF_Body_Aches_Sev, POS$MF_SoB_Sev, POS$MF_Chest_pain_Sev, POS$Symptom_Severity_Total)
colnames(cor_titer_sym_POS) <- c("Age", "BMI", "SO-T1", "SO-R","S1_IgG","S1_IgM","S1_IgA","S2_IgG","S2_IgM","S2_IgA","RBD_IgG", "RBD_IgM", "RBD_IgA", "N_IgG", "N_IgM", "N_IgA", "Fever","Cough", "ST", "Fatigue","SP", "NC", "RN", "Headache","LoS", "LoT", "Vomiting", "Diarrhea", "Dizziness", "Chills", "BA", "SoB", "CP", "Sev_Total")
str(cor_titer_sym_POS)
cts_POS <- corr.test(cor_titer_sym_POS)
print(corr.test(cor_titer_sym_POS))

write.csv(cts_POS$r,file="coef_titer_sym_POS.csv")#Export CSV with Pearson's r - correlation coefficient 
write.csv(cts_POS$p,file="pvalue_titer_sym_POS.csv")#Export CSV with p-values 
write.csv(cts_POS$n,file="sample_size_titer_sym_POS.csv")#Export CSV with n-values

######
#Antibody titers against SARS-CoV-2 antigens in participants with mild versus severe symptoms 

median(POS_symptomatic$Symptom_Severity_Total, na.rm=TRUE)#Median symptom severity 

table(POS$Group_All)#The number of COVID-19-positive participants who had mild-moderate symptoms, moderate-severe symptoms, or who were asymptomatic

mean(POS_symptomatic$Symptom_Severity_Total[POS_symptomatic$Symptom_Sev_Cat_1=="Moderate-Severe"])#Mean symptom severity for moderate-severe group
mean(POS_symptomatic$Symptom_Severity_Total[POS_symptomatic$Symptom_Sev_Cat_1=="Mild-Moderate"])#Mean symptom severity for mild-moderate group

aggregate(POS$RBD_IgA_Titer,by=list(POS$Group_All),FUN=mean,na.rm=TRUE)
aggregate(POS$RBD_IgA_Titer,by=list(POS$Group_All),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(POS$Nucleoprotein_IgG_Titer,by=list(POS$Group_All),FUN=mean,na.rm=TRUE)
aggregate(POS$Nucleoprotein_IgG_Titer,by=list(POS$Group_All),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(POS$S2_Spike_IgA_Titer,by=list(POS$Group_All),FUN=mean,na.rm=TRUE)
aggregate(POS$S2_Spike_IgA_Titer,by=list(POS$Group_All),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

#For graphical representation of difference between RBD IgA, Nucleoprotein IgG and S2 Spike IgA - See "Figure_1_Final.R" script 
#For graphical representation of difference between RBD IgA, Nucleoprotein IgG and S2 Spike IgA for males and females in each symptom severity group - See "Supplementary_Figure_3_Final.R" script 

#No significant differences in days between symptom onset to serological test date, age, sex and BMI between COVID-19-positive participants with mild-moderate versus moderate-severe symptomology

res <- t.test(S_Onset_To_T1_Days ~ Symptom_Sev_Cat_1, data = POS, var.equal = TRUE)
res

res <- t.test(Age ~ Symptom_Sev_Cat_1, data = POS, var.equal = TRUE)
res

tbl = table(POS$Gender, POS$Symptom_Sev_Cat_1) 
tbl
fisher.test(tbl)

res <- t.test(BMI ~ Symptom_Sev_Cat_1, data = POS, var.equal = TRUE)
res

#####
#IgM and IgG timelines 

#Pearson's correlations between titers and days between symptom onset and resolution or days between symptom onset and the initial test in the entire population 

cor_titer_ALL <- data.frame(dataset523$S_Onset_To_T1_Days, dataset523$S_Onset_To_R_Days, dataset523$BMI, dataset523$S1_Spike_IgG_Titer, dataset523$S1_Spike_IgM_Titer, dataset523$S1_Spike_IgA_Titer,dataset523$S2_Spike_IgG_Titer,dataset523$S2_Spike_IgM_Titer,dataset523$S2_Spike_IgA_Titer,dataset523$RBD_IgG_Titer,dataset523$RBD_IgM_Titer,dataset523$RBD_IgA_Titer,dataset523$Nucleoprotein_IgG_Titer,dataset523$Nucleoprotein_IgM_Titer,dataset523$Nucleoprotein_IgA_Titer)
colnames(cor_titer_ALL) <- c("SO-T1", "SO-R", "BMI", "S1_IgG","S1_IgM","S1_IgA","S2_IgG","S2_IgM","S2_IgA","RBD_IgG", "RBD_IgM", "RBD_IgA", "N_IgG", "N_IgM", "N_IgA")
str(cor_titer_ALL)
ct_ALL <- corr.test(cor_titer_ALL)
print(corr.test(cor_titer_ALL))

write.csv(ct_ALL$r,file="rtmp.csv")#Export CSV with Pearson's r - correlation coefficient
write.csv(ct_ALL$p,file="ptmp.csv")#Export CSV with p-values 
write.csv(ct_ALL$n,file="ntmp.csv")#Export CSV with n-values

##Pearson's correlations in COVID-19-positive participants between titers and days between symptom onset and resolution or days between symptom onset and the initial test with participants who were COVID-19-positive 

cor_titer_ALL <- data.frame(POS$S_Onset_To_T1_Days, POS$S_Onset_To_R_Days, POS$BMI, POS$S1_Spike_IgG_Titer, POS$S1_Spike_IgM_Titer, POS$S1_Spike_IgA_Titer,POS$S2_Spike_IgG_Titer,POS$S2_Spike_IgM_Titer,POS$S2_Spike_IgA_Titer,POS$RBD_IgG_Titer,POS$RBD_IgM_Titer,POS$RBD_IgA_Titer,POS$Nucleoprotein_IgG_Titer,POS$Nucleoprotein_IgM_Titer,POS$Nucleoprotein_IgA_Titer)
colnames(cor_titer_ALL) <- c("SO-T1", "SO-R", "BMI", "S1_IgG","S1_IgM","S1_IgA","S2_IgG","S2_IgM","S2_IgA","RBD_IgG", "RBD_IgM", "RBD_IgA", "N_IgG", "N_IgM", "N_IgA")
str(cor_titer_ALL)
ct_ALL <- corr.test(cor_titer_ALL)
print(corr.test(cor_titer_ALL))

write.csv(ct_ALL$r,file="rtmp.csv")#Export CSV with Pearson's r - correlation coefficient
write.csv(ct_ALL$p,file="ptmp.csv")#Export CSV with p-values 
write.csv(ct_ALL$n,file="ntmp.csv")#Export CSV with n-values

#For graphical representation of titers and symptom onset categories see - "Figure_2_Final.R" script 

aggregate(POS$IgGMean,by=list(POS$S_Onset_To_T1_Days_Cat),FUN=mean,na.rm=TRUE)
aggregate(POS$IgGMean,by=list(POS$S_Onset_To_T1_Days_Cat),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(POS$IgMMean,by=list(POS$S_Onset_To_T1_Days_Cat),FUN=mean,na.rm=TRUE)
aggregate(POS$IgMMean,by=list(POS$S_Onset_To_T1_Days_Cat),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(POS$S1_Spike_IgM_Titer,by=list(POS$S_Onset_To_T1_Days_Cat),FUN=mean,na.rm=TRUE)
aggregate(POS$S1_Spike_IgM_Titer,by=list(POS$S_Onset_To_T1_Days_Cat),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(POS$RBD_IgM_Titer,by=list(POS$S_Onset_To_T1_Days_Cat),FUN=mean,na.rm=TRUE)
aggregate(POS$RBD_IgM_Titer,by=list(POS$S_Onset_To_T1_Days_Cat),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

#Inferential Statistics - One-Way ANOVA 
#Compares antibody titers at different intervals (0-30,30-60,60-90,90-120) in COVID-19-positive participants 

#IgG 
res.aov <- aov(IgGMean ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

res.aov <- aov(S1_Spike_IgG_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

res.aov <- aov(S2_Spike_IgG_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

res.aov <- aov(RBD_IgG_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

res.aov <- aov(Nucleoprotein_IgG_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)


#IgM

#Sig 
res.aov <- aov(IgMMean ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)
TukeyHSD(res.aov)#Tukey Post-Hoc test 

#Sig 
res.aov <- aov(S1_Spike_IgM_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)
TukeyHSD(res.aov)#Tukey Post-Hoc test 

res.aov <- aov(S2_Spike_IgM_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

#Sig 
res.aov <- aov(RBD_IgM_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)
TukeyHSD(res.aov)#Tukey Post-Hoc test 

res.aov <- aov(Nucleoprotein_IgM_Titer ~ S_Onset_To_T1_Days_Cat, data =POS)
summary(res.aov)

#IgA 
res.aov <- aov(IgAMean ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

res.aov <- aov(S1_Spike_IgA_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

res.aov <- aov(S2_Spike_IgA_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

res.aov <- aov(RBD_IgA_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

res.aov <- aov(Nucleoprotein_IgA_Titer ~ S_Onset_To_T1_Days_Cat, data = POS)
summary(res.aov)

#####
#Antibody titers in COVID-19-positive symptomatic versus asymptomatic participants

#For differences in sex, age and BMI see - "Table_3_Final.R" script 

tbl = table(POS$Group, POS$Smoking_History) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$Cardiovascular_Disease) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$Diabetes) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$Autoimmune_Disease) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$Tick_Born_Illness) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$Hemochromatosis) 
tbl
fisher.test(tbl)

tbl = table(POS$Group, POS$Blood_Related_Diseases) 
tbl
fisher.test(tbl)

#Only titers for IgG against RBD were significantly increased in symptomatic females (n=6) versus symptomatic males (n=4, p=0.03). 

RBD_IgG_Pos_Symptomatic <- RBD_IgG_Pos %>% filter((Group_Pos=="Positive (Symptomatic)")) 

res <- t.test(RBD_IgG_Titer ~ Gender, data = RBD_IgG_Pos_Symptomatic, var.equal = TRUE)
res

table(RBD_IgG_Pos_Symptomatic$Gender[RBD_IgG_Pos_Symptomatic$Group_Pos=="Positive (Symptomatic)"])

#T-tests - Differences between antibody means and Group_Pos 

res <- t.test(IgGMean ~ Group_Pos, data = dataset523, var.equal = TRUE)
res

#Chi-Square Goodness of Fit Test 

tbl = table(IgM_Only$Group_Pos, IgM_Only$IgM_Result) 
tbl
chisq.test(tbl,correct=FALSE) 

#Sig 
tbl = table(IgG_Only$Group_Pos, IgG_Only$IgG_Result) 
tbl
chisq.test(tbl,correct=FALSE) 

tbl = table(IgA_Only$Group_Pos, IgA_Only$IgA_Result) 
tbl

#Differences between POS Symptomatic and POS asymptomatic participants positive for IgM or IgG 

tbl = table(POS_asymptomatic$IgG_Result, POS_asymptomatic$IgM_Result) 
tbl
fisher.test(tbl)

######
#Antibody/antigen combinations in relation to COVID-19 diagnosis and symptom severity

#For Pearson's correlations and graphical representation of correlations see "Supplementary_Figure_4_Final.R" script 
#For heatmap data see "Figure_3_Final.R" script 


