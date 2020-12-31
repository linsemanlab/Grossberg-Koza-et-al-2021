#Supplementary Table 1 - Negative vs. Positive Symptom Data 
#Reported symptoms and range of symptom severity in symptomatic COVID-19 positive and symptomatic COVID-19 negative participants.

######
#Libraries used 
library(plotrix)

######
#Number of participants reporting symptoms  

table(dataset523$MF_COVID_Symptoms[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_COVID_Symptoms[dataset523$Ig_Test_Result=="Negative"])

#Days Between Symptom Onset and Symptom Resolution
aggregate(dataset523$S_Onset_To_R_Days,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$S_Onset_To_R_Days,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$S_Onset_To_R_Days,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(S_Onset_To_R_Days ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

#Symptom Count

table(dataset523$MF_Fever_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Fever_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Dry_Cough_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Dry_Cough_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Sore_Throat_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Sore_Throat_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Fatigue_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Fatigue_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_SP_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_SP_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_NC_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_NC_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Runny_Nose_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Runny_Nose_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Headache_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Headache_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_LoS_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_LoS_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_LoT_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_LoT_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Vomiting_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Vomiting_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Diarrhea_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Diarrhea_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Dizziness_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Dizziness_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Chills_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Chills_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Body_Aches_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Body_Aches_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_SoB_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_SoB_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Chest_Pain_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Chest_Pain_Cat[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$Swollen_lymph_nodes_Cat[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$Swollen_lymph_nodes_Cat[dataset523$Ig_Test_Result=="Negative"])

#Symptom Severity 

aggregate(dataset523$MF_Fever_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Fever_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Fever_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Dry_cough_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Dry_cough_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Dry_cough_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Sore_throat_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Sore_throat_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Sore_throat_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Fatigue_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Fatigue_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Fatigue_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_SP_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_SP_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_SP_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_NC_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_NC_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_NC_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Runny_Nose_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Runny_Nose_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Runny_Nose_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Headache_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Headache_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Headache_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_LoS_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_LoS_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_LoS_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_LoT_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_LoT_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_LoT_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Vomiting_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Vomiting_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Vomiting_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Diarrhea_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Diarrhea_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Diarrhea_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Dizziness_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Dizziness_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Dizziness_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Chills_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Chills_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Chills_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Body_Aches_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Body_Aches_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Body_Aches_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_SoB_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_SoB_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_SoB_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_Chest_pain_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_Chest_pain_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_Chest_pain_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

aggregate(dataset523$MF_SLN_Sev,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$MF_SLN_Sev,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$MF_SLN_Sev,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

#Inferential Statistics 

#T-Tests and Chi-Square to look for differences between symptom severity in POS/NEG groups 

res <- t.test(MF_Fever_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Fever_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Fever_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 


res <- t.test(MF_Dry_cough_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Dry_cough_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Dry_Cough_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_Sore_throat_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Sore_throat_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Sore_Throat_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_Fatigue_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Fatigue_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Fatigue_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_SP_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_SP_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_SP_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 


res <- t.test(MF_NC_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_NC_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_NC_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 


res <- t.test(MF_Runny_Nose_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Runny_Nose_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Runny_Nose_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 


res <- t.test(MF_Headache_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Headache_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Headache_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 


res <- t.test(MF_LoT_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_LoT_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_LoT_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

#sig 
res <- t.test(MF_LoS_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_LoS_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_LoS_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_Vomiting_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Vomiting_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Vomiting_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_Diarrhea_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Diarrhea_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Diarrhea_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_Dizziness_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Dizziness_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Dizziness_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_Chills_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Chills_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Chills_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_Body_Aches_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Body_Aches_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Body_Aches_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_SoB_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_SoB_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_SoB_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_SLN_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_SLN_Sev ~ Gender, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_SLN_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 

res <- t.test(MF_Chest_pain_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

res <- t.test(MF_Chest_pain_Sev ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Chest_Pain_Cat) 
tbl
chisq.test(tbl,correct=FALSE) 




