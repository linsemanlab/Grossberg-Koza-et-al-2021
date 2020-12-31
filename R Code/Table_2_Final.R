### Table 2 - Final
#Demographic and clinical characteristics of the Colorado cohort including COVID-19-positive and COVID-19-negative participants.

######
#Libraries used 
library(plotrix)
library(plyr)

#######
##### Demographic Information

#Sample Size 
table(dataset523$Ig_Test_Result) #Number of patients who tested negative or positive 
nrow(dataset523)#Number of rows in data set - 107 total participants

#Age 
mean(dataset523$Age,na.rm=TRUE)
range(dataset523$Age,na.rm=TRUE)
std.error(dataset523$Age,na.rm=TRUE)

aggregate(dataset523$Age,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$Age,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$Age,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(Age ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

#Gender 
table(dataset523$Gender)
table(dataset523$Gender[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$Gender[dataset523$Ig_Test_Result=="Negative"])

tbl = table(dataset523$Ig_Test_Result, dataset523$Gender) 
tbl
chisq.test(tbl,correct=FALSE) 

#Questionnaire Sample Size 
table(dataset523$Form_Response) 
table(POS$Form_Response) 
table(NEG$Form_Response) 

##Days Between Symptom Onset and Initial Test 
aggregate(dataset523$S_Onset_To_T1_Days,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$S_Onset_To_T1_Days,by=list(dataset523$Ig_Test_Result),FUN=median,na.rm=TRUE)
aggregate(dataset523$S_Onset_To_T1_Days,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$S_Onset_To_T1_Days,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(S_Onset_To_T1_Days ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

mean(dataset523$S_Onset_To_T1_Days, na.rm=TRUE)
std.error(dataset523$S_Onset_To_T1_Days, na.rm=TRUE)
range(dataset523$S_Onset_To_T1_Days, na.rm=TRUE) 

#Ethnicity
table(dataset523$MF_Ethnicity)
table(dataset523$MF_Ethnicity[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Ethnicity[dataset523$Ig_Test_Result=="Negative"])

tbl = table(dataset523$Ig_Test_Result, dataset523$MF_Ethnicity) 
tbl
fisher.test(tbl)

#BMI 
mean(dataset523$BMI,na.rm=TRUE)
std.error(dataset523$BMI,na.rm=TRUE)
median(dataset523$BMI,na.rm=TRUE)
range(dataset523$BMI,na.rm=TRUE)

aggregate(dataset523$BMI,by=list(dataset523$Ig_Test_Result),FUN=mean,na.rm=TRUE)
aggregate(dataset523$BMI,by=list(dataset523$Ig_Test_Result),FUN=range,na.rm=TRUE)
aggregate(dataset523$BMI,by=list(dataset523$Ig_Test_Result),FUN=function(x)c(mean = mean(x),se=std.error(x),na.rm=TRUE))

res <- t.test(BMI ~ Ig_Test_Result, data = dataset523, var.equal = TRUE)
res

#Smoking History 
table(dataset523$MF_Tobacco_Freq)

table(dataset523$MF_Tobacco_Freq[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Tobacco_Freq[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$MF_Marijuana_Freq)

table(dataset523$MF_Marijuana_Freq[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$MF_Marijuana_Freq[dataset523$Ig_Test_Result=="Negative"])

table(dataset523$Smoking_History)
table(dataset523$Smoking_History[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$Smoking_History[dataset523$Ig_Test_Result=="Negative"])

tbl = table(dataset523$Ig_Test_Result, dataset523$Smoking_History) 
tbl
fisher.test(tbl)

#Medical History 
table(dataset523$Hypertension)#0 participants had this condition - no inferential test was conducted 

table(dataset523$Cardiovascular_Disease)
table(dataset523$Cardiovascular_Disease[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$Cardiovascular_Disease[dataset523$Ig_Test_Result=="Negative"])

tbl = table(dataset523$Ig_Test_Result, dataset523$Cardiovascular_Disease) 
tbl
fisher.test(tbl)

table(dataset523$Diabetes)
table(dataset523$Diabetes[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$Diabetes[dataset523$Ig_Test_Result=="Negative"])

tbl = table(dataset523$Ig_Test_Result, dataset523$Diabetes) 
tbl
fisher.test(tbl)

table(dataset523$Liver_Disease)#0 participants had this condition - no inferential test was conducted 

table(dataset523$Autoimmune_Disease)
table(dataset523$Autoimmune_Disease[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$Autoimmune_Disease[dataset523$Ig_Test_Result=="Negative"])

tbl = table(dataset523$Ig_Test_Result, dataset523$Autoimmune_Disease) 
tbl
fisher.test(tbl)

table(dataset523$Tick_Born_Illness)
table(dataset523$Tick_Born_Illness[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$Tick_Born_Illness[dataset523$Ig_Test_Result=="Negative"])

tbl = table(dataset523$Ig_Test_Result, dataset523$Tick_Born_Illness) 
tbl
fisher.test(tbl)

table(dataset523$Hemochromatosis)
table(dataset523$Hemochromatosis[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$Hemochromatosis[dataset523$Ig_Test_Result=="Negative"])

tbl = table(dataset523$Ig_Test_Result, dataset523$Hemochromatosis) 
tbl
fisher.test(tbl)

table(dataset523$Blood_Related_Diseases)
table(dataset523$Blood_Related_Diseases[dataset523$Ig_Test_Result=="Positive"])
table(dataset523$Blood_Related_Diseases[dataset523$Ig_Test_Result=="Negative"])

tbl = table(dataset523$Ig_Test_Result, dataset523$Blood_Related_Diseases) 
tbl
fisher.test(tbl)


