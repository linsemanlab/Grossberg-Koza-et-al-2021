#Data Cleaning 
#This script contains code used to create new columns and data frames used in analyses and figures 

######
#Libraries used 
library(plyr)
library(tidyverse) 
library(dplyr)

######
#Creating antibody Positive Columns - Participants who are positive for either IgM, IgG, or IgA

#IgM
#Adds column at the end of dataset with numerical count of the # of IgM antigens each participants was positive for 
dataset523$IgM_count <- dataset523$RBD_IgM + dataset523$Nucleoprotein_IgM + dataset523$S1_Spike_IgM + dataset523$S2_Spike_IgM

which(colnames(dataset523)=="IgM_count")#Getting column number of input column 

#Running a loop to recategorize IgM count as a categorical - IgM Negative (when IgM count=0) or IgM Positive (when IgM count>=1)
#New column will be added to end of dataset 
i = 1
n <- length(dataset523$IgM_count)
for(i in 1:n){
  if (is.na(dataset523[i,194])){dataset523[i,195]<- "NA"}
  else if (dataset523[i,194]=="0"){dataset523[i,195]<-"IgM Negative"}
  else if(dataset523[i,194]>="1"){dataset523[i,195]<-"IgM Positive"}
} 

colnames(dataset523)[195] <- "IgM_Result" #Renaming newly created column

View(data.frame(dataset523$RBD_IgM,dataset523$Nucleoprotein_IgM,dataset523$S1_Spike_IgM,dataset523$S2_Spike_IgM,dataset523$IgM_Result,dataset523$IgM_count)) 


#IgG
#Adds column at the end of dataset with numerical count of the # of IgG antigens each participants was positive for 
dataset523$IgG_count <- dataset523$RBD_IgG + dataset523$Nucleoprotein_IgG + dataset523$S1_Spike_IgG + dataset523$S2_Spike_IgG

which(colnames(dataset523)=="IgG_count") #Getting column number of input column 

#Running a loop to recategorize IgG count as a categorical - IgG Negative (when IgG count=0) or IgG Positive (when IgG count>=1)
#New column will be added to end of dataset
i = 1
n <- length(dataset523$IgG_count)
for(i in 1:n){
  if (is.na(dataset523[i,196])){dataset523[i,197]<- "NA"}
  else if (dataset523[i,196]=="0"){dataset523[i,197]<-"IgG Negative"}
  else if(dataset523[i,196]>="1"){dataset523[i,197]<-"IgG Positive"}
} 

colnames(dataset523)[197] <- "IgG_Result" #Renaming newly created column

View(data.frame(dataset523$RBD_IgG,dataset523$Nucleoprotein_IgG,dataset523$S1_Spike_IgG,dataset523$S2_Spike_IgG,dataset523$IgG_Result,dataset523$IgG_count)) 

#IgA
#Adds column at the end of dataset with numerical count of the # of IgA antigens each participants was positive for
dataset523$IgA_count <- dataset523$RBD_IgA + dataset523$Nucleoprotein_IgA + dataset523$S1_Spike_IgA + dataset523$S2_Spike_IgA

which(colnames(dataset523)=="IgA_count") #Getting column number of input column 

#Running a loop to recategorize IgA count as a categorical - IgA Negative (when IgA count=0) or IgA Positive (when IgA count>=1)
#New column will be added to end of dataset
i = 1
n <- length(dataset523$IgA_count)
for(i in 1:n){
  if (is.na(dataset523[i,198])){dataset523[i,199]<- "NA"}
  else if (dataset523[i,198]=="0"){dataset523[i,199]<-"IgA Negative"}
  else if(dataset523[i,198]>="1"){dataset523[i,199]<-"IgA Positive"}
} 

colnames(dataset523)[199] <- "IgA_Result" #Renaming newly created column

View(data.frame(dataset523$RBD_IgA,dataset523$Nucleoprotein_IgA,dataset523$S1_Spike_IgA,dataset523$S2_Spike_IgA,dataset523$IgA_Result,dataset523$IgA_count)) 

#####
#Creating categorical variables for symptoms using the symptom severity scores (scale from 1-5) from the questionnaire 
#If participant rated symptom severity as 1-5 then the new category = yes 
#If participant did not rate symptom severity then the new category = no

#Fever - new column 200 
dataset523$MF_Fever_Cat <- ifelse(dataset523$MF_Fever_Sev == "1", "Yes", ifelse(dataset523$MF_Fever_Sev== "2", "Yes", ifelse(dataset523$MF_Fever_Sev== "3", "Yes", ifelse(dataset523$MF_Fever_Sev == "4", "Yes", ifelse(dataset523$MF_Fever_Sev == "5", "Yes","No")))))

#Dry cough - new column 201 
dataset523$MF_Dry_Cough_Cat <- ifelse(dataset523$MF_Dry_cough_Sev == "1", "Yes", ifelse(dataset523$MF_Dry_cough_Sev== "2", "Yes", ifelse(dataset523$MF_Dry_cough_Sev== "3", "Yes", ifelse(dataset523$MF_Dry_cough_Sev == "4", "Yes", ifelse(dataset523$MF_Dry_cough_Sev == "5", "Yes","No")))))

#Sore Throat - new column 202 
dataset523$MF_Sore_Throat_Cat <- ifelse(dataset523$MF_Sore_throat_Sev == "1", "Yes", ifelse(dataset523$MF_Sore_throat_Sev== "2", "Yes", ifelse(dataset523$MF_Sore_throat_Sev== "3", "Yes", ifelse(dataset523$MF_Sore_throat_Sev == "4", "Yes", ifelse(dataset523$MF_Sore_throat_Sev == "5", "Yes","No")))))

#Fatigue- new column 203 
dataset523$MF_Fatigue_Cat <- ifelse(dataset523$MF_Fatigue_Sev == "1", "Yes", ifelse(dataset523$MF_Fatigue_Sev== "2", "Yes", ifelse(dataset523$MF_Fatigue_Sev== "3", "Yes", ifelse(dataset523$MF_Fatigue_Sev == "4", "Yes", ifelse(dataset523$MF_Fatigue_Sev == "5", "Yes","No")))))

#Sputum production - - new column 204
dataset523$MF_SP_Cat <- ifelse(dataset523$MF_SP_Sev == "1", "Yes", ifelse(dataset523$MF_SP_Sev== "2", "Yes", ifelse(dataset523$MF_SP_Sev== "3", "Yes", ifelse(dataset523$MF_SP_Sev == "4", "Yes", ifelse(dataset523$MF_SP_Sev == "5", "Yes","No")))))

#Nasal congestion - new column 205
dataset523$MF_NC_Cat <- ifelse(dataset523$MF_NC_Sev == "1", "Yes", ifelse(dataset523$MF_NC_Sev== "2", "Yes", ifelse(dataset523$MF_NC_Sev== "3", "Yes", ifelse(dataset523$MF_NC_Sev == "4", "Yes", ifelse(dataset523$MF_NC_Sev == "5", "Yes","No")))))

#Runny nose - new column 206 
dataset523$MF_Runny_Nose_Cat <- ifelse(dataset523$MF_Runny_Nose_Sev == "1", "Yes", ifelse(dataset523$MF_Runny_Nose_Sev== "2", "Yes", ifelse(dataset523$MF_Runny_Nose_Sev== "3", "Yes", ifelse(dataset523$MF_Runny_Nose_Sev == "4", "Yes", ifelse(dataset523$MF_Runny_Nose_Sev == "5", "Yes","No")))))

#Headache - - new column 207
dataset523$MF_Headache_Cat <- ifelse(dataset523$MF_Headache_Sev == "1", "Yes", ifelse(dataset523$MF_Headache_Sev== "2", "Yes", ifelse(dataset523$MF_Headache_Sev== "3", "Yes", ifelse(dataset523$MF_Headache_Sev == "4", "Yes", ifelse(dataset523$MF_Headache_Sev == "5", "Yes","No")))))

#Loss of Smell - - new column 208
dataset523$MF_LoS_Cat <- ifelse(dataset523$MF_LoS_Sev == "1", "Yes", ifelse(dataset523$MF_LoS_Sev== "2", "Yes", ifelse(dataset523$MF_LoS_Sev== "3", "Yes", ifelse(dataset523$MF_LoS_Sev == "4", "Yes", ifelse(dataset523$MF_LoS_Sev == "5", "Yes","No")))))

#Loss of Taste - - new column 209
dataset523$MF_LoT_Cat <- ifelse(dataset523$MF_LoT_Sev == "1", "Yes", ifelse(dataset523$MF_LoT_Sev== "2", "Yes", ifelse(dataset523$MF_LoT_Sev== "3", "Yes", ifelse(dataset523$MF_LoT_Sev == "4", "Yes", ifelse(dataset523$MF_LoT_Sev == "5", "Yes","No")))))

#Vomiting - - new column 210
dataset523$MF_Vomiting_Cat <- ifelse(dataset523$MF_Vomiting_Sev == "1", "Yes", ifelse(dataset523$MF_Vomiting_Sev== "2", "Yes", ifelse(dataset523$MF_Vomiting_Sev== "3", "Yes", ifelse(dataset523$MF_Vomiting_Sev== "4", "Yes", ifelse(dataset523$MF_Vomiting_Sev== "5", "Yes","No")))))

#Diarrhea - new column 211
dataset523$MF_Diarrhea_Cat <- ifelse(dataset523$MF_Diarrhea_Sev == "1", "Yes", ifelse(dataset523$MF_Diarrhea_Sev== "2", "Yes", ifelse(dataset523$MF_Diarrhea_Sev== "3", "Yes", ifelse(dataset523$MF_Diarrhea_Sev== "4", "Yes", ifelse(dataset523$MF_Diarrhea_Sev== "5", "Yes","No")))))

#Dizziness - new column 212
dataset523$MF_Dizziness_Cat <- ifelse(dataset523$MF_Dizziness_Sev == "1", "Yes", ifelse(dataset523$MF_Dizziness_Sev== "2", "Yes", ifelse(dataset523$MF_Dizziness_Sev== "3", "Yes", ifelse(dataset523$MF_Dizziness_Sev== "4", "Yes", ifelse(dataset523$MF_Dizziness_Sev== "5", "Yes","No")))))

#Chills - new column 213
dataset523$MF_Chills_Cat <- ifelse(dataset523$MF_Chills_Sev == "1", "Yes", ifelse(dataset523$MF_Chills_Sev== "2", "Yes", ifelse(dataset523$MF_Chills_Sev== "3", "Yes", ifelse(dataset523$MF_Chills_Sev== "4", "Yes", ifelse(dataset523$MF_Chills_Sev== "5", "Yes","No")))))

#Body Aches - new column 214
dataset523$MF_Body_Aches_Cat <- ifelse(dataset523$MF_Body_Aches_Sev == "1", "Yes", ifelse(dataset523$MF_Body_Aches_Sev== "2", "Yes", ifelse(dataset523$MF_Body_Aches_Sev== "3", "Yes", ifelse(dataset523$MF_Body_Aches_Sev== "4", "Yes", ifelse(dataset523$MF_Body_Aches_Sev== "5", "Yes","No")))))

#Shortness of Breath - new column 215
dataset523$MF_SoB_Cat <- ifelse(dataset523$MF_SoB_Sev == "1", "Yes", ifelse(dataset523$MF_SoB_Sev== "2", "Yes", ifelse(dataset523$MF_SoB_Sev== "3", "Yes", ifelse(dataset523$MF_SoB_Sev== "4", "Yes", ifelse(dataset523$MF_SoB_Sev== "5", "Yes","No")))))

#Chest pain - new column 216
dataset523$MF_Chest_Pain_Cat <- ifelse(dataset523$MF_Chest_pain_Sev == "1", "Yes", ifelse(dataset523$MF_Chest_pain_Sev== "2", "Yes", ifelse(dataset523$MF_Chest_pain_Sev== "3", "Yes", ifelse(dataset523$MF_Chest_pain_Sev== "4", "Yes", ifelse(dataset523$MF_Chest_pain_Sev== "5", "Yes","No")))))

#Swollen lymph nodes - new column 217
dataset523$Swollen_lymph_nodes_Cat <- ifelse(dataset523$MF_SLN_Sev == "1", "Yes", ifelse(dataset523$MF_Chest_pain_Sev== "2", "Yes", ifelse(dataset523$MF_Chest_pain_Sev== "3", "Yes", ifelse(dataset523$MF_Chest_pain_Sev== "4", "Yes", ifelse(dataset523$MF_Chest_pain_Sev== "5", "Yes","No")))))

######
#Creating Positive and Negative Grouping Variables (Group_Pos=all participants who tested positive and are either asymptomatic or symptomatic), (Group_Neg=all participants who tested negative and are either asymptomatic or symptomatic)

dataset523$Group_Pos <- ifelse(dataset523$Group == "Positive (Symptomatic)", "Positive (Symptomatic)", ifelse(dataset523$Group== "Positive (Asymptomatic)", "Positive (Asymptomatic)",NA))
dataset523$Group_Neg <- ifelse(dataset523$Group == "Negative (Symptomatic)", "Negative (Symptomatic)", ifelse(dataset523$Group== "Negative (Asymptomatic)", "Negative (Asymptomatic)",NA))

View(data.frame(dataset523$Group,dataset523$Group_Pos))
View(data.frame(dataset523$Group,dataset523$Group_Neg))

######
#Creating a categorical variable for the time from symptom onset to the initial serological test date 
#Intervals include 0-30 days, 30-60 days, 60-90 days, and 90-120 days 

which(colnames(dataset523)=="S_Onset_To_T1_Days") #Getting column number of input column 

i = 1
n <- length(dataset523$S_Onset_To_T1_Days)
for(i in 1:n){
  if (is.na(dataset523[i,155])){dataset523[i,220]<- "NA"}
  else if (dataset523[i,155]>=30 & dataset523[i,155] <60){dataset523[i,220]<-"30-60"}
  else if (dataset523[i,155]>=60 & dataset523[i,155] <90){dataset523[i,220]<-"60-90"}
  else if (dataset523[i,155]>=90 & dataset523[i,155] <120){dataset523[i,220]<-"90-120"}
  else if (dataset523[i,155]>=0 & dataset523[i,155] <30){dataset523[i,220]<-"0-30"}
} 

colnames(dataset523)[220] <- "S_Onset_To_T1_Days_Cat" #Renaming newly created column - S_Onset_To_T1_Days_Cat = Symptom Onset to intial test (T1) in days categorical

table(dataset523$S_Onset_To_T1_Days_Cat)
View(data.frame(dataset523$S_Onset_To_T1_Days, dataset523$S_Onset_To_T1_Days_Cat)) #Looking at the columns  

######
#Creating a categorical variable for the time from time of symptom onset to time of symptom resolution 
#Intervals include 0-25 days, 25-50 days, 50-75 days, and 75-100 days

table(dataset523$S_Onset_To_R_Days)

which(colnames(dataset523)=="S_Onset_To_R_Days") #Getting column number of input column 

i = 1
n <- length(dataset523$S_Onset_To_R_Days)
for(i in 1:n){
  if (is.na(dataset523[i,156])){dataset523[i,221]<- "NA"}
  else if (dataset523[i,156]>=25 & dataset523[i,156] <50){dataset523[i,221]<-"25-50"}
  else if (dataset523[i,156]>=50 & dataset523[i,156] <75){dataset523[i,221]<-"50-75"}
  else if (dataset523[i,156]>=75 & dataset523[i,156] <100){dataset523[i,221]<-"75-100"}
  else if (dataset523[i,156]>=0 & dataset523[i,156] <25){dataset523[i,221]<-"0-25"}
} 

colnames(dataset523)[221] <- "S_Onset_To_R_Days_Cat" #Renaming newly created column - S_Onset_To_R_Days_Cat = Symptom Onset to symptom resolution in days categorical

#Removing character NA's from newly created columns 

dataset523$S_Onset_To_T1_Days_Cat <- ifelse(dataset523$S_Onset_To_T1_Days_Cat == "NA", NA, dataset523$S_Onset_To_T1_Days_Cat)
dataset523$S_Onset_To_R_Days_Cat <- ifelse(dataset523$S_Onset_To_R_Days_Cat == "NA", NA, dataset523$S_Onset_To_R_Days_Cat)

######
#Creating averages for titers for each antibody (IgG, IgM and IgA)

#IgG Mean - new column 222
dataset523$IgGMean <- (dataset523$S1_Spike_IgG_Titer + dataset523$S2_Spike_IgG_Titer + dataset523$RBD_IgG_Titer + dataset523$Nucleoprotein_IgG_Titer)/4 #Mean of IgG - titer for each domain summed and then divided by 4 (4 different domains)

#IgA Mean - new column 223
dataset523$IgMMean <- (dataset523$S1_Spike_IgM_Titer + dataset523$S2_Spike_IgM_Titer + dataset523$RBD_IgM_Titer + dataset523$Nucleoprotein_IgM_Titer)/4 #Mean of IgM - titer for each domain summed and then divided by 4 (4 different domains)

#IgA Mean - new column 224 
dataset523$IgAMean <- (dataset523$S1_Spike_IgA_Titer + dataset523$S2_Spike_IgA_Titer + dataset523$RBD_IgA_Titer + dataset523$Nucleoprotein_IgA_Titer)/4 #Mean of IgA - titer for each domain summed and then divided by 4 (4 different domains)

#####
#Creating new datasets that contain only those positive for each antibody or combinations of antibodies 

#Patients ONLY positive for IgM and not IgG and IgA 
IgM_Only <- dataset523 %>% filter((IgM_Result =="IgM Positive")) 
IgM_Only <- IgM_Only  %>% filter((IgA_Result == "IgA Negative")) 
IgM_Only <- IgM_Only  %>% filter((IgG_Result == "IgG Negative")) 

#Patients ONLY positive for IgG and not IgM and IgA
IgG_Only <- dataset523 %>% filter((IgG_Result =="IgG Positive"))  
IgG_Only <- IgG_Only  %>% filter((IgA_Result == "IgA Negative")) 
IgG_Only <- IgG_Only  %>% filter((IgM_Result == "IgM Negative")) 

#Patients ONLY positive for IgA and not IgG and IgM 
IgA_Only <- dataset523 %>% filter((IgA_Result =="IgA Positive")) 
IgA_Only <- IgA_Only  %>% filter((IgG_Result == "IgG Negative")) 
IgA_Only <- IgA_Only  %>% filter((IgM_Result == "IgM Negative")) 

#Patients ONLY positive for IgM and IgG and not IgA 
IgM_IgG_Only <- dataset523 %>% filter((IgM_Result =="IgM Positive")) 
IgM_IgG_Only <- IgM_IgG_Only  %>% filter((IgG_Result == "IgG Positive")) 
IgM_IgG_Only <- IgM_IgG_Only  %>% filter((IgA_Result == "IgA Negative")) 

#Patients positive for IgM, IgG and IgA 
IgM_IgG_IgA_Pos <- dataset523 %>% filter((IgM_Result =="IgM Positive"))
IgM_IgG_IgA_Pos <- IgM_IgG_IgA_Pos  %>% filter((IgG_Result == "IgG Positive")) 
IgM_IgG_IgA_Pos <- IgM_IgG_IgA_Pos  %>% filter((IgA_Result == "IgA Positive")) 

######
#Not all participants are positive for every antigen/antibody combination so datasets are needed that include only those participants positive or negative for specific antibody/antigen combinations 
#Creating new datasets for use in graphics and inferential stats 

S1_Spike_IgG_Pos <- dataset523 %>% filter((S1_Spike_IgG == "1")) #Only participants positive for S1_Spike IgG
S1_Spike_IgM_Pos <- dataset523 %>% filter((S1_Spike_IgM == "1")) #Only participants positive for S1_Spike IgM
S1_Spike_IgA_Pos <- dataset523 %>% filter((S1_Spike_IgA == "1")) #Only participants positive for S1_Spike IgA

S2_Spike_IgG_Pos <- dataset523 %>% filter((S2_Spike_IgG == "1")) #Only participants positive for S2_Spike IgG
S2_Spike_IgM_Pos <- dataset523 %>% filter((S2_Spike_IgM == "1")) #Only participants positive for S2_Spike IgM
S2_Spike_IgA_Pos <- dataset523 %>% filter((S2_Spike_IgA == "1")) #Only participants positive for S2_Spike IgA

RBD_IgG_Pos <- dataset523 %>% filter((RBD_IgG == "1")) #Only participants positive for RBD IgG
RBD_IgM_Pos <- dataset523 %>% filter((RBD_IgM == "1")) #Only participants positive for RBD IgM
RBD_IgA_Pos <- dataset523 %>% filter((RBD_IgA == "1")) #Only participants positive for RBD IgA

Nucleoprotein_IgG_Pos <- dataset523 %>% filter((Nucleoprotein_IgG == "1")) #Only participants positive for Nucleoprotein IgG
Nucleoprotein_IgM_Pos <- dataset523 %>% filter((Nucleoprotein_IgM == "1")) #Only participants positive for Nucleoprotein IgM
Nucleoprotein_IgA_Pos <- dataset523 %>% filter((Nucleoprotein_IgA == "1")) #Only participants positive for Nucleoprotein IgA 

S1_Spike_IgG_Neg <- dataset523 %>% filter((S1_Spike_IgG == "0")) #Only participants negative for S1_Spike IgG
S1_Spike_IgM_Neg <- dataset523 %>% filter((S1_Spike_IgM == "0")) #Only participants negative for S1_Spike IgM
S1_Spike_IgA_Neg <- dataset523 %>% filter((S1_Spike_IgA == "0")) #Only participants negative for S1_Spike IgA

S2_Spike_IgG_Neg <- dataset523 %>% filter((S2_Spike_IgG == "0")) #Only participants negative for S2_Spike IgG
S2_Spike_IgM_Neg <- dataset523 %>% filter((S2_Spike_IgM == "0")) #Only participants negative for S2_Spike IgM
S2_Spike_IgA_Neg <- dataset523 %>% filter((S2_Spike_IgA == "0")) #Only participants negative for S2_Spike IgA

RBD_IgG_Neg <- dataset523 %>% filter((RBD_IgG == "0")) #Only participants negative for RBD IgG
RBD_IgM_Neg <- dataset523 %>% filter((RBD_IgM == "0")) #Only participants negative for RBD IgM
RBD_IgA_Neg <- dataset523 %>% filter((RBD_IgA == "0")) #Only participants negative for RBD IgA

Nucleoprotein_IgG_Neg <- dataset523 %>% filter((Nucleoprotein_IgG == "0")) #Only participants negative for Nucleoprotein IgG 
Nucleoprotein_IgM_Neg <- dataset523 %>% filter((Nucleoprotein_IgM == "0")) #Only participants negative for Nucleoprotein IgM
Nucleoprotein_IgA_Neg <- dataset523 %>% filter((Nucleoprotein_IgA == "0")) #Only participants negative for Nucleoprotein IgA

#####
#Creating a categorical variable for total symptom severity based on median split (median = 21) - for descriptive stats see "Descriptives_Results_Final.R" script 
# >= 22 is moderate to severe 
# <= 21 is mild to moderate 

dataset523$Symptom_Sev_Cat_1 <- ifelse(dataset523$Symptom_Severity_Total >= 22, "Moderate-Severe", ifelse(dataset523$Symptom_Severity_Total <= 21, "Mild-Moderate", NA)) 

#####
#Creating new columns to use for Figure 1 - Group_All contains a mix of mod/sev, mild/mod and pos-asympomtatic participants

dataset523$Group_All <- coalesce(dataset523$Symptom_Sev_Cat_1, dataset523$Group)

View(data.frame(dataset523$Group_All, dataset523$Symptom_Sev_Cat_1, dataset523$Group)) 

######
#Creating new datasets to use in analysis 

POS <- dataset523 %>% filter((Ig_Test_Result == "Positive")) #Only positive test results 

NEG <- dataset523 %>% filter((Ig_Test_Result == "Negative")) #Only negative test results 

NEG_symptomatic <- dataset523 %>% filter((Group == "Negative (Symptomatic)")) #Only negative symptomatic participants 

NEG_asymptomatic <- dataset523 %>% filter((Group == "Negative (Asymptomatic)")) #Only negative asymptomatic participants 

POS_symptomatic <- dataset523 %>% filter((Group == "Positive (Symptomatic)")) #Only positive symptomatic participants 

POS_asymptomatic <- dataset523 %>% filter((Group == "Positive (Asymptomatic)")) #Only positive asymptomatic participants

#Only positive, symptomatic participants that are included in the time from symptom onset to intial serological test date groups 
Symptom_Onset_POS <- POS %>% filter((S_Onset_To_T1_Days_Cat == "0-30") | (S_Onset_To_T1_Days_Cat == "30-60") | (S_Onset_To_T1_Days_Cat == "60-90") | (S_Onset_To_T1_Days_Cat == "90-120"))

Sym_Cat_MM <- POS %>% filter((Symptom_Sev_Cat_1 == "Mild-Moderate")) #Only participants with mild-moderate symptom severity

Sym_Cat_MS <- POS %>% filter((Symptom_Sev_Cat_1 == "Moderate-Severe")) #Only participants with moderate-severe symptom severity

#######
#Converting data from wide to long to use in Supplemental Figures 1-3 
library(reshape2)

#Creating new data.frame with only the columns to be used in new long form dataset 
df_long_prep<- dataset523 %>% select(GUID, Ig_Test_Result, Group, Gender, Symptom_Sev_Cat_1,Group_All, S1_Spike_IgG_Titer, S2_Spike_IgG_Titer, RBD_IgG_Titer, Nucleoprotein_IgG_Titer, S1_Spike_IgM_Titer, S2_Spike_IgM_Titer, RBD_IgM_Titer, Nucleoprotein_IgM_Titer, S1_Spike_IgA_Titer, S2_Spike_IgA_Titer, RBD_IgA_Titer, Nucleoprotein_IgA_Titer)

#Using gather function to convert from wide to long 
dataset523_long <-gather(df_long_prep, test, value, S1_Spike_IgG_Titer:Nucleoprotein_IgA_Titer, factor_key=TRUE)

#Viewing newly created data.frame 
View(dataset523_long)
