# Grossberg-Koza-et-al-2020
This repository contains R code and source data for the following paper: Grossberg &amp; Koza et al. (2020) A multiplex chemiluminescent immunoassay for serological profiling of COVID-19-positive symptomatic and asymptomatic patients. Nature Communications. 

The folder "data" contains all the data required to recreate these figures as well as the de-indentified raw serological/questionnaire data. 

Data presented in Table 1 was analyzed by Vibrant Clinical Labs. For raw data included in Table 1, please contact Hari Krishnamurthy at Vibrant Labs (hari@vibrantsci.com). A more comprehensive analysis of the sensitivity and specificity of the Vibrant chemiluminescent immunoassay for SARS-CoV-2 has recently been published here: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0242655#sec006. 

R scripts for cleaning the dataset, analyzing the data and creating the tables and figures are included in the repository:

- The script "Data_Cleaning_Final.R" contains code used to create new columns and data frames used in analyses and figures. 

- The script "Descriptive_Statistics_Results_Final.R" contains statistics presented in the results section of the paper - each section of code is matched to the correct heading from the publication. 

- The scripts "Table_2_Final.R", "Table_3_Final.R" and "Supplementary_Table_1_Final.R" contains code used to generate the values presented in Tables 2 and 3 and Supplementary Table 1. Table 2 investigates demographic and clinical characteristics of the Colorado cohort including COVID-19-positive and COVID-19-negative participants. Table 3 investigates IgM, IgA, and IgG titers against SARS-CoV-2 spike 1 glycoprotein (S1 SP), spike 2 glycoprotein (S2 SP), receptor binding domain (RBD), and nucleoprotein (NP) in symptomatic and asymptomatic COVID-19-positive participants. Supplemental Table 3 contains reported symptoms and range of symptom severity in symptomatic COVID-19 positive and symptomatic COVID-19 negative participants.

- The script "Figure_1_Final.R" contains code used to generate Figure 1. Figure 1 shows IgA antibody titers against receptor binding domain (RBD) and spike 2 glycoprotein (S2 SP) and IgG antibody titers against nucleoprotein plotted against symptom severity in COVID-19-positive participants. 

- The script "Figure_2_Final.R" contains code used to generate Figure 2. Figure 2 shows IgG and IgM antibody titers plotted against days between symptom onset and serological test for COVID-19-positive participants. 

- The script "Figure_3_Final.R" contains code used to generate Figure 3. Figure 3 is a heatmap that shows differences in IgG, IgA, and IgM positivity against SARS-CoV-2 antigens in asymptomatic, mild-moderate and moderate-severe COVID-19-positive participants. 

- The script "Supplementary_Figure_1_Final.R" contains code used to generate Supplementary Figure 1. Supplementary Figure 1 compares titers between COVID-19-positive and -negative participants for each antibody/antigen combination.

- The script "Supplementary_Figure_2_Final.R" contains code used to generate Supplementary Figure 2. Supplementary Figure 2 shows a comparison of IgA, IgG, and IgM antibody titers against SARS-CoV-2 antigens in COVID-19-positive and -negative females and males.

- The script "Supplementary_Figure_3_Final.R" contains code used to generate Supplementary Figure 3. Supplementary Figure 3 shows IgA antibody titers against receptor binding domain (RBD) and spike 2 glycoprotein (S2 SP) and IgG antibody titers against nucleoprotein in COVID-19-positive female and male participants with varying symptomology.

- The script "Supplementary_Figure_4_Final.R" contains code used to generate Supplementary Figure 4. Supplementary Figure 4 shows correlation plots between titers against SARS-CoV-2 antigens in COVID-19-positive versus –negative participants, and between COVID-19-positive participants with varied symptom severity.

- The script "Supplementary_Figure_5_Supplementary_Table_2_Final.R" contains code used to generate Supplementary Figure 5 and Supplementary Table 2. Supplementary Figure 5 shows plots of the General Additive Models fit to the data shown in Figure 2. Supplementary Table 2 shows Chi-Square test data to compare the fits of the General Additive Models and Genereral Linear Models fit to the data shown in Figure 2. 
