#Figure 3 - Final 
#Heatmap of IgG, IgA, and IgM titers against SARS-CoV-2 antigens in asymptomatic and symptomatic COVID-19-positive participants. 

######
#Libraries used 
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
install.packages("dichromat")

library(ComplexHeatmap)
library(circlize)
library(tidyverse)
library(dichromat)

a #Update packages 

######
#All positive 
POS_1 <- POS %>% filter((Group_All == "Moderate-Severe") | (Group_All == "Mild-Moderate") | (Group_All == "Positive (Asymptomatic)")) 
POS_1 <- tibble::rowid_to_column(POS_1, "ID")#Add ID column to data.frame 
POS_1 <- POS_1 %>% unite(Group_All_ID, c("Group_All", "ID"), sep = "_", remove = TRUE, na.rm = FALSE)#Combine group and ID 
View(POS_1)

#Create new data.frame with only data to be used 
POS_1_HM <- data.frame(POS_1$Group_All_ID, POS_1$S1_Spike_IgM_Titer, POS_1$S1_Spike_IgG_Titer, POS_1$S1_Spike_IgA_Titer, POS_1$S2_Spike_IgM_Titer, POS_1$S2_Spike_IgG_Titer, POS_1$S2_Spike_IgA_Titer, POS_1$RBD_IgM_Titer, POS_1$RBD_IgG_Titer, POS_1$RBD_IgA_Titer, POS_1$Nucleoprotein_IgM_Titer, POS_1$Nucleoprotein_IgG_Titer, POS_1$Nucleoprotein_IgA_Titer, row.names=1)
names(POS_1_HM) <- c("S1 SP IgM", "S1 SP IgG", "S1 SP IgA", "S2 SP IgM", "S2 SP IgG", "S2 SP IgA", "RBD IgM", "RBD IgG", "RBD IgA", "NP IgM", "NP IgG", "NP IgA")#Rename column headers 
POS_1_HM_ordered <- POS_1_HM[ order(row.names(POS_1_HM)), ]#Order data
View(POS_1_HM_ordered)

POS_1_HM_ordered <- POS_1_HM_ordered[-c(6),]#Remove outlier (NP IgA - Mild-Moderate_22)
View(POS_1_HM_ordered)

#Creating data matrix for use in heatmap 
POS_HM_matrix_1 <- as.matrix(POS_1_HM_ordered)
View(POS_HM_matrix_1)

######
#Generating heatmap 
split = data.frame(rep(c("Mild-Moderate (10)", "Moderate-Severe (10)", "Asymptomatic (7)"), c(10, 10, 7)))

my_palette <- colorRampPalette(c("white","grey","black"))(n = 299)#Creating color pallete 

Heatmap(POS_HM_matrix_1, 
        name="Titer",
        clustering_method_columns = "complete",
        clustering_distance_columns = "euclidean",
        cluster_columns = TRUE,
        cluster_rows = FALSE,
        col = my_palette,
        show_row_dend = FALSE, 
        show_column_dend = FALSE,
        column_km=0,
        show_row_names = FALSE,
        row_gap = unit(3, "mm"), 
        cluster_row_slices=FALSE, 
        row_split = split, 
        border = TRUE)

######
#Export CSV with data used 
write.csv(POS_HM_matrix_1,file="Figure_3_Source_Data.csv")

