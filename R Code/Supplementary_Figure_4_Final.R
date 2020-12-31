### Supplementary Figure 4 - Final 
#Correlations between titers against SARS-CoV-2 antigens between COVID-19-positive versus –negative participants, and between COVID-19-positive participants with varied symptomology.

######
#Libraries used 
library(ggcorrplot)
library(cowplot)

######
###Mild-Moderate 

corr_df_MM <- data.frame(Sym_Cat_MM$S1_Spike_IgG_Titer, Sym_Cat_MM$S1_Spike_IgM_Titer, Sym_Cat_MM$S1_Spike_IgA_Titer, Sym_Cat_MM$S2_Spike_IgG_Titer, Sym_Cat_MM$S2_Spike_IgM_Titer, Sym_Cat_MM$S2_Spike_IgA_Titer, Sym_Cat_MM$RBD_IgG_Titer, Sym_Cat_MM$RBD_IgM_Titer, Sym_Cat_MM$RBD_IgA_Titer, Sym_Cat_MM$Nucleoprotein_IgG_Titer, Sym_Cat_MM$Nucleoprotein_IgM_Titer, Sym_Cat_MM$Nucleoprotein_IgA_Titer)
names(corr_df_MM) <- c("S1 SP IgG", "S1 SP IgM", "S1 SP IgA", "S2 SP IgG", "S2 SP IgM", "S2 SP IgA", "RBD IgG", "RBD IgM", "RBD IgA", "NP IgG", "NP IgM", "NP IgA")
View(corr_df_MM)

corr_MM <- round(cor(corr_df_MM), 1)
View(corr_MM)

FigureS4B_MM <- ggcorrplot(corr_MM, hc.order = FALSE, method="square", 
           type = "lower",
           lab = TRUE, outline.col = "white",
           ggtheme = ggplot2::theme_minimal,
           colors = c("white", "grey", "black"), 
           lab_col = "white", lab_size=6, 
           title = "COVID-19-Positive Mild-Moderate (11)",
           legend.title = "Coefficient")+ 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title = element_text(size = 12), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
FigureS4B_MM

#Export CSV with data used
write.csv(corr_df_MM,file="Supplementary_Figure_4B_Mild_Moderate_Source_Code.csv")

###Moderate-Severe

corr_df_MS <- data.frame(Sym_Cat_MS$S1_Spike_IgG_Titer, Sym_Cat_MS$S1_Spike_IgM_Titer, Sym_Cat_MS$S1_Spike_IgA_Titer, Sym_Cat_MS$S2_Spike_IgG_Titer, Sym_Cat_MS$S2_Spike_IgM_Titer, Sym_Cat_MS$S2_Spike_IgA_Titer, Sym_Cat_MS$RBD_IgG_Titer, Sym_Cat_MS$RBD_IgM_Titer, Sym_Cat_MS$RBD_IgA_Titer, Sym_Cat_MS$Nucleoprotein_IgG_Titer, Sym_Cat_MS$Nucleoprotein_IgM_Titer, Sym_Cat_MS$Nucleoprotein_IgA_Titer)
names(corr_df_MS) <- c("S1 SP IgG", "S1 SP IgM", "S1 SP IgA", "S2 SP IgG", "S2 SP IgM", "S2 SP IgA", "RBD IgG", "RBD IgM", "RBD IgA", "NP IgG", "NP IgM", "NP IgA")
View(corr_df_MS)

corr_MS <- round(cor(corr_df_MS), 1)
View(corr_MS)

FigureS4B_MS <- ggcorrplot(corr_MS, hc.order = FALSE, method="square", 
           type = "lower",
           lab = TRUE, outline.col = "white",
           ggtheme = ggplot2::theme_minimal,
           colors = c("white", "grey", "black"), 
           lab_col = "white", lab_size=6,
           title = "COVID-19-Positive Moderate-Severe (10)",
           legend.title = "Coefficient")+
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title = element_text(size = 12), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
FigureS4B_MS

#Export CSV with data used
write.csv(corr_df_MS,file="Supplementary_Figure_4B_Moderate_Severe_Source_Code.csv")

###Asymptomatic

corr_df_PA <- data.frame(POS_asymptomatic$S1_Spike_IgG_Titer, POS_asymptomatic$S1_Spike_IgM_Titer, POS_asymptomatic$S1_Spike_IgA_Titer, POS_asymptomatic$S2_Spike_IgG_Titer, POS_asymptomatic$S2_Spike_IgM_Titer, POS_asymptomatic$S2_Spike_IgA_Titer, POS_asymptomatic$RBD_IgG_Titer, POS_asymptomatic$RBD_IgM_Titer, POS_asymptomatic$RBD_IgA_Titer, POS_asymptomatic$Nucleoprotein_IgG_Titer, POS_asymptomatic$Nucleoprotein_IgM_Titer, POS_asymptomatic$Nucleoprotein_IgA_Titer)
names(corr_df_PA) <- c("S1 SP IgG", "S1 SP IgM", "S1 SP IgA", "S2 SP IgG", "S2 SP IgM", "S2 SP IgA", "RBD IgG", "RBD IgM", "RBD IgA", "NP IgG", "NP IgM", "NP IgA")
View(corr_df_PA)

corr_PA <- round(cor(corr_df_PA), 1)
View(corr_PA)

FigureS4B_PA <- ggcorrplot(corr_PA, hc.order = FALSE, method="square", 
           type = "lower",
           lab = TRUE, outline.col = "white",
           ggtheme = ggplot2::theme_minimal,
           colors = c("white", "grey", "black"), 
           lab_col = "white", lab_size=6, 
           legend.title = "Coefficient", title="COVID-19-Positive Asymptomatic (7)")+ 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title = element_text(size = 12), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
FigureS4B_PA

#Export CSV with data used
write.csv(corr_df_PA,file="Supplementary_Figure_4B_Positive_Asymptomatic_Source_Code.csv")


###All positives

corr_df_P <- data.frame(POS$S1_Spike_IgG_Titer, POS$S1_Spike_IgM_Titer, POS$S1_Spike_IgA_Titer, POS$S2_Spike_IgG_Titer, POS$S2_Spike_IgM_Titer, POS$S2_Spike_IgA_Titer, POS$RBD_IgG_Titer, POS$RBD_IgM_Titer, POS$RBD_IgA_Titer, POS$Nucleoprotein_IgG_Titer, POS$Nucleoprotein_IgM_Titer, POS$Nucleoprotein_IgA_Titer)
names(corr_df_P) <- c("S1 SP IgG", "S1 SP IgM", "S1 SP IgA", "S2 SP IgG", "S2 SP IgM", "S2 SP IgA", "RBD IgG", "RBD IgM", "RBD IgA", "NP IgG", "NP IgM", "NP IgA")
View(corr_df_P)

corr_P <- round(cor(corr_df_P), 1)
View(corr_P)

FigureS4A_P <- ggcorrplot(corr_P, hc.order = FALSE, method="square", 
                 type = "lower",
                 lab = TRUE, outline.col = "white",
                 ggtheme = ggplot2::theme_minimal,
                 colors = c("white", "grey", "black"), 
                 lab_col = "white", lab_size=6, 
                 legend.title = "Coefficient", title="All COVID-19-Positive (31)")+ 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title = element_text(size = 12))
FigureS4A_P

#Export CSV with data used
write.csv(corr_df_P,file="Supplementary_Figure_4A_All_Positive_Source_Code.csv")

###All Negatives

corr_df_N <- data.frame(NEG$S1_Spike_IgG_Titer, NEG$S1_Spike_IgM_Titer, NEG$S1_Spike_IgA_Titer, NEG$S2_Spike_IgG_Titer, NEG$S2_Spike_IgM_Titer, NEG$S2_Spike_IgA_Titer, NEG$RBD_IgG_Titer, NEG$RBD_IgM_Titer, NEG$RBD_IgA_Titer, NEG$Nucleoprotein_IgG_Titer, NEG$Nucleoprotein_IgM_Titer, NEG$Nucleoprotein_IgA_Titer)
names(corr_df_N) <- c("S1 SP IgG", "S1 SP IgM", "S1 SP IgA", "S2 SP IgG", "S2 SP IgM", "S2 SP IgA", "RBD IgG", "RBD IgM", "RBD IgA", "NP IgG", "NP IgM", "NP IgA")
View(corr_df_N)

corr_df_N <- corr_df_N[-c(10,45,47,62,65),] 

corr_N <- round(cor(corr_df_N), 1)
View(corr_N)

FigureS4A_N <- ggcorrplot(corr_N, hc.order = FALSE, method="square", 
                type = "lower",
                lab = TRUE, outline.col = "white",
                ggtheme = ggplot2::theme_minimal,
                colors = c("white", "grey", "black"), 
                lab_col = "white", lab_size=6, 
                legend.title = "Coefficient", title="All COVID-19-Negative (76)")+ 
  theme(plot.title = element_text(hjust = 0.5, size=20), legend.title = element_text(size = 12))
FigureS4A_N

#Export CSV with data used
write.csv(corr_df_N,file="Supplementary_Figure_4A_All_Negative_Source_Code.csv")

######
#Final Panel 

library(cowplot)

panel_a <- plot_grid(FigureS4A_P,FigureS4A_N, ncol=2) 

panel_b <- plot_grid(FigureS4B_PA,FigureS4B_MM,FigureS4B_MS, ncol=3) 

plot_grid(panel_a, panel_b,
          labels = c("a", "b"),label_size = 25,
          nrow = 2)
