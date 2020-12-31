#Supplementary Figure 2 - Final 
#Comparison of IgA, IgG, and IgM antibody titers against SARS-CoV-2 antigens in COVID-19-positive and -negative females and males.

######
#Libraries used 
library(ggplot2)
library(tidyverse)
library(ggsignif)
library(cowplot)
library(tidyr)

######
#Creating dataset with Gender_Test_Result column = merged Gender column with Test Results (negative/positive)
dataset523_long_SF2 <- dataset523_long %>% unite(Gender_Test_Result, c("Ig_Test_Result_updated", "Gender"), sep = ": ", remove = FALSE, na.rm = FALSE)
View(dataset523_long_SF2)

#####
#Panel A 
#S1 Spike IgG 

dataset523_long_S1_IgG_SF2 <- dataset523_long_SF2 %>% filter((test == "S1_Spike_IgG_Titer"))
dataset523_long_S1_IgG_SF2 <- dataset523_long_S1_IgG_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S1_IgG_SF2)

FigureS2A_S1 <- ggplot(dataset523_long_S1_IgG_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "S1 SP IgG (AU/mL)")

FigureS2A_S1

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S1_IgG_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_S1_IgG_SF2_Neg <- dataset523_long_S1_IgG_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_S1_IgG_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S1_IgG_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_S1_IgG_SF2_Pos <- dataset523_long_S1_IgG_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_S1_IgG_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S1_IgG_SF2_Pos, var.equal = TRUE)
res

#S2 Spike IgG 

dataset523_long_S2_IgG_SF2 <- dataset523_long_SF2 %>% filter((test == "S2_Spike_IgG_Titer"))
dataset523_long_S2_IgG_SF2 <- dataset523_long_S2_IgG_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S2_IgG_SF2)

FigureS2A_S2 <- ggplot(dataset523_long_S2_IgG_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "S2 SP IgG (AU/mL)")

FigureS2A_S2

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S2_IgG_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_S2_IgG_SF2_Neg <- dataset523_long_S2_IgG_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_S2_IgG_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S2_IgG_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_S2_IgG_SF2_Pos <- dataset523_long_S2_IgG_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_S2_IgG_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S2_IgG_SF2_Pos, var.equal = TRUE)
res

#RBD Spike IgG 

dataset523_long_RBD_IgG_SF2 <- dataset523_long_SF2 %>% filter((test == "RBD_IgG_Titer"))
dataset523_long_RBD_IgG_SF2 <- dataset523_long_RBD_IgG_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_RBD_IgG_SF2)

FigureS2A_RBD <- ggplot(dataset523_long_RBD_IgG_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "RBD IgG (AU/mL)")

FigureS2A_RBD

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_RBD_IgG_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_RBD_IgG_SF2_Neg <- dataset523_long_RBD_IgG_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_RBD_IgG_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_RBD_IgG_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_RBD_IgG_SF2_Pos <- dataset523_long_RBD_IgG_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_RBD_IgG_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_RBD_IgG_SF2_Pos, var.equal = TRUE)
res

#Nucleoprotein Spike IgG 

dataset523_long_NP_IgG_SF2 <- dataset523_long_SF2 %>% filter((test == "Nucleoprotein_IgG_Titer"))
dataset523_long_NP_IgG_SF2 <- dataset523_long_NP_IgG_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_NP_IgG_SF2)

FigureS2A_NP <- ggplot(dataset523_long_NP_IgG_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "NP IgG (AU/mL)")

FigureS2A_NP

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_NP_IgG_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_NP_IgG_SF2_Neg <- dataset523_long_NP_IgG_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_NP_IgG_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_NP_IgG_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_NP_IgG_SF2_Pos <- dataset523_long_NP_IgG_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_NP_IgG_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_NP_IgG_SF2_Pos, var.equal = TRUE)
res

#######
#Panel B
#S1 Spike IgM 

dataset523_long_S1_IgM_SF2 <- dataset523_long_SF2 %>% filter((test == "S1_Spike_IgM_Titer"))
dataset523_long_S1_IgM_SF2 <- dataset523_long_S1_IgM_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S1_IgM_SF2)

FigureS2B_S1 <- ggplot(dataset523_long_S1_IgM_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "S1 SP IgM (AU/mL)")

FigureS2B_S1

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S1_IgM_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_S1_IgM_SF2_Neg <- dataset523_long_S1_IgM_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_S1_IgM_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S1_IgM_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_S1_IgM_SF2_Pos <- dataset523_long_S1_IgM_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_S1_IgM_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S1_IgM_SF2_Pos, var.equal = TRUE)
res

#S2 Spike IgM 

dataset523_long_S2_IgM_SF2 <- dataset523_long_SF2 %>% filter((test == "S2_Spike_IgM_Titer"))
dataset523_long_S2_IgM_SF2 <- dataset523_long_S2_IgM_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S2_IgM_SF2)

FigureS2B_S2 <- ggplot(dataset523_long_S2_IgM_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "S2 SP IgM (AU/mL)")

FigureS2B_S2

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S2_IgM_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_S2_IgM_SF2_Neg <- dataset523_long_S2_IgM_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_S2_IgM_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S2_IgM_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_S2_IgM_SF2_Pos <- dataset523_long_S2_IgM_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_S2_IgM_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S2_IgM_SF2_Pos, var.equal = TRUE)
res

#RBD Spike IgM 

dataset523_long_RBD_IgM_SF2 <- dataset523_long_SF2 %>% filter((test == "RBD_IgM_Titer"))
dataset523_long_RBD_IgM_SF2 <- dataset523_long_RBD_IgM_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_RBD_IgM_SF2)

FigureS2B_RBD <- ggplot(dataset523_long_RBD_IgM_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "RBD IgM (AU/mL)")

FigureS2B_RBD

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_RBD_IgM_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_RBD_IgM_SF2_Neg <- dataset523_long_RBD_IgM_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_RBD_IgM_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_RBD_IgM_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_RBD_IgM_SF2_Pos <- dataset523_long_RBD_IgM_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_RBD_IgM_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_RBD_IgM_SF2_Pos, var.equal = TRUE)
res

#Nucleoprotein Spike IgM 

dataset523_long_NP_IgM_SF2 <- dataset523_long_SF2 %>% filter((test == "Nucleoprotein_IgM_Titer"))
dataset523_long_NP_IgM_SF2 <- dataset523_long_NP_IgM_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_NP_IgM_SF2)

FigureS2B_NP <- ggplot(dataset523_long_NP_IgM_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "NP IgM (AU/mL)")

FigureS2B_NP

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_NP_IgM_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_NP_IgM_SF2_Neg <- dataset523_long_NP_IgM_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_NP_IgM_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_NP_IgM_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_NP_IgM_SF2_Pos <- dataset523_long_NP_IgM_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_NP_IgM_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_NP_IgM_SF2_Pos, var.equal = TRUE)
res

#######
#Panel C
#S1 Spike IgA 

dataset523_long_S1_IgA_SF2 <- dataset523_long_SF2 %>% filter((test == "S1_Spike_IgA_Titer"))
dataset523_long_S1_IgA_SF2 <- dataset523_long_S1_IgA_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S1_IgA_SF2)

FigureS2C_S1 <- ggplot(dataset523_long_S1_IgA_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "S1 SP IgA (AU/mL)")

FigureS2C_S1

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S1_IgA_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_S1_IgA_SF2_Neg <- dataset523_long_S1_IgA_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_S1_IgA_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S1_IgA_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_S1_IgA_SF2_Pos <- dataset523_long_S1_IgA_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_S1_IgA_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S1_IgA_SF2_Pos, var.equal = TRUE)
res

#S2 Spike IgA 

dataset523_long_S2_IgA_SF2 <- dataset523_long_SF2 %>% filter((test == "S2_Spike_IgA_Titer"))
dataset523_long_S2_IgA_SF2 <- dataset523_long_S2_IgA_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S2_IgA_SF2)

FigureS2C_S2 <- ggplot(dataset523_long_S2_IgA_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "S2 SP IgA (AU/mL)")

FigureS2C_S2

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S2_IgA_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_S2_IgA_SF2_Neg <- dataset523_long_S2_IgA_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_S2_IgA_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S2_IgA_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_S2_IgA_SF2_Pos <- dataset523_long_S2_IgA_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_S2_IgA_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_S2_IgA_SF2_Pos, var.equal = TRUE)
res

#RBD Spike IgA 

dataset523_long_RBD_IgA_SF2 <- dataset523_long_SF2 %>% filter((test == "RBD_IgA_Titer"))
dataset523_long_RBD_IgA_SF2 <- dataset523_long_RBD_IgA_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_RBD_IgA_SF2)

FigureS2C_RBD <- ggplot(dataset523_long_RBD_IgA_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "RBD IgA (AU/mL)")

FigureS2C_RBD

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_RBD_IgA_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_RBD_IgA_SF2_Neg <- dataset523_long_RBD_IgA_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_RBD_IgA_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_RBD_IgA_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_RBD_IgA_SF2_Pos <- dataset523_long_RBD_IgA_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_RBD_IgA_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_RBD_IgA_SF2_Pos, var.equal = TRUE)
res

#Nucleoprotein Spike IgA 

dataset523_long_NP_IgA_SF2 <- dataset523_long_SF2 %>% filter((test == "Nucleoprotein_IgA_Titer"))
dataset523_long_NP_IgA_SF2 <- dataset523_long_NP_IgA_SF2 %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_NP_IgA_SF2)

FigureS2C_NP <- ggplot(dataset523_long_NP_IgA_SF2, aes(x = Gender_Test_Result, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Gender_Test_Result, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  coord_cartesian(ylim=c(0, 4.5)) + scale_y_continuous(breaks=seq(0, 4.5, 0.5))+ 
  labs(x = "Group", y = "NP IgA (AU/mL)")

FigureS2C_NP

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_NP_IgA_SF2$Gender_Test_Result)

#T-test Male Negative vs. Female Negative 
dataset523_long_NP_IgA_SF2_Neg <- dataset523_long_NP_IgA_SF2 %>% filter((Gender_Test_Result == "Negative: Male" | Gender_Test_Result == "Negative: Female")) 
View(dataset523_long_NP_IgA_SF2_Neg)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_NP_IgA_SF2_Neg, var.equal = TRUE)
res

#T-test Male Positive vs. Female Positive 
dataset523_long_NP_IgA_SF2_Pos <- dataset523_long_NP_IgA_SF2 %>% filter((Gender_Test_Result == "Positive: Male" | Gender_Test_Result == "Positive: Female")) 
View(dataset523_long_NP_IgA_SF2_Pos)

res <- t.test(value ~ Gender_Test_Result, data = dataset523_long_NP_IgA_SF2_Pos, var.equal = TRUE)
res

#####
#Creating final plot
plot_grid(FigureS2A_S1, FigureS2A_S2, FigureS2A_RBD, FigureS2A_NP, 
          FigureS2B_S1, FigureS2B_S2, FigureS2B_RBD, FigureS2B_NP, 
          FigureS2C_S1, FigureS2C_S2, FigureS2C_RBD, FigureS2C_NP,
          labels = c("a", "", "","", "b", "", "","", "c", "", "",""),label_size = 25,
          ncol = 2, nrow = 6)

######
#Export CSV with data used 
write.csv(dataset523_long_SF2,file="Supplementary_Figure_2_Source_Code.csv")



