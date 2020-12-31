#Supplementary Figure 1 - Final 
#Comparison of titers in all participants positive and negative for IgA,IgG, and IgM antibodies against SARS-CoV-2 antigens.

######
#Libraries used 
library(ggplot2)
library(tidyverse)
library(ggsignif)
library(cowplot)

#Loop to assign 'Positive' or 'Negative' to each value for each antigen-antibody combination
dataset523_long$Ig_Test_Result_updated <- ifelse(dataset523_long$value >= 1, "Positive", ifelse(dataset523_long$value < 1, "Negative", NA)) 

######
#Panel A 
#S1 Spike IgG 

dataset523_long_S1_IgG <- dataset523_long %>% filter((test == "S1_Spike_IgG_Titer"))
dataset523_long_S1_IgG <- dataset523_long_S1_IgG %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S1_IgG)

FigureS1A_S1 <- ggplot(dataset523_long_S1_IgG, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "S1 SP IgG (AU/mL)")

FigureS1A_S1

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S1_IgG$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_S1_IgG, var.equal = TRUE)
res

#Panel A 
#S2 Spike IgG 

dataset523_long_S2_IgG <- dataset523_long %>% filter((test == "S2_Spike_IgG_Titer"))
dataset523_long_S2_IgG <- dataset523_long_S2_IgG %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S2_IgG)

FigureS1A_S2 <- ggplot(dataset523_long_S2_IgG, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "S2 SP IgG (AU/mL)")

FigureS1A_S2

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S2_IgG$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_S2_IgG, var.equal = TRUE)
res

#Panel A 
#RBD IgG 

dataset523_long_RBD_IgG <- dataset523_long %>% filter((test == "RBD_IgG_Titer"))
dataset523_long_RBD_IgG <- dataset523_long_RBD_IgG %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_RBD_IgG)

FigureS1A_RBD <- ggplot(dataset523_long_RBD_IgG, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "RBD IgG (AU/mL)")

FigureS1A_RBD

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_RBD_IgG$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_RBD_IgG, var.equal = TRUE)
res

#Panel A 
#Nucleoprotein IgG 

dataset523_long_NP_IgG <- dataset523_long %>% filter((test == "Nucleoprotein_IgG_Titer"))
dataset523_long_NP_IgG <- dataset523_long_NP_IgG %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_NP_IgG)

FigureS1A_NP <- ggplot(dataset523_long_NP_IgG, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "NP IgG (AU/mL)")

FigureS1A_NP

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_NP_IgG$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_NP_IgG, var.equal = TRUE)
res

#####
#Panel B 
#S1 Spike IgM

dataset523_long_S1_IgM <- dataset523_long %>% filter((test == "S1_Spike_IgM_Titer"))
dataset523_long_S1_IgM <- dataset523_long_S1_IgM %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S1_IgM)

FigureS1B_S1 <- ggplot(dataset523_long_S1_IgM, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "S1 SP IgM (AU/mL)")

FigureS1B_S1

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S1_IgM$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_S1_IgM, var.equal = TRUE)
res

#S2 Spike IgM 

dataset523_long_S2_IgM <- dataset523_long %>% filter((test == "S2_Spike_IgM_Titer"))
dataset523_long_S2_IgM <- dataset523_long_S2_IgM %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S2_IgM)

FigureS1B_S2 <- ggplot(dataset523_long_S2_IgM, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "S2 SP IgM (AU/mL)")

FigureS1B_S2

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S2_IgM$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_S2_IgM, var.equal = TRUE)
res

#RBD IgM 

dataset523_long_RBD_IgM <- dataset523_long %>% filter((test == "RBD_IgM_Titer"))
dataset523_long_RBD_IgM <- dataset523_long_RBD_IgM %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_RBD_IgM)

FigureS1B_RBD <- ggplot(dataset523_long_RBD_IgM, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "RBD IgM (AU/mL)")

FigureS1B_RBD

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_RBD_IgM$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_RBD_IgM, var.equal = TRUE)
res

#Nucleoprotein IgM 

dataset523_long_NP_IgM <- dataset523_long %>% filter((test == "Nucleoprotein_IgM_Titer"))
dataset523_long_NP_IgM <- dataset523_long_NP_IgM %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_NP_IgM)

FigureS1B_NP <- ggplot(dataset523_long_NP_IgM, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "NP IgM (AU/mL)")

FigureS1B_NP

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_NP_IgM$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_NP_IgM, var.equal = TRUE)
res


#####
#Panel C
#S1 Spike IgA

dataset523_long_S1_IgA <- dataset523_long %>% filter((test == "S1_Spike_IgA_Titer"))
dataset523_long_S1_IgA <- dataset523_long_S1_IgA %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S1_IgA)

FigureS1C_S1 <- ggplot(dataset523_long_S1_IgA, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "S1 SP IgA (AU/mL)")

FigureS1C_S1

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S1_IgA$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_S1_IgA, var.equal = TRUE)
res

#S2 Spike IgA 

dataset523_long_S2_IgA <- dataset523_long %>% filter((test == "S2_Spike_IgA_Titer"))
dataset523_long_S2_IgA <- dataset523_long_S2_IgA %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_S2_IgA)

FigureS1C_S2 <- ggplot(dataset523_long_S2_IgA, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "S2 SP IgA (AU/mL)")

FigureS1C_S2

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_S2_IgA$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_S2_IgA, var.equal = TRUE)
res

#RBD IgA 

dataset523_long_RBD_IgA <- dataset523_long %>% filter((test == "RBD_IgA_Titer"))
dataset523_long_RBD_IgA <- dataset523_long_RBD_IgA %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_RBD_IgA)

FigureS1C_RBD <- ggplot(dataset523_long_RBD_IgA, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "RBD IgA (AU/mL)")

FigureS1C_RBD

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_RBD_IgA$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_RBD_IgA, var.equal = TRUE)
res

#Nucleoprotein IgA 

dataset523_long_NP_IgA <- dataset523_long %>% filter((test == "Nucleoprotein_IgA_Titer"))
dataset523_long_NP_IgA <- dataset523_long_NP_IgA %>% filter((Ig_Test_Result_updated == "Positive" | Ig_Test_Result_updated == "Negative"))
View(dataset523_long_NP_IgA)

FigureS1C_NP <- ggplot(dataset523_long_NP_IgA, aes(x = Ig_Test_Result_updated, y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Ig_Test_Result_updated, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(4.5), xmin = c(1), 
              xmax = c(2), annotation = c("***"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 5.5)) + scale_y_continuous(breaks=seq(0, 5.5, 0.5))+ 
  labs(x = "Group", y = "NP IgA (AU/mL)")

FigureS1C_NP

#Five people didn't have titer values so they are not included - total n=102 
table(dataset523_long_NP_IgA$Ig_Test_Result_updated)

res <- t.test(value ~ Ig_Test_Result_updated, data = dataset523_long_NP_IgA, var.equal = TRUE)
res

#####
#Creating final plot 
plot_grid(FigureS1A_S1, FigureS1A_S2, FigureS1A_RBD, FigureS1A_NP, 
          FigureS1B_S1, FigureS1B_S2, FigureS1B_RBD, FigureS1B_NP, 
          FigureS1C_S1, FigureS1C_S2, FigureS1C_RBD, FigureS1C_NP,
          labels = c("a", "", "","", "b", "", "","", "c", "", "",""),label_size = 25,
          ncol = 2, nrow = 6)

######
#Export CSV with data used 
write.csv(dataset523_long,file="Supplementary_Figure_1_Source_Data.csv")
