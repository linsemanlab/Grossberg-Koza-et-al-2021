#Supplementary Figure 3 - Final 
#Comparison of IgA antibody titers against receptor binding domain (RBD) and spike 2 glycoprotein (S2 SP) and IgG antibody titers against nucleoprotein in COVID-19-positive female and male participants with varying symptomology.

######
#Libraries used 
library(ggplot2)
library(tidyverse)
library(ggsignif)
library(cowplot)

#######

dataset523_long$Ig_Test_Result_updated <- ifelse(dataset523_long$value >= 1, "Positive", ifelse(dataset523_long$value < 1, "Negative", NA)) 

#####
#Panel A - RBD IgA 

dataset523_long_RBD_IgA <- dataset523_long %>% filter((test == "RBD_IgA_Titer"))
dataset523_long_RBD_IgA_POS <- dataset523_long_RBD_IgA %>% filter((Ig_Test_Result == "Positive"))
dataset523_long_RBD_IgA_POS <- dataset523_long_RBD_IgA_POS %>% filter((Group_All =="Mild-Moderate") | (Group_All =="Moderate-Severe") | (Group_All =="Positive (Asymptomatic)"))
dataset523_long_RBD_IgA_POS <- mutate(dataset523_long_RBD_IgA_POS, Group_All=recode(Group_All, "Positive (Asymptomatic)"="Asymptomatic"))

FigureS3_A <- ggplot(dataset523_long_RBD_IgA_POS, aes(x = factor(Group_All, level = c('Asymptomatic', 'Mild-Moderate', 'Moderate-Severe')), y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Group_All, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(strip.background =element_rect(color="#9B9C9B", fill="#9B9C9B"), strip.text = element_text(colour = 'black', size=15))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  facet_grid(~ Gender)+ 
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "RBD IgA (AU/mL)")

FigureS3_A

dataset523_long_RBD_IgA_POS_Female <- dataset523_long_RBD_IgA_POS %>% filter((Gender == "Female"))
dataset523_long_RBD_IgA_POS_Male <- dataset523_long_RBD_IgA_POS %>% filter((Gender == "Male"))

res.aov <- aov(value ~ Group_All, data = dataset523_long_RBD_IgA_POS_Male)
summary(res.aov)

res.aov <- aov(value ~ Group_All, data = dataset523_long_RBD_IgA_POS_Female)
summary(res.aov)

table(dataset523_long_RBD_IgA_POS_Female$Group_All)
table(dataset523_long_RBD_IgA_POS_Male$Group_All)

######
#Panel B - Nucleoprotein IgG

dataset523_long_NP_IgG <- dataset523_long %>% filter((test == "Nucleoprotein_IgG_Titer"))
dataset523_long_NP_IgG_POS <- dataset523_long_NP_IgG %>% filter((Ig_Test_Result == "Positive"))
dataset523_long_NP_IgG_POS <- dataset523_long_NP_IgG_POS %>% filter((Group_All =="Mild-Moderate") | (Group_All =="Moderate-Severe") | (Group_All =="Positive (Asymptomatic)"))
dataset523_long_NP_IgG_POS <- mutate(dataset523_long_NP_IgG_POS, Group_All=recode(Group_All, "Positive (Asymptomatic)"="Asymptomatic"))

FigureS3_B <- ggplot(dataset523_long_NP_IgG_POS, aes(x = factor(Group_All, level = c('Asymptomatic', 'Mild-Moderate', 'Moderate-Severe')), y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Group_All, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(strip.background =element_rect(color="#9B9C9B", fill="#9B9C9B"), strip.text = element_text(colour = 'black', size=15))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  facet_grid(~ Gender)+ 
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "NP IgG (AU/mL)")

FigureS3_B

dataset523_long_NP_IgG_POS_Female <- dataset523_long_NP_IgG_POS %>% filter((Gender == "Female"))
dataset523_long_NP_IgG_POS_Male <- dataset523_long_NP_IgG_POS %>% filter((Gender == "Male"))

res.aov <- aov(value ~ Group_All, data = dataset523_long_NP_IgG_POS_Male)
summary(res.aov)

res.aov <- aov(value ~ Group_All, data = dataset523_long_NP_IgG_POS_Female)
summary(res.aov)

table(dataset523_long_NP_IgG_POS_Female$Group_All)
table(dataset523_long_NP_IgG_POS_Male$Group_All)

######
#Panel C - S2 Spike IgA

dataset523_long_S2_IgA <- dataset523_long %>% filter((test == "S2_Spike_IgA_Titer"))
dataset523_long_S2_IgA_POS <- dataset523_long_S2_IgA %>% filter((Ig_Test_Result == "Positive"))
dataset523_long_S2_IgA_POS <- dataset523_long_S2_IgA_POS %>% filter((Group_All =="Mild-Moderate") | (Group_All =="Moderate-Severe") | (Group_All =="Positive (Asymptomatic)"))
dataset523_long_S2_IgA_POS <- mutate(dataset523_long_S2_IgA_POS, Group_All=recode(Group_All, "Positive (Asymptomatic)"="Asymptomatic"))

FigureS3_C <- ggplot(dataset523_long_S2_IgA_POS, aes(x = factor(Group_All, level = c('Asymptomatic', 'Mild-Moderate', 'Moderate-Severe')), y=value))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Group_All, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1.5,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(strip.background =element_rect(color="#9B9C9B", fill="#9B9C9B"), strip.text = element_text(colour = 'black', size=15))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  facet_grid(~ Gender)+ 
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "S2 SP IgA (AU/mL)")

FigureS3_C

dataset523_long_S2_IgA_POS_Female <- dataset523_long_S2_IgA_POS %>% filter((Gender == "Female"))
dataset523_long_S2_IgA_POS_Male <- dataset523_long_S2_IgA_POS %>% filter((Gender == "Male"))

res.aov <- aov(value ~ Group_All, data = dataset523_long_S2_IgA_POS_Male)
summary(res.aov)

res.aov <- aov(value ~ Group_All, data = dataset523_long_S2_IgA_POS_Female)
summary(res.aov)

table(dataset523_long_S2_IgA_POS_Female$Group_All)
table(dataset523_long_S2_IgA_POS_Male$Group_All)

#####
#Final Figure Panel 
plot_grid(FigureS3_A, FigureS3_B, FigureS3_C, labels = "auto", ncol=1, label_size = 25) 


######
#Export CSV with data used 
write.csv(dataset523_long,file="Supplementary_Figure_3_Source_Code.csv")

