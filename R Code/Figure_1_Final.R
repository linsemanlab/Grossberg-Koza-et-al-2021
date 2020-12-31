#Figure 1 - Final Version 
#IgA antibody titers against receptor binding domain (RBD) and spike 2 glycoprotein (S2 SP) and IgG antibody titers against nucleoprotein plotted against symptom severity in COVID-19-positive participants. 

######
#Libraries used 
library(ggplot2)
library(tidyverse)
library(ggsignif)
library(cowplot)

######
#RBD IgA 

POS_1 <- POS %>% filter((Group_All =="Mild-Moderate") | (Group_All =="Moderate-Severe") | (Group_All =="Positive (Asymptomatic)"))#Filter to remove NA's 
POS_1 <- mutate(POS_1, Group_All=recode(Group_All, "Positive (Asymptomatic)"="Asymptomatic"))#Rename group

#Sig 
Figure1A <- ggplot(POS_1, aes(x = factor(Group_All, level = c('Asymptomatic', 'Mild-Moderate', 'Moderate-Severe')), y=RBD_IgA_Titer))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Group_All, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(3), xmin = c(1), 
              xmax = c(3), annotation = c("*"),
              tip_length = 0.02, textsize = 12, size=2)+
  geom_signif(y_position = c(2.5), xmin = c(2), 
              xmax = c(3), annotation = c("*"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3.25)) + scale_y_continuous(breaks=seq(0, 3.25, 0.5))+ 
  labs(x = "Group", y = "RBD IgA (AU/mL)")

Figure1A

res.aov <- aov(RBD_IgA_Titer ~ Group_All, data = POS)
summary(res.aov)
TukeyHSD(res.aov)

######
#IgG Nucleoprotien

POS_2 <- POS %>% filter((Group_All =="Moderate-Severe") | (Group_All =="Positive (Asymptomatic)"))
POS_2 <- mutate(POS_2, Group_All=recode(Group_All, "Positive (Asymptomatic)"="Asymptomatic"))

Figure1B <- ggplot(POS_1, aes(x = factor(Group_All, level = c('Asymptomatic', 'Mild-Moderate', 'Moderate-Severe')), y=Nucleoprotein_IgG_Titer))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Group_All, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.75), xmin = c(1), 
              xmax = c(3), annotation = c("*"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 3)) + scale_y_continuous(breaks=seq(0, 3, 0.5))+ 
  labs(x = "Group", y = "NP IgG (AU/mL)")

Figure1B

#Sig
res <- t.test(Nucleoprotein_IgG_Titer ~ Group_All, data = POS_2, var.equal = TRUE)
res

######
#IgA S2 

Figure1C <- ggplot(POS_1, aes(x = factor(Group_All, level = c('Asymptomatic', 'Mild-Moderate', 'Moderate-Severe')), y=S2_Spike_IgA_Titer))+
  geom_boxplot(width = 0.5,position = position_dodge(0.8))+ 
  geom_dotplot(aes(fill = Group_All, color = "black"), stroke=1, binaxis='y', stackdir='center',dotsize=1,position = "dodge")+ 
  scale_color_grey()+ 
  scale_fill_manual(values=c("#9B9C9B", "#9B9C9B","#9B9C9B", "#9B9C9B"))+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  geom_signif(y_position = c(2.25), xmin = c(1), 
              xmax = c(3), annotation = c("*"),
              tip_length = 0.02, textsize = 12, size=2)+
  coord_cartesian(ylim=c(0, 2.5)) + scale_y_continuous(breaks=seq(0, 2.5, 0.5))+ 
  labs(x = "Group", y = "S2 SP IgA (AU/mL)")

Figure1C

#Sig
res <- t.test(S2_Spike_IgA_Titer ~ Group_All, data = POS_2, var.equal = TRUE)
res

######
#Final Figure Panel 
plot_grid(Figure1A, Figure1B, Figure1C, labels = "auto", ncol=1, label_size = 25) 

######
#Export CSV with data used 
write.csv(POS_1,file="Figure_1_Source_Data.csv")

