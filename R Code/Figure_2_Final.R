#Figure 2 - Final 
#IgG and IgM antibody titers plotted against days between symptom onset and serological test for COVID-19-positive participants. 

######
#Libraries used 
library(ggplot2)
library(cowplot) 

######
#Not Sig 
Figure2A <- ggplot(POS, aes(x=S_Onset_To_T1_Days, y=IgGMean, color=Ig_Test_Result))+
  geom_point(size=2)+
  scale_color_grey()+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  labs(x = "Days Between Symptom Onset and Test", y = "Averege IgG (AU/mL)")+ 
  coord_cartesian(ylim=c(0, 2.1)) + scale_y_continuous(breaks=seq(0, 2.1, 0.5))+ 
  annotate("rect", xmin=0, xmax=60, ymin=0, ymax=Inf, alpha=0.1, fill="#A29A9B")

Figure2A

#Sig 
Figure2B <- ggplot(POS, aes(x=S_Onset_To_T1_Days, y=IgMMean, color=Ig_Test_Result))+ 
  geom_point(size=2)+
  theme_minimal()+
  scale_color_grey()+ 
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  labs(x = "Days Between Symptom Onset and Test", y = "Average IgM (AU/mL)")+ 
  coord_cartesian(ylim=c(0, 2.1)) + scale_y_continuous(breaks=seq(0, 2.1, 0.5))+ 
  annotate("rect", xmin=0, xmax=30, ymin=0, ymax=Inf, alpha=0.1, fill="#A29A9B")

Figure2B

#Sig 
Figure2C <- ggplot(POS, aes(x=S_Onset_To_T1_Days, y=S1_Spike_IgM_Titer, color=Ig_Test_Result))+
  geom_point(size=2)+
  scale_color_grey()+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  labs(x = "Days Between Symptom Onset and Test", y = "S1 SP IgM (AU/mL)")+
  coord_cartesian(ylim=c(0, 2.1)) + scale_y_continuous(breaks=seq(0, 2.1, 0.5))+ 
  annotate("rect", xmin=0, xmax=30, ymin=0, ymax=Inf, alpha=0.1, fill="#A29A9B")

Figure2C

#Sig 
Figure2D <- ggplot(POS, aes(x=S_Onset_To_T1_Days, y=RBD_IgM_Titer, color=Ig_Test_Result))+
  geom_point(size=2)+
  scale_color_grey()+ 
  theme_minimal()+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))+
  theme(legend.position="none",legend.title=element_blank(),axis.text = element_text(size = 20),axis.title = element_text(size = 20),legend.text = element_text(size = 20))+
  geom_hline(yintercept = 1, linetype="dashed", size=1)+
  labs(x = "Days Between Symptom Onset and Test", y = "RBD IgM (AU/mL)")+ 
  coord_cartesian(ylim=c(0, 2.1)) + scale_y_continuous(breaks=seq(0, 2.1, 0.5))+ 
  annotate("rect", xmin=0, xmax=30, ymin=0, ymax=Inf, alpha=0.1, fill="#A29A9B")

Figure2D

######
#Final Figure Panel 
plot_grid(Figure2A, Figure2B, Figure2C, Figure2D, labels = "auto", label_size = 25) 

######
#Export CSV with data used 
write.csv(POS,file="Figure_2_Source_Data.csv")
