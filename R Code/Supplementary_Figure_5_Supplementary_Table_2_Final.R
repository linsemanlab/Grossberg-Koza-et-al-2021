#Supplementary Table 2 and Supplementary Figure 5 

######
#Libraries used 
install.packages("mgcv")
library(mgcv)

######
#General Additive Models and General Linear Models to fit data shown in Figure 2 

#Fit general linear models 
mod_lm_1 <- lm(RBD_IgM_Titer ~ S_Onset_To_T1_Days, data = POS)
summary(mod_lm_1)

mod_lm_2 <- lm(S1_Spike_IgM_Titer ~ S_Onset_To_T1_Days, data = POS)
summary(mod_lm_2)

mod_lm_3 <- lm(IgMMean ~ S_Onset_To_T1_Days, data = POS)
summary(mod_lm_3)

mod_lm_4 <- lm(IgGMean ~ S_Onset_To_T1_Days, data = POS)
summary(mod_lm_4)

# Fit General Additive Models (GAMS)
gam_mod_1 <- gam(RBD_IgM_Titer ~ s(S_Onset_To_T1_Days), data = POS, method = "REML")
gam_mod_2 <- gam(S1_Spike_IgM_Titer ~ s(S_Onset_To_T1_Days), data = POS, method = "REML")
gam_mod_3 <- gam(IgMMean ~ s(S_Onset_To_T1_Days), data = POS, method = "REML")
gam_mod_4 <- gam(IgGMean ~ s(S_Onset_To_T1_Days), data = POS, method = "REML")

#Nine basis functions with coefficients 
coef(gam_mod_1)
coef(gam_mod_2)
coef(gam_mod_3)
coef(gam_mod_4)

#Summary of GAMS 
summary(gam_mod_1)
summary(gam_mod_2)
summary(gam_mod_3)
summary(gam_mod_4)

######
#Supplementary Table 2 - Final 
#Analysis of variance table comparing general additive models (GAM) and general linear models (GLM) for IgG average, IgM average, and IgM titers against SARS-CoV-2 spike 1 glycoprotein (S1 SP) and receptor binding domain (RBD) as a function of days between symptom onset and initial test.

anova(mod_lm_1, gam_mod_1, test="Chisq")
anova(mod_lm_2, gam_mod_2, test="Chisq")
anova(mod_lm_3, gam_mod_3, test="Chisq")
anova(mod_lm_4, gam_mod_4, test="Chisq")

######
### Supplementary Figure 5 - Final 
#Non-linear trend lines generated using general additive models (GAM) for IgG and IgM antibody titers against SARS-CoV-2 antigens as a function of days between symptom onset and serological test.

# Plot the GAM models 
p1_RBD_IgM <- plot(gam_mod_1, residuals = TRUE, pch = 1)
p2_S1_Spike_IgM <- plot(gam_mod_2, residuals = TRUE, pch = 1)
p3_IgM_Mean <- plot(gam_mod_3, residuals = TRUE, pch = 1)
p4_IgG_Mean <- plot(gam_mod_4, residuals = TRUE, pch = 1)

######
#Export CSV with data used 
write.csv(POS,file="Supplementary_Figure_5_Source_Data.csv")

