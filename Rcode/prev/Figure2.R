# health score sum

Mod_un_male_health <- data.frame(function_health_outcome(varOutcome="health.score.sum",adjusted="no",sex="male")$coefficients) %>%
    mutate(Est = round(exp(Estimate),2),
           CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
           CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
           Fac = row.names(.)) %>%
    filter(Fac =="heightdiff") %>%
    dplyr::select(Fac, Est, CIl,CIu) %>%
    mutate(Var="health score",
           Sex="male",
           Model="unadjusted")
  
Mod_ad_male_health <- data.frame(function_health_outcome(varOutcome="health.score.sum",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="health score",
         Sex="male",
         Model="adjusted")

Mod_un_female_health <- data.frame(function_health_outcome(varOutcome="health.score.sum",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="health score",
         Sex="female",
         Model="unadjusted")


Mod_ad_female_health <- data.frame(function_health_outcome(varOutcome="health.score.sum",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="health score",
         Sex="female",
         Model="adjusted")
  

# chronicDiseaseScore

Mod_un_male_chronic <- data.frame(function_health_outcome(varOutcome="chronicDiseaseScore",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="chronic disease",
         Sex="male",
         Model="unadjusted")

Mod_ad_male_chronic <- data.frame(function_health_outcome(varOutcome="chronicDiseaseScore",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="chronic disease",
         Sex="male",
         Model="adjusted")

Mod_un_female_chronic <- data.frame(function_health_outcome(varOutcome="chronicDiseaseScore",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="chronic disease",
         Sex="female",
         Model="unadjusted")


Mod_ad_female_chronic <- data.frame(function_health_outcome(varOutcome="chronicDiseaseScore",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="chronic disease",
         Sex="female",
         Model="adjusted")


# Osteoarthritis

Mod_un_male_osteo <- data.frame(function_health_outcome(varOutcome="osteo",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="osteoarthritis",
         Sex="male",
         Model="unadjusted")

Mod_ad_male_osteo <- data.frame(function_health_outcome(varOutcome="osteo",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="osteoarthritis",
         Sex="male",
         Model="adjusted")

Mod_un_female_osteo <- data.frame(function_health_outcome(varOutcome="osteo",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="osteoarthritis",
         Sex="female",
         Model="unadjusted")


Mod_ad_female_osteo <- data.frame(function_health_outcome(varOutcome="osteo",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="osteoarthritis",
         Sex="female",
         Model="adjusted")

# Ability to walk restricted due to pain

Mod_un_male_pain <- data.frame(function_health_outcome(varOutcome="pain.walking",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="pain walk",
         Sex="male",
         Model="unadjusted")

Mod_ad_male_pain <- data.frame(function_health_outcome(varOutcome="pain.walking",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="pain walk",
         Sex="male",
         Model="adjusted")

Mod_un_female_pain <- data.frame(function_health_outcome(varOutcome="pain.walking",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="pain walk",
         Sex="female",
         Model="unadjusted")


Mod_ad_female_pain <- data.frame(function_health_outcome(varOutcome="pain.walking",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(exp(Estimate),2),
         CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
         CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
         Fac = row.names(.)) %>%
  filter(Fac =="heightdiff") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="pain walk",
         Sex="female",
         Model="adjusted")

# plot

data_reg <- rbind(Mod_un_male_health, Mod_ad_male_health, Mod_un_female_health, Mod_ad_female_health,
                  Mod_un_male_chronic, Mod_ad_male_chronic, Mod_un_female_chronic, Mod_ad_female_chronic,
                  Mod_un_male_osteo, Mod_ad_male_osteo, Mod_un_female_osteo, Mod_ad_female_osteo,
                  Mod_un_male_pain, Mod_ad_male_pain, Mod_un_female_pain, Mod_ad_female_pain) %>%
  mutate(Model=factor(Model, levels=c("unadjusted", "adjusted")))


Figure2 <- ggplot( data_reg, aes(x=forcats::fct_rev(Var),y=Est),position=pd) + 
  geom_hline(yintercept=1, colour="grey", lwd=lwdline) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Sex),lwd=lwd_size,position=pd,fatten=fatten_size)+
  facet_grid(~Model) +
  # ylim(c(0,2.5))+
  labs(x="",y="OR and 95% CI") +
  guides(color = guide_legend(override.aes = list(size = 1.5)))+
  # ggtitle("OR")+
  scale_color_manual(" ",
                     breaks=c("female","male"),
                     # labels=c("Male (Ref)","Female"),
                     values = c(mypalette[3],mypalette[2]))+
  theme_bw()+
  theme(aspect.ratio=1,
        strip.text = element_text(color="black",size= strip_text),
        axis.text=element_text(color="black",size= size_axis),
        axis.title=element_text(size= size_axis_title),
        plot.title = element_text(size=size_plot_title),
        legend.text=element_text(size=size_legend_text),
        legend.title= element_blank(),
        legend.position = "bottom") +
  coord_flip() 

cowplot::save_plot("Analysis/output/Figure2.pdf", Figure2,base_height=8,base_width=16) 

