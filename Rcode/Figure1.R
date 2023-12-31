# Age 2

Mod_un_male2 <- data.frame(function_heightgain_loss_rel(varAge="height02_z",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height02_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 2",
         Sex="male",
         Model="unadjusted")

Mod_ad_male2 <- data.frame(function_heightgain_loss_rel(varAge="height02_z",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height02_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 2",
         Sex="male",
         Model="adjusted")

Mod_un_female2 <- data.frame(function_heightgain_loss_rel(varAge="height02_z",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height02_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 2",
         Sex="female",
         Model="unadjusted")

Mod_ad_female2 <- data.frame(function_heightgain_loss_rel(varAge="height02_z",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height02_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 2",
         Sex="female",
         Model="adjusted")

# Age 7

Mod_un_male7 <- data.frame(function_heightgain_loss_rel(varAge="height07_z",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height07_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 7",
         Sex="male",
         Model="unadjusted")

Mod_ad_male7 <- data.frame(function_heightgain_loss_rel(varAge="height07_z",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height07_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 7",
         Sex="male",
         Model="adjusted")

Mod_un_female7 <- data.frame(function_heightgain_loss_rel(varAge="height07_z",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height07_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 7",
         Sex="female",
         Model="unadjusted")

Mod_ad_female7 <- data.frame(function_heightgain_loss_rel(varAge="height07_z",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height07_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 7",
         Sex="female",
         Model="adjusted")

# Age 11

Mod_un_male11 <- data.frame(function_heightgain_loss_rel(varAge="height11_z",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height11_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 11",
         Sex="male",
         Model="unadjusted")

Mod_ad_male11 <- data.frame(function_heightgain_loss_rel(varAge="height11_z",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height11_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 11",
         Sex="male",
         Model="adjusted")

Mod_un_female11 <- data.frame(function_heightgain_loss_rel(varAge="height11_z",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height11_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 11",
         Sex="female",
         Model="unadjusted")

Mod_ad_female11 <- data.frame(function_heightgain_loss_rel(varAge="height11_z",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height11_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 11",
         Sex="female",
         Model="adjusted")


# Age 15

Mod_un_male15 <- data.frame(function_heightgain_loss_rel(varAge="height15_z",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height15_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 15",
         Sex="male",
         Model="unadjusted")

Mod_ad_male15 <- data.frame(function_heightgain_loss_rel(varAge="height15_z",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height15_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 15",
         Sex="male",
         Model="adjusted")

Mod_un_female15 <- data.frame(function_heightgain_loss_rel(varAge="height15_z",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height15_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 15",
         Sex="female",
         Model="unadjusted")

Mod_ad_female15 <- data.frame(function_heightgain_loss_rel(varAge="height15_z",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height15_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 15",
         Sex="female",
         Model="adjusted")

# Age 36

Mod_un_male36 <- data.frame(function_heightgain_loss_rel(varAge="height36_z",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height36_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 36",
         Sex="male",
         Model="unadjusted")

Mod_ad_male36 <- data.frame(function_heightgain_loss_rel(varAge="height36_z",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height36_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 36",
         Sex="male",
         Model="adjusted")

Mod_un_female36 <- data.frame(function_heightgain_loss_rel(varAge="height36_z",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height36_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 36",
         Sex="female",
         Model="unadjusted")

Mod_ad_female36 <- data.frame(function_heightgain_loss_rel(varAge="height36_z",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="height36_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height at age 36",
         Sex="female",
         Model="adjusted")

# Delta between age 2 and age 7

Mod_un_male2_7 <- data.frame(function_heightgain_loss_rel(varAge="heightgain2_7_z",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain2_7_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 2 and 7",
         Sex="male",
         Model="unadjusted")

Mod_ad_male2_7 <- data.frame(function_heightgain_loss_rel(varAge="heightgain2_7_z",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain2_7_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 2 and 7",
         Sex="male",
         Model="adjusted")

Mod_un_female2_7 <- data.frame(function_heightgain_loss_rel(varAge="heightgain2_7_z",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain2_7_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 2 and 7",
         Sex="female",
         Model="unadjusted")

Mod_ad_female2_7 <- data.frame(function_heightgain_loss_rel(varAge="heightgain2_7_z",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain2_7_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 2 and 7",
         Sex="female",
         Model="adjusted")

# Delta between age 7 and age 11

Mod_un_male7_11 <- data.frame(function_heightgain_loss_rel(varAge="heightgain7_11_z",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain7_11_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 7 and 11",
         Sex="male",
         Model="unadjusted")

Mod_ad_male7_11 <- data.frame(function_heightgain_loss_rel(varAge="heightgain7_11_z",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain7_11_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 7 and 11",
         Sex="male",
         Model="adjusted")

Mod_un_female7_11 <- data.frame(function_heightgain_loss_rel(varAge="heightgain7_11_z",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain7_11_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 7 and 11",
         Sex="female",
         Model="unadjusted")

Mod_ad_female7_11 <- data.frame(function_heightgain_loss_rel(varAge="heightgain7_11_z",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain7_11_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 7 and 11",
         Sex="female",
         Model="adjusted")


# Delta between age 11 and age 15

Mod_un_male11_15<- data.frame(function_heightgain_loss_rel(varAge="heightgain11_15_z",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain11_15_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 11 and 15",
         Sex="male",
         Model="unadjusted")

Mod_ad_male11_15<- data.frame(function_heightgain_loss_rel(varAge="heightgain11_15_z",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain11_15_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 11 and 15",
         Sex="male",
         Model="adjusted")

Mod_un_female11_15<- data.frame(function_heightgain_loss_rel(varAge="heightgain11_15_z",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain11_15_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 11 and 15",
         Sex="female",
         Model="unadjusted")

Mod_ad_female11_15<- data.frame(function_heightgain_loss_rel(varAge="heightgain11_15_z",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="heightgain11_15_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="Height gain between 11 and 15",
         Sex="female",
         Model="adjusted")


# SITAR Height

Mod_un_male_s_height <- data.frame(function_heightgain_loss_rel(varAge="SITAR_size_height_z",adjusted="no",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="SITAR_size_height_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="SITAR height size",
         Sex="male",
         Model="unadjusted")

Mod_ad_male_s_height <- data.frame(function_heightgain_loss_rel(varAge="SITAR_size_height_z",adjusted="yes",sex="male")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="SITAR_size_height_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="SITAR height size",
         Sex="male",
         Model="adjusted")

Mod_un_female_s_height <- data.frame(function_heightgain_loss_rel(varAge="SITAR_size_height_z",adjusted="no",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="SITAR_size_height_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="SITAR height size",
         Sex="female",
         Model="unadjusted")

Mod_ad_female_s_height <- data.frame(function_heightgain_loss_rel(varAge="SITAR_size_height_z",adjusted="yes",sex="female")$coefficients) %>%
  mutate(Est = round(Estimate,3),
         CIl = round(Estimate - 1.96*`Std..Error`,3),
         CIu = round(Estimate + 1.96*`Std..Error`,3),
         Fac = row.names(.)) %>%
  filter(Fac =="SITAR_size_height_z") %>%
  dplyr::select(Fac, Est, CIl,CIu) %>%
  mutate(Var="SITAR height size",
         Sex="female",
         Model="adjusted")


### plot

data_reg <- rbind(Mod_un_male2,Mod_ad_male2,Mod_un_female2, Mod_ad_female2,
                  Mod_un_male7,Mod_ad_male7,Mod_un_female7, Mod_ad_female7,
                  Mod_un_male11,Mod_ad_male11,Mod_un_female11, Mod_ad_female11,
                  Mod_un_male15,Mod_ad_male15,Mod_un_female15, Mod_ad_female15,
                  Mod_un_male36,Mod_ad_male36,Mod_un_female36, Mod_ad_female36,
                  Mod_un_male2_7,Mod_ad_male2_7,Mod_un_female2_7, Mod_ad_female2_7,
                  Mod_un_male7_11,Mod_ad_male7_11,Mod_un_female7_11, Mod_ad_female7_11,
                  Mod_un_male11_15,Mod_ad_male11_15,Mod_un_female11_15, Mod_ad_female11_15,
                  Mod_un_male_s_height,Mod_ad_male_s_height,Mod_un_female_s_height, Mod_ad_female_s_height) %>%
  mutate(Var=factor(Var,levels=c("Height at age 2","Height at age 7","Height at age 11",
                            "Height at age 15" ,"Height at age 36","Height gain between 2 and 7",
                            "Height gain between 7 and 11", "Height gain between 11 and 15","SITAR height size")),
         Model=factor(Model, levels=c("unadjusted", "adjusted")))

Figure1_rel <- ggplot( data_reg, aes(x=forcats::fct_rev(Var),y=Est),position=pd) + 
  geom_hline(yintercept=0, colour="grey", lwd=lwdline) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Sex),lwd=lwd_size,position=pd,fatten=fatten_size)+
  facet_grid(~Model) +
  # ylim(c(0,2.5))+
  labs(x="",y="Coefficient (z-values)") +
  guides(color = guide_legend(override.aes = list(size = 1.5)))+
  # ggtitle("OR")+
  scale_color_manual(" ",
                     breaks=c("male","female"),
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

cowplot::save_plot("output/Figure1_rel.pdf", Figure1_rel,base_height=8,base_width=18) 


results_linear <- data_reg %>%
  mutate( Est=round(Est,2),
          CIl = round(CIl, 2),
          CIu = round(CIu, 2),
          `Coeff 95% CI` = paste0(Est," (", CIl,"-",CIu,")"))%>%
  select(Var, Sex, Model,`Coeff 95% CI`)


write.table(results_linear,file="output/results_linear.csv",row.names=FALSE, sep=";")