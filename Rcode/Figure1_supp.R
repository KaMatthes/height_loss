dat_all_mean <- dat %>%
  group_by(sex) %>%
  summarise(height02=mean(height02, na.rm=TRUE),
            height04=mean(height04, na.rm=TRUE),
            height06=mean(height06, na.rm=TRUE),
            height07=mean(height07, na.rm=TRUE),
            height11=mean(height11, na.rm=TRUE),
            height15=mean(height15, na.rm=TRUE),
            height36=mean(height36, na.rm=TRUE),
            height43=mean(height43, na.rm=TRUE),
            height53=mean(height53, na.rm=TRUE),
            height69=mean(height69, na.rm=TRUE)) %>%
  ungroup() %>%
  gather(., Age, Mean_height,height02:height69) %>%
  mutate(Var="Full sample")
  

dat_all_sd <- dat %>%
  group_by(sex) %>%
  summarise(height02=sd(height02, na.rm=TRUE),
            height04=sd(height04, na.rm=TRUE),
            height06=sd(height06, na.rm=TRUE),
            height07=sd(height07, na.rm=TRUE),
            height11=sd(height11, na.rm=TRUE),
            height15=sd(height15, na.rm=TRUE),
            height36=sd(height36, na.rm=TRUE),
            height43=sd(height43, na.rm=TRUE),
            height53=sd(height53, na.rm=TRUE),
            height69=sd(height69, na.rm=TRUE)) %>%
  ungroup() %>%
  gather(., Age, Sd_height,height02:height69) %>%
  mutate(Var="Full sample")

dat_all_n <- dat %>%
  group_by(sex) %>%
  summarise(height02=sum(!is.na(height02)),
            height04=sum(!is.na(height04)),
            height06=sum(!is.na(height06)),
            height07=sum(!is.na(height07)),
            height11=sum(!is.na(height11)),
            height15=sum(!is.na(height15)),
            height36=sum(!is.na(height36)),
            height43=sum(!is.na(height43)),
            height53=sum(!is.na(height53)),
            height69=sum(!is.na(height69))) %>%
  ungroup() %>%
  gather(., Age, n_height,height02:height69) %>%
  mutate(Var="Full sample")

dat_full_sample <- dat_all_mean %>%
  full_join(dat_all_sd) %>%
  full_join(dat_all_n)

dat_69_mean <- dat %>%
  filter(!is.na(height69))%>%
  group_by(sex) %>%
  summarise(height02=mean(height02, na.rm=TRUE),
            height04=mean(height04, na.rm=TRUE),
            height06=mean(height06, na.rm=TRUE),
            height07=mean(height07, na.rm=TRUE),
            height11=mean(height11, na.rm=TRUE),
            height15=mean(height15, na.rm=TRUE),
            height36=mean(height36, na.rm=TRUE),
            height43=mean(height43, na.rm=TRUE),
            height53=mean(height53, na.rm=TRUE),
            height69=mean(height69, na.rm=TRUE)) %>%
  ungroup() %>%
  gather(., Age, Mean_height,height02:height69) %>%
  mutate(Var="Nurse visit 2015")



dat_69_sd <- dat %>%
  filter(!is.na(height69)) %>%
  group_by(sex) %>%
  summarise(height02=sd(height02, na.rm=TRUE),
            height04=sd(height04, na.rm=TRUE),
            height06=sd(height06, na.rm=TRUE),
            height07=sd(height07, na.rm=TRUE),
            height11=sd(height11, na.rm=TRUE),
            height15=sd(height15, na.rm=TRUE),
            height36=sd(height36, na.rm=TRUE),
            height43=sd(height43, na.rm=TRUE),
            height53=sd(height53, na.rm=TRUE),
            height69=sd(height69, na.rm=TRUE)) %>%
  ungroup() %>%
  gather(., Age, Sd_height,height02:height69) %>%
  mutate(Var="Nurse visit 2015")


dat_69_n <- dat %>%
  filter(!is.na(height69))%>%
  group_by(sex) %>%
  summarise(height02=sum(!is.na(height02)),
            height04=sum(!is.na(height04)),
            height06=sum(!is.na(height06)),
            height07=sum(!is.na(height07)),
            height11=sum(!is.na(height11)),
            height15=sum(!is.na(height15)),
            height36=sum(!is.na(height36)),
            height43=sum(!is.na(height43)),
            height53=sum(!is.na(height53)),
            height69=sum(!is.na(height69))) %>%
  ungroup() %>%
  gather(., Age, n_height,height02:height69) %>%
  mutate(Var="Nurse visit 2015")

dat_nurse <- dat_69_mean %>%
  full_join(dat_69_sd)%>%
  full_join(dat_69_n)


dat_full_box <- dat %>%
  mutate(Var="Full sample")

dat_nurse_box <- dat %>%
  filter(!is.na(height69)) %>%
  mutate(Var="Nurse visit 2015")




dat_all <- rbind(dat_full_sample, dat_nurse) %>%
  mutate(se = Sd_height/sqrt(n_height),
         CIl = Mean_height-1.96*(Sd_height/sqrt(n_height)),
         CIu = Mean_height+1.96*(Sd_height/sqrt(n_height)),
         Age=factor(Age, levels=c("height69", "height53","height43","height36",
                                    "height15", "height11", "height07","height06",
                                    "height04", "height02")))

  


Figure1_Supp <- ggplot( dat_all, aes(x=forcats::fct_rev(Age),y=Mean_height),position=pd) + 
  geom_hline(yintercept=0, colour="grey", lwd=lwdline) + 
  geom_pointrange(aes(ymin=CIl, ymax=CIu,col=Var),lwd=lwd_size,position=pd,fatten=fatten_size)+
  facet_grid(~sex) +
  # ylim(c(0,2.5))+
  labs(x="",y="Coefficient (z-values)") +
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

dat_box <- rbind(dat_full_box, dat_nurse_box) %>%
  select(Var,sex,height02, height04, height06, height07,
         height11, height15, height36, height43,
         height53, height69) %>%
  gather(., Age,Height, height02:height69) %>%
  mutate(sex=as.factor(sex),
         sex=recode(sex, 
                    "1" ="male",
                    "2" ="female"),
         Age=recode(Age,
                    "height02" = "2",
                    "height04" = "4",
                    "height06" = "6",
                    "height07" = "7",
                    "height11" = "11",
                    "height15" = "15",
                    "height36" = "36",
                    "height43" = "43",
                    "height53" = "53",
                    "height69" = "69"),
         Age=factor(Age, levels=c("2", "4","6","7",
                                  "11", "15", "36","43",
                                  "53", "69")))


Figure1_supp <- ggplot(data=dat_box)+
  geom_violin(aes(x=Age, y=Height,fill=Var),position=pd_v)+
  # geom_quasirandom(aes(x=Age,y=Height, col=Var),size=1) +
  geom_boxplot(aes(x=Age,y=Height, fill=Var),width=0.3,position=pd_b)+
  facet_grid(~sex) +
  ylab("Height in cm")+
  xlab("Age in years")+
  scale_fill_manual(" ",
                     breaks=c("Full sample","Nurse visit 2015"),
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
        legend.position = "bottom") 

cowplot::save_plot("Analysis/output/Figure1_supp.pdf", Figure1_supp,base_height=8,base_width=15) 

