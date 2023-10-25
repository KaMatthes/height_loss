dat_69 <- dat %>%
  filter(!is.na(height69)) %>%
  mutate(sex=as.factor(sex),
         sex=recode(sex, 
                    "1" ="male",
                    "2" ="female"))
         # points_exc = ifelse(heightdiff > 15 | heightdiff < -5, 1,0),
         # points_0 = ifelse((heightdiff < 0 ) , 1, 0),
         # points_col= case_when(
         #   points_exc== 0 &  points_0== 0 ~ "0",
         #   points_exc == 0 &  points_0 == 1 ~ "1" ,
         #   points_exc == 1 ~  "2"))

  
  t.test(dat_69[dat_69$sex=="male",]$heightdiff,dat_69[dat_69$sex=="female",]$heightdiff)



Suppl_dis <- ggplot(data=dat_69 ,aes(x=sex,y=heightdiff)) +
  geom_violin(aes(x=sex,y=heightdiff)) +
  # geom_boxplot(data=datared, aes(factor(Grippe), weight),width=.1) +
  geom_quasirandom(aes(x=sex,y=heightdiff), col="grey", width = 0.4) +
  stat_summary(fun = "median",
               geom = "crossbar", 
               width = 0.5,
               colour = "black") +
  annotate("text", x=1, y=-3, label= "mean difference = 2.03 cm", size=5) +
  # annotate("text", x=0.66, y=33, label= "preterm = 29.73 %", size=5) + 
  annotate("text", x=2,y=-3, label= "mean difference =  2.45 cm", size=5) + 
  annotate("text", x=1.5,y=-5, label= "p-value < 0.0000", size=5) + 
  # annotate("text", x=1.70, y=33, label=  "preterm = 14.85 %", size=5) + 
  # scale_color_manual("",
  #                    breaks=c("0","1", "2"),
  #                    labels=c("original data", "set to zero","exclude"),
  #                    values=c("grey", "grey40", "red")) +
  # scale_size_manual(
  #                    breaks=c("0","1", "2"),
  #                    values=c(1,2,2.5))+
  guides(size = "none") +
  ylim(-5,30)+
  xlab("")+
  ylab("Heightdifference in cm")+
  theme_bw()+
  theme(aspect.ratio=1,
        axis.text=element_text(color="black",size=15),
        axis.title=element_text(size=15),
        panel.grid.major.x = element_blank(),
        legend.text=element_text(size=15),
        legend.position = c(.2, .8),
        legend.key.size = unit(0.8, "cm"))


cowplot::save_plot("Analysis/output/Suppl_dis.pdf", Suppl_dis,base_height=10,base_width=10)

