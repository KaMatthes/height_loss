
dat_dis <- dat %>% 
  mutate(sex=as.factor(sex),
         sex=recode(sex, 
                    "1" ="male",
                    "2" ="female"),
         ) %>%
  select(sex, height36, height43, height53, height69) %>%
  filter(complete.cases(.)) %>%
  gather(., age, height, height36:height69) %>%
  mutate(age = recode(age, 
                      "height36" = "Age 36",
                      "height43" = "Age 43",
                      "height53" = "Age 53",
                      "height69" = "Age 69"
                      ))
           
Figure3_supp <- ggplot() +
  geom_density(data=dat_dis,aes(x=height,col=age),lwd=2) +
  facet_wrap(~sex, nrow=2) +
  xlab("Height in cm")+
  ylab("Density")+
  xlim(c(140,200))+
  scale_color_manual("",
                     values = c("grey40",cbp1[3],cbp1[2],cbp1[4]))+
  theme_bw()+
  theme(strip.text = element_text(size=20),
        axis.text=element_text(color="black",size=20),
        axis.title=element_text(size=20),
        panel.grid.major.x = element_blank(),
        legend.text=element_text(size=20),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"))

save_plot("Analysis/output/Figure3_supp.pdf", Figure3_supp,base_height=10,base_width=16)

