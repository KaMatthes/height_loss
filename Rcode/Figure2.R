
function_health_outcome_gam_all <- function (Sex, var_nr=1) {

  dat_gam1 <- dat %>%
    select(health.score.sum,sex,heightdiff_rel) %>%
    filter(complete.cases(.)) %>%
    filter(sex==Sex) %>%
    filter(!heightdiff_rel > 5.5)
  
  dat_gam2 <- dat %>%
    select(chronicDiseaseScore,sex,heightdiff_rel) %>%
    filter(complete.cases(.)) %>%
    filter(sex==Sex) %>%
    filter(!heightdiff_rel > 5.5)
  
  dat_gam3 <- dat %>%
    select(osteo,sex,heightdiff_rel) %>%
    filter(complete.cases(.)) %>%
    filter(sex==Sex) %>%
    filter(!heightdiff_rel > 5.5)
  
  dat_gam4 <- dat %>%
    select(pain.walking,sex,heightdiff_rel) %>%
    filter(complete.cases(.)) %>%
    filter(sex==Sex) %>%
    filter(!heightdiff_rel > 5.5)
  

  GAM_plot_function_binary <- function(model) {
    plot_mod <- plot(model, select=var_nr)
    plot_x <-  plot_mod[[var_nr]]$x
    plot_fit <-  plot_mod[[var_nr]]$fit + coef(model)[1]
    plot_CIl<- plot_fit-(1.96*( plot_mod[[var_nr]]$se))
    plot_CIu<- plot_fit+(1.96*( plot_mod[[var_nr]]$se))
    plot_data <-  data.frame(x=plot_x, fit=plogis(plot_fit),CIl=plogis(plot_CIl),CIu=plogis(plot_CIu)) %>%
      gather(., variable, value, fit:CIu)
  }
  
  
    mod_1 <- gam(health.score.sum~s(heightdiff_rel),data=dat_gam1, family = binomial)
    plot_data_1 <-   GAM_plot_function_binary(mod_1) %>%
      mutate(Model= "General health status")
    
    
    mod_2 <- gam(chronicDiseaseScore~s(heightdiff_rel),data=dat_gam2, family = binomial)
    plot_data_2 <-   GAM_plot_function_binary(mod_2) %>%
      mutate(Model= "Chronic diseases")
    
    
    mod_3 <- gam(osteo~s(heightdiff_rel),data=dat_gam3, family = binomial)
    plot_data_3 <-   GAM_plot_function_binary(mod_3) %>%
      mutate(Model= "Osteoarthritis")
    
    mod_4 <- gam(pain.walking~s(heightdiff_rel),data=dat_gam4, family = binomial)
    plot_data_4 <-   GAM_plot_function_binary(mod_4) %>%
      mutate(Model= "Pain while walking")
    
    
    plot_data <- rbind(plot_data_1,plot_data_2,plot_data_3,plot_data_4) %>%
      mutate(sex=Sex)
    
    return(plot_data)
}

data_male <- function_health_outcome_gam_all(Sex=1)
data_female <- function_health_outcome_gam_all(Sex=2)
plot_data <- rbind(data_male, data_female) %>%
  mutate(sex=recode(sex, "1"="Male",
                    "2" ="Female"))
    
  
Figure2 <- ggplot()+
      geom_line(data=plot_data, aes(x=x,y=value,linetype=variable,linewidth=variable, col= Model))+
  facet_grid(~sex) + 
      xlab("Relative height loss in %") +
      ylab("Probabilty")+
      ylim(c(0,1)) +
      labs(color="")+
      labs(color="",linetype="")+
      # ggtitle(Title) +
      scale_colour_manual("",
                          values = c(cbp1[1],cbp1[2],cbp1[3],cbp1[4]))+
      scale_linetype_manual(name= "", guide = "none",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      scale_linewidth_manual(name= "",guide = "none",
                             limits=c("fit","CIl","CIu"),
                             labels=c("fit","95% CI","95% CI"),
                             values =c(3,1.5,1.5))+
      guides(colour = guide_legend(override.aes = list(size=10,lwd=6)))+
      theme_bw()+
      theme(aspect.ratio=1,
            plot.title = element_text(size=size_axis),
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=20),
            axis.title=element_text(size=22),
            legend.text=element_text(size=25),
            legend.title=element_text(size=size_legend_title),
            legend.position="bottom")
    

save_plot("output/Figure2.pdf", Figure2,base_height=10,base_width=16) 