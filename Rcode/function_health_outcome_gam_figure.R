
function_health_outcome_gam_figure <- function (varOutcome, sex, title_plot, var_nr,legend_t) {

  dat_gam <- dat %>%
    select(eval(substitute(varOutcome)),sex,heightdiff_rel,SITAR_size_height, Excess.weight, region,education,socialclass,smoking, fruits,exercise) %>%
    filter(complete.cases(.)) %>%
    filter(!heightdiff_rel > 6)

  GAM_plot_function_binary <- function(model) {
    plot_mod <- plot(model, select=var_nr)
    plot_x <-  plot_mod[[var_nr]]$x
    plot_fit <-  plot_mod[[var_nr]]$fit + coef(model)[1]
    plot_CIl<- plot_fit-(1.96*( plot_mod[[var_nr]]$se))
    plot_CIu<- plot_fit+(1.96*( plot_mod[[var_nr]]$se))
    plot_data <-  data.frame(x=plot_x, fit=plogis(plot_fit),CIl=plogis(plot_CIl),CIu=plogis(plot_CIu)) %>%
      gather(., variable, value, fit:CIu)
  }
  
  if(sex=="male" & var_nr==1 &  legend_t == "no"){
  
    formula_no <-as.formula(paste(eval(substitute(varOutcome)),"~s(heightdiff_rel)"))
    mod_no <- gam(formula_no,data=dat_gam[dat_gam$sex=="1",], family = binomial)
    plot_data_no <-   GAM_plot_function_binary(mod_no) %>%
      mutate(Model= "Model 1")
    
    formula_h <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass"))
    mod_h <- gam(formula_h,data=dat_gam[dat_gam$sex=="1",], family = binomial)
    plot_data_h <-   GAM_plot_function_binary(mod_h) %>%
      mutate(Model= "Model 2")
    
    
    formula_a <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass+smoking+ fruits+exercise"))
    mod_a <- gam(formula_a,data=dat_gam[dat_gam$sex=="1",], family = binomial)
    plot_data_a <-   GAM_plot_function_binary(mod_a) %>%
      mutate(Model= "Model 3")
    
    plot_data <- rbind(plot_data_no, plot_data_h, plot_data_a)
    
    plot_height_diff <- ggplot()+
      geom_line(data=plot_data[plot_data$Model=="Model 1",], aes(x=x,y=value,linetype=variable,linewidth=variable, col= "Model 1"))+
      geom_line(data=plot_data[plot_data$Model=="Model 2",], aes(x=x,y=value,linetype=variable,linewidth=variable,color="Model 2"))+
      geom_line(data=plot_data[plot_data$Model=="Model 3",], aes(x=x,y=value,linetype=variable,linewidth=variable,color= "Model 3"))+
      xlab("Relative heightloss in %") +
      ylab("Probabilty")+
      ylim(c(0,1)) +
      labs(color="")+
      labs(color="",linetype="")+
      ggtitle(title_plot) +
      scale_colour_manual("",
                          values = c(cbp1[3],cbp1[2],cbp1[4]),
                          limits=c("Model 1","Model 2","Model 3"),
                          labels=c("Model 1","Model 2","Model 3"))+
      scale_linetype_manual(name= "", guide = "none",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      scale_linewidth_manual(name= "",guide = "none",
                             limits=c("fit","CIl","CIu"),
                             labels=c("fit","95% CI","95% CI"),
                             values =c(3,1.5,1.5))+
      theme_bw()+
      theme(aspect.ratio=1,
            plot.title = element_text(size=size_axis),
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=size_axis),
            axis.title=element_text(size=size_axis_title),
            legend.text=element_text(size=size_legend),
            legend.title=element_text(size=size_legend_title),
            legend.position="none")
    
    return(plot_height_diff)

  }
  
  else if(sex=="male" & var_nr==2 &  legend_t == "no"){
    
    formula_h <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass"))
    mod_h <- gam(formula_h,data=dat_gam[dat_gam$sex=="1",], family = binomial)
    plot_data_h <-   GAM_plot_function_binary(mod_h) %>%
      mutate(Model= "Model 2")

    
    formula_a <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass+smoking+fruits+exercise"))
    mod_a <- gam(formula_a,data=dat_gam[dat_gam$sex=="1",], family = binomial)
    plot_data_a <-   GAM_plot_function_binary(mod_a) %>%
      mutate(Model= "Model 3")
    
    plot_data <- rbind(plot_data_h, plot_data_a)
    
    plot_height_diff <- ggplot()+
      geom_line(data=plot_data[plot_data$Model=="Model 2",], aes(x=x,y=value,linetype=variable,linewidth=variable,color="Model 2"))+
      geom_line(data=plot_data[plot_data$Model=="Model 3",], aes(x=x,y=value,linetype=variable,linewidth=variable,color= "Model 3"))+
      xlab("SITAR size height") +
      ylab("Probabilty")+
      ylim(c(0,1)) +
      labs(color="")+
      labs(color="",linetype="")+
      ggtitle(title_plot) +
      scale_colour_manual("",
                          values =  c(cbp1[2],cbp1[4]),
                          limits=c("Model 2","Model 3"),
                          labels=c("Model 2","Model 3"))+
      scale_linetype_manual(name= "",  guide = "none",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      scale_linewidth_manual(name= "",guide = "none",
                             limits=c("fit","CIl","CIu"),
                             labels=c("fit","95% CI","95% CI"),
                             values =c(3,1.5,1.5))+
      theme_bw()+
      theme(aspect.ratio=1,
            plot.title = element_text(size=size_axis),
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=size_axis),
            axis.title=element_text(size=size_axis_title),
            legend.text=element_text(size=size_legend),
            legend.title=element_text(size=size_legend_title),
            legend.position="none")
    
    return(plot_height_diff)
  }
  
  
  else if(sex=="female" & var_nr==1 &  legend_t == "no"){
    
    formula_no <-as.formula(paste(eval(substitute(varOutcome)),"~s(heightdiff_rel)"))
    mod_no <- gam(formula_no,data=dat_gam[dat_gam$sex=="2",], family = binomial)
    plot_data_no <-   GAM_plot_function_binary(mod_no) %>%
      mutate(Model= "Model 1")
    
    
    formula_h <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass"))
    mod_h <- gam(formula_h,data=dat_gam[dat_gam$sex=="2",], family = binomial)
    plot_data_h <-   GAM_plot_function_binary(mod_h) %>%
      mutate(Model= "Model 2")
    
    formula_a <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass+smoking+ fruits+exercise"))
    mod_a <- gam(formula_a,data=dat_gam[dat_gam$sex=="2",], family = binomial)
    plot_data_a <-   GAM_plot_function_binary(mod_a) %>%
      mutate(Model= "Model 3")
    
    plot_data <- rbind(plot_data_no, plot_data_h, plot_data_a)
    
    plot_height_diff <- ggplot()+
      geom_line(data=plot_data[plot_data$Model=="Model 1",], aes(x=x,y=value,linetype=variable,linewidth=variable, col= "Model 1"))+
      geom_line(data=plot_data[plot_data$Model=="Model 2",], aes(x=x,y=value,linetype=variable,linewidth=variable,color="Model 2"))+
      geom_line(data=plot_data[plot_data$Model=="Model 3",], aes(x=x,y=value,linetype=variable,linewidth=variable,color= "Model 3"))+
      xlab("Relative heightloss in %") +
      ylab("Probabilty")+
      ylim(c(0,1)) +
      labs(color="")+
      labs(color="",linetype="")+
      ggtitle(title_plot) +
      scale_colour_manual("",
                          values = c(cbp1[3],cbp1[2],cbp1[4]),
                          limits=c("Model 1","Model 2","Model 3"),
                          labels=c("Model 1","Model 2","Model 3"))+
      scale_linetype_manual(name= "", guide = "none",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      scale_linewidth_manual(name= "",guide = "none",
                             limits=c("fit","CIl","CIu"),
                             labels=c("fit","95% CI","95% CI"),
                             values =c(3,1.5,1.5))+
      theme_bw()+
      theme(aspect.ratio=1,
            plot.title = element_text(size=size_axis),
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=size_axis),
            axis.title=element_text(size=size_axis_title),
            legend.text=element_text(size=size_legend),
            legend.title=element_text(size=size_legend_title),
            legend.position="none")
    
    return(plot_height_diff)
  }
  
  else if(sex=="female" & var_nr==2 &  legend_t == "no"){
    
    formula_h <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass"))
    mod_h <- gam(formula_h,data=dat_gam[dat_gam$sex=="2",], family = binomial)
    plot_data_h <-   GAM_plot_function_binary(mod_h) %>%
      mutate(Model= "Model 2")
    
    formula_a <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass+smoking+fruits+exercise"))
    mod_a <- gam(formula_a,data=dat_gam[dat_gam$sex=="2",], family = binomial)
    plot_data_a <-   GAM_plot_function_binary(mod_a) %>%
      mutate(Model= "Model 3")
    
    
    plot_data <- rbind(plot_data_h, plot_data_a)
    
    plot_height_diff <- ggplot()+
      geom_line(data=plot_data[plot_data$Model=="Model 2",], aes(x=x,y=value,linetype=variable,linewidth=variable,color="Model 2"))+
      geom_line(data=plot_data[plot_data$Model=="Model 3",], aes(x=x,y=value,linetype=variable,linewidth=variable,color= "Model 3"))+
      xlab("SITAR size height") +
      ylab("Probabilty")+
      ylim(c(0,1)) +
      labs(color="")+
      labs(color="",linetype="")+
      ggtitle(title_plot) +
      scale_colour_manual("",
                          values =  c(cbp1[2],cbp1[4]),
                          limits=c("Model 2","Model 3"),
                          labels=c("Model 2","Model 3"))+
      scale_linetype_manual(name= "",  guide = "none",
                            limits=c("fit","CIl","CIu"),
                            labels=c("fit","95% CI","95% CI"),
                            values =c("solid","52","52"))+
      scale_linewidth_manual(name= "",guide = "none",
                             limits=c("fit","CIl","CIu"),
                             labels=c("fit","95% CI","95% CI"),
                             values =c(3,1.5,1.5))+
      theme_bw()+
      theme(aspect.ratio=1,
            plot.title = element_text(size=size_axis),
            strip.text.x=element_text(size=strip_text),
            axis.text=element_text(color="black",size=size_axis),
            axis.title=element_text(size=size_axis_title),
            legend.text=element_text(size=size_legend),
            legend.title=element_text(size=size_legend_title),
            legend.position="none")
    
    return(plot_height_diff)
  }
  


  else if(sex=="male" & var_nr==1 & legend_t == "legend1"){
    
    formula_no <-as.formula(paste(eval(substitute(varOutcome)),"~s(heightdiff_rel)"))
    mod_no <- gam(formula_no,data=dat_gam[dat_gam$sex=="1",], family = binomial)
    plot_data_no <-   GAM_plot_function_binary(mod_no) %>%
      mutate(Model= "Model 1")
    
    
    formula_h <-as.formula(paste(eval(substitute(varOutcome)),"~s(heightdiff_rel)"))
    mod_h <- gam(formula_h,data=dat_gam[dat_gam$sex=="1",], family = binomial)
    plot_data_h <-   GAM_plot_function_binary(mod_h) %>%
      mutate(Model= "Model 2")
    
    
    formula_a <-as.formula(paste(eval(substitute(varOutcome)),"~s(heightdiff_rel)"))
    mod_a <- gam(formula_a,data=dat_gam[dat_gam$sex=="1",], family = binomial)
    plot_data_a <-   GAM_plot_function_binary(mod_a) %>%
      mutate(Model= "Model 3")
    
    plot_data <- rbind(plot_data_no, plot_data_h, plot_data_a)
  
    
    plot_height_diff <- ggplot()+
      geom_line(data=plot_data[plot_data$Model=="Model 1",], aes(x=x,y=value,linetype=variable,linewidth=variable, col= "Model 1"))+
      geom_line(data=plot_data[plot_data$Model=="Model 2",], aes(x=x,y=value,linetype=variable,linewidth=variable,color="Model 2"))+
      geom_line(data=plot_data[plot_data$Model=="Model 3",], aes(x=x,y=value,linetype=variable,linewidth=variable,color= "Model 3"))+
      xlab("Relative heightloss in %") +
      ylab("Probabilty")+
      ylim(c(0,1)) +
      labs(color="")+
      labs(color="",linetype="")+
      # ggtitle(title_plot) +
      scale_colour_manual("",
                          values = c(cbp1[3],cbp1[2],cbp1[4]),
                          limits=c("Model 1","Model 2","Model 3"),
                          labels=c("Model 1","Model 2","Model 3"))+
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
            axis.text=element_text(color="black",size=size_axis),
            axis.title=element_text(size=size_axis_title),
            legend.text=element_text(size=25),
            legend.title=element_text(size=size_legend_title),
            legend.position="bottom")
    
    return(plot_height_diff)
  }
  
  
  else if(sex=="male" & var_nr==2 & legend_t == "legend2"){

    
    formula_h <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass"))
    mod_h <- gam(formula_h,data=dat_gam[dat_gam$sex=="2",], family = binomial)
    plot_data_h <-   GAM_plot_function_binary(mod_h) %>%
      mutate(Model= "Model 2")
    
    formula_a <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+region+education+socialclass+smoking+ fruits+exercise"))
    mod_a <- gam(formula_a,data=dat_gam[dat_gam$sex=="2",], family = binomial)
    plot_data_a <-   GAM_plot_function_binary(mod_a) %>%
      mutate(Model= "Model 3")
    
    
    plot_data <- rbind(plot_data_h, plot_data_a)

    plot_height_diff <- ggplot()+
      geom_line(data=plot_data[plot_data$Model=="Model 2",], aes(x=x,y=value,linetype=variable,linewidth=variable,color="Model 2"))+
      geom_line(data=plot_data[plot_data$Model=="Model 3",], aes(x=x,y=value,linetype=variable,linewidth=variable,color= "Model 3"))+
      xlab("SITAR size height") +
      ylab("Probabilty")+
      ylim(c(0,1)) +
      labs(color="")+
      labs(color="",linetype="")+
      ggtitle(title_plot) +
      scale_colour_manual("",
                          values =  c(cbp1[2],cbp1[4]),
                          limits=c("Model 2","Model 3"),
                          labels=c("Model 2","Model 3"))+
      scale_linetype_manual(name= "",  guide = "none",
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
            axis.text=element_text(color="black",size=size_axis),
            axis.title=element_text(size=size_axis_title),
            legend.text=element_text(size=25),
            legend.title=element_text(size=size_legend_title),
            legend.position="bottom")

    return(plot_height_diff)
  }

 
}

