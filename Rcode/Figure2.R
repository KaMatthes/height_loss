# Heightloss  Male

plot1_m <- function_health_outcome_gam_figure(varOutcome="health.score.sum",sex="male", title="General health status", var_nr=1,legend_t = "no")
plot2_m <- function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="male", title="Chronic diseases", var_nr=1,legend_t = "no")
plot3_m <- function_health_outcome_gam_figure(varOutcome="osteo",sex="male", title="Osteoarthritis", var_nr=1, legend_t = "no")
plot4_m <- function_health_outcome_gam_figure(varOutcome="pain.walking",sex="male", title="Ability to walk restricted due to pain", var_nr=1,legend_t = "no")

plot_leg <- get_legend(function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="male", title="Chronic disease score", var_nr=1,legend_t = "legend1"))
plot_leg <- as_ggplot(plot_leg)

plot_male_loss <- plot_grid(plot1_m, plot2_m, plot3_m, plot4_m,align="hv",
                            ncol=2,nrow=2)

title <- ggdraw() + draw_label("Probability of heightloss - male", x=0.25, fontface='bold', size=25)
Figure_male_loss <- plot_grid(title,plot_male_loss,plot_leg, nrow=3, rel_heights = c(.05,1,.05))
save_plot("Analysis/output/Figure_male_loss.pdf", Figure_male_loss,base_height=16,base_width=14)

# Heightloss  Female

plot1_f <- function_health_outcome_gam_figure(varOutcome="health.score.sum",sex="female", title="General health status", var_nr=1,legend_t = "no")
plot2_f <- function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="female", title="Chronic diseases", var_nr=1,legend_t = "no")
plot3_f <- function_health_outcome_gam_figure(varOutcome="osteo",sex="female", title="Osteoarthritis", var_nr=1,legend_t = "no")
plot4_f <- function_health_outcome_gam_figure(varOutcome="pain.walking",sex="female", title="Ability to walk restricted due to pain", var_nr=1,legend_t = "no")

# plot_leg <- get_legend(function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="female", title="Chronic disease score",var_nr=1,legend_t = "legend1"))
# plot_leg <- as_ggplot(plot_leg)

plot_female_loss <- plot_grid(plot1_f, plot2_f, plot3_f, plot4_f,align="hv",
                            ncol=2,nrow=2)

title <- ggdraw() + draw_label("Probability of heightloss - female", x=0.26, fontface='bold', size=25)
Figure_female_loss <- plot_grid(title,plot_female_loss,plot_leg, nrow=3, rel_heights = c(.05,1,.05))
save_plot("Analysis/output/Figure_female_loss.pdf", Figure_female_loss,base_height=16,base_width=14)

# SITAR height  Male 

plot1_m_h <- function_health_outcome_gam_figure(varOutcome="health.score.sum",sex="male", title="General health status", var_nr=2,legend_t = "no")
plot2_m_h <- function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="male", title="Chronic diseases", var_nr=2,legend_t = "no")
plot3_m_h <- function_health_outcome_gam_figure(varOutcome="osteo",sex="male", title="Osteoarthritis", var_nr=2,legend_t = "no")
plot4_m_h <- function_health_outcome_gam_figure(varOutcome="pain.walking",sex="male", title="Ability to walk restricted due to pain", var_nr=2,legend_t = "no")

plot_leg <- get_legend(function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="male", title="Chronic disease score",var_nr=2,legend_t = "legend2"))
plot_leg <- as_ggplot(plot_leg)

plot_male_height <- plot_grid(plot1_m_h, plot2_m_h, plot3_m_h, plot4_m_h,align="hv",
                            ncol=2,nrow=2)

title <- ggdraw() + draw_label("Probability of SITAR height - male", x=0.265, fontface='bold', size=25)
Figure_male_height <- plot_grid(title,plot_male_height,plot_leg, nrow=3, rel_heights = c(.05,1,.05))
save_plot("Analysis/output/Figure_male_sitar_height.pdf", Figure_male_height,base_height=16,base_width=14)


# Height  Female

plot1_f_h <- function_health_outcome_gam_figure(varOutcome="health.score.sum",sex="female", title="General health status", var_nr=2,legend_t = "no")
plot2_f_h <- function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="female", title="Chronic diseases", var_nr=2,legend_t = "no")
plot3_f_h <- function_health_outcome_gam_figure(varOutcome="osteo",sex="female", title="Osteoarthritis", var_nr=2,legend_t = "no")
plot4_f_h <- function_health_outcome_gam_figure(varOutcome="pain.walking",sex="female",title="Ability to walk restricted due to pain", var_nr=2,legend_t = "no")

plot_leg <- get_legend(function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="female", title="Chronic disease score", var_nr=2,legend_t = "no"))
plot_leg <- as_ggplot(plot_leg)

plot_female_height <- plot_grid(plot1_f_h, plot2_f_h, plot3_f_h, plot4_f_h,align="hv",
                              ncol=2,nrow=2)

title <- ggdraw() + draw_label("Probability of SITAR height - female", x=0.275, fontface='bold', size=25)
Figure_female_height <- plot_grid(title,plot_female_height,plot_leg, nrow=3, rel_heights = c(.05,1,.05))
save_plot("Analysis/output/Figure_female_sitar_height.pdf", Figure_female_height,base_height=16,base_width=14)
