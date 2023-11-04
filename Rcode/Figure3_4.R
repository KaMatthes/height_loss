# Heightloss  Male

plot1_m <- function_health_outcome_gam_figure(varOutcome="health.score.sum",sex="male", title="A) General health status", var_nr=1,legend_t = "no")
plot2_m <- function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="male", title="B) Chronic diseases", var_nr=1,legend_t = "no")
plot3_m <- function_health_outcome_gam_figure(varOutcome="osteo",sex="male", title="C) Osteoarthritis", var_nr=1, legend_t = "no")
plot4_m <- function_health_outcome_gam_figure(varOutcome="pain.walking",sex="male", title= "D) Pain while walking", var_nr=1,legend_t = "no")

plot_leg <- get_legend(function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="male", title="Chronic disease score", var_nr=1,legend_t = "legend1"))
plot_leg <- as_ggplot(plot_leg)

plot_male_loss <- plot_grid(plot1_m, plot2_m, plot3_m, plot4_m,align="hv",
                            ncol=2,nrow=2)

title <- ggdraw() + draw_label("Probability of height loss - male", x=0.20, fontface='bold', size=18)
Figure_male_loss <- plot_grid(title,plot_male_loss,plot_leg, nrow=3, rel_heights = c(.05,1,.05))

save_plot("output/Figure2.pdf", Figure_male_loss,base_height=16,base_width=14)

# Heightloss  Female

plot1_f <- function_health_outcome_gam_figure(varOutcome="health.score.sum",sex="female", title="A) General health status", var_nr=1,legend_t = "no")
plot2_f <- function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="female", title="B) Chronic diseases", var_nr=1,legend_t = "no")
plot3_f <- function_health_outcome_gam_figure(varOutcome="osteo",sex="female", title="C) Osteoarthritis", var_nr=1,legend_t = "no")
plot4_f <- function_health_outcome_gam_figure(varOutcome="pain.walking",sex="female", title="D) Pain while walking", var_nr=1,legend_t = "no")

plot_female_loss <- plot_grid(plot1_f, plot2_f, plot3_f, plot4_f,align="hv",
                            ncol=2,nrow=2)

title <- ggdraw() + draw_label("Probability of height loss - female", x=0.21, fontface='bold', size=18)
Figure_female_loss <- plot_grid(title,plot_female_loss,plot_leg, nrow=3, rel_heights = c(.05,1,.05))
save_plot("output/Figure3.pdf", Figure_female_loss,base_height=16,base_width=14)






# # SITAR height  Male 
# 
# plot1_m_h <- function_health_outcome_gam_figure(varOutcome="health.score.sum",sex="male", title="A) General health status", var_nr=2,legend_t = "no")
# plot2_m_h <- function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="male", title="B) Chronic diseases", var_nr=2,legend_t = "no")
# plot3_m_h <- function_health_outcome_gam_figure(varOutcome="osteo",sex="male", title="C) Osteoarthritis", var_nr=2,legend_t = "no")
# plot4_m_h <- function_health_outcome_gam_figure(varOutcome="pain.walking",sex="male", title="D) Pain while walking", var_nr=2,legend_t = "no")
# 
# plot_leg <- get_legend(function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="male", title="Chronic disease score",var_nr=2,legend_t = "legend2"))
# plot_leg <- as_ggplot(plot_leg)
# 
# plot_male_height <- plot_grid(plot1_m_h, plot2_m_h, plot3_m_h, plot4_m_h,align="hv",
#                             ncol=2,nrow=2)
# 
# title <- ggdraw() + draw_label("Probability of SITAR size height - male", x=0.225, fontface='bold', size=18)
# Figure_male_height <- plot_grid(title,plot_male_height,plot_leg, nrow=3, rel_heights = c(.05,1,.05))
# save_plot("output/Figure4_supp.pdf", Figure_male_height,base_height=16,base_width=14)
# 
# 
# # Height  Female
# 
# plot1_f_h <- function_health_outcome_gam_figure(varOutcome="health.score.sum",sex="female", title="A) General health status", var_nr=2,legend_t = "no")
# plot2_f_h <- function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="female", title="B) Chronic diseases", var_nr=2,legend_t = "no")
# plot3_f_h <- function_health_outcome_gam_figure(varOutcome="osteo",sex="female", title="C) Osteoarthritis", var_nr=2,legend_t = "no")
# plot4_f_h <- function_health_outcome_gam_figure(varOutcome="pain.walking",sex="female",title="D) Pain while walking", var_nr=2,legend_t = "no")
# 
# plot_leg <- get_legend(function_health_outcome_gam_figure(varOutcome="chronicDiseaseScore",sex="female", title="Chronic disease score", var_nr=2,legend_t = "no"))
# plot_leg <- as_ggplot(plot_leg)
# 
# plot_female_height <- plot_grid(plot1_f_h, plot2_f_h, plot3_f_h, plot4_f_h,align="hv",
#                               ncol=2,nrow=2)
# 
# title <- ggdraw() + draw_label("Probability of SITAR size height - female", x=0.235, fontface='bold', size=18)
# Figure_female_height <- plot_grid(title,plot_female_height,plot_leg, nrow=3, rel_heights = c(.05,1,.05))
# save_plot("output/Figure5_supp.pdf", Figure_female_height,base_height=16,base_width=14)
