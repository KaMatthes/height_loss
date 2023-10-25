.libPaths(c("H:/Documents/R/win-library/4.0", "C:/Program Files/R/R-4.0.5/library"))


library(tidyverse)
library(mgcv)
library(ggpubr)
library(cowplot)

# library(rmarkdown) # brauchen wir nicht
# library(kableExtra) # brauchen wir nicht
# # library(arsenal)
# library(lubridate) 
library(ggsci)
# library(knitr) # brauchen wir nicht
# library(viridis)

library(ggbeeswarm)
library(conflicted)


conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("recode", "dplyr")
conflicts_prefer(ggpubr::get_legend)

mypalette <-pal_jco()(10)
# mypalette2 <- viridis(7, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
# mypalette2 <- inferno(7, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")

strip_text <- 25
lwd_size <- 2
pch_type <- 19
lwdline <- 1
size_legend <- 15
size_legend_title<- 25
pd <-position_dodge(width=0.3)
pd_v <-position_dodge(width=0.7)
pd_b <-position_dodge(width=0.7)
fatten_size <- 6
plot_title <- 25

size_axis <- 18
size_axis_title <- 18
size_plot_title <-15
size_legend_text <- 20
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#D55E00","#A6761D" ,"#7570B3")


# load R codes
# source("Analysis/Rcode/data.R")
# # source("Analysis/Rcode/Diff_sex.R")
# source("Analysis/Rcode/heightgain_vs_hightloss.R")
# source("Analysis/Rcode/health_outcomes.R")
# source("Analysis/Rcode/Figure1.R")
# source("Analysis/Rcode/Figure2.R")
# 


# render html
# 
# render(paste0("Analysis/Rcode/Heightloss.Rmd"), output_file = paste0("C:/Users/kmatth/Dropbox/Heightloss/Analysis/output/",today(),"_Report_Heightloss.html"))


render(paste0("Analysis/Rcode/Heightloss_paper.Rmd"), output_file = paste0("C:/Users/kmatth/Dropbox/Heightloss/Analysis/output/",today(),"_Report_Heightloss_paper.html"))