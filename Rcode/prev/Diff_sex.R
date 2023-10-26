function_diff_sex <- function() {
  
  
  dat_69 <- dat %>%
    filter(!is.na(heightdiff)) %>%
    mutate(sex=as.factor(sex),
           sex=recode(sex, 
                      "1" ="male",
                      "2" ="female"))
  
  t.test(dat_69[dat_69$sex=="male",]$heightdiff,dat_69[dat_69$sex=="female",]$heightdiff)
  t.test(dat_69[dat_69$sex=="male",]$heightdiff_rel,dat_69[dat_69$sex=="female",]$heightdiff_rel)
}


           


dat_69_n <-  dat_69 %>%
filter(sex=="male")