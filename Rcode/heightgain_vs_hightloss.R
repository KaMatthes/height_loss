function_heightgain_loss_rel <- function(varAge, adjusted, sex) {
  
  dat_f <- dat %>%
    select(eval(substitute(varAge)),sex, heightdiff_rel,Excess.weight, region, education, socialclass, smoking, fruits, exercise) %>%
    filter(complete.cases(.))

  if(adjusted=="no" & sex=="male"){
    formula <-as.formula(paste("heightdiff_rel ~",eval(substitute(varAge))))
    mod <- summary(lm(formula, data=dat_f[dat_f$sex=="1",]))
    }
  
  else if(adjusted=="yes" & sex=="male") {
    formula <-as.formula(paste("heightdiff_rel ~",eval(substitute(varAge)),"+Excess.weight+education+socialclass+region+smoking+ fruits+exercise"))
    mod<- summary(lm(formula, data=dat_f[dat_f$sex=="1",]))
    }
  
  else if(adjusted=="no" & sex=="female") {
    formula <-as.formula(paste("heightdiff_rel ~",eval(substitute(varAge))))
    mod<- summary(lm(formula, data=dat_f[dat_f$sex=="2",]))
    }
  
  else{
    formula <-as.formula(paste("heightdiff_rel ~",eval(substitute(varAge)),"+Excess.weight+education+socialclass+region+smoking+ fruits+exercise"))
    mod<- summary(lm(formula, data=dat_f[dat_f$sex=="2",]))
    }
  
  return(mod)
}

