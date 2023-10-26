function_heightgain_loss <- function(varAge, adjusted, sex) {
  
  dat_f <- dat 
  # %>%
  #   select(height02, height07, height11, height15, height36, height69,heightgain2_7, heightgain7_11, heightdiff,
  #          Excess.weight, education43, socialclass, smoking, fruits, exercise) %>%
  #   filter(complete.cases(.))
  
  if(adjusted=="no" & sex=="male"){
    formula <-as.formula(paste("heightdiff ~",eval(substitute(varAge))))
    mod <- summary(lm(formula, data=dat_f[dat_f$sex=="1",]))
    }
  
  else if(adjusted=="yes" & sex=="male") {
    formula <-as.formula(paste("heightdiff ~",eval(substitute(varAge)),"+Excess.weight+education+socialclass+region+smoking+ fruits+exercise"))
    mod<- summary(lm(formula, data=dat_f[dat_f$sex=="1",]))
    }
  
  else if(adjusted=="no" & sex=="female") {
    formula <-as.formula(paste("heightdiff ~",eval(substitute(varAge))))
    mod<- summary(lm(formula, data=dat_f[dat_f$sex=="2",]))
    }
  
  else{
    formula <-as.formula(paste("heightdiff ~",eval(substitute(varAge)),"+Excess.weight+education+socialclass+region+smoking+ fruits+exercise"))
    mod<- summary(lm(formula, data=dat_f[dat_f$sex=="2",]))
    }
  
  return(mod)
}



# formula <-as.formula(paste("heightdiff ~health.score.sum+Excess.weight+education43+socialclass+smoking+ fruits+exercise"))
# mod<- summary(lm(formula, data=dat))