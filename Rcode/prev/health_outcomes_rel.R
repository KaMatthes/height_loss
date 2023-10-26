function_health_outcome_rel <- function (varOutcome,adjusted="no",  sex="male") {
  
  
  if(adjusted=="no" & sex=="male"){
    formula <-as.formula(paste(eval(substitute(varOutcome)),"~heightdiff_rel"))
    mod <- summary(glm(formula,data=dat[dat$sex=="1",], family = binomial))
  }
  
  else if(adjusted=="yes" & sex=="male") {
    formula <-as.formula(paste(eval(substitute(varOutcome)), "~heightdiff_rel+height69+SITAR_size_height+SITAR_size_tempo+SITAR_size_velocity+Excess.weight+education+socialclass+region+smoking+ fruits+exercise"))
    mod<- summary(glm(formula, data=dat[dat$sex=="1",], family = binomial))
  }
  
  else if(adjusted=="no" & sex=="female") {
    formula <-as.formula(paste(eval(substitute(varOutcome)),"~heightdiff_rel"))
    mod <- summary(glm(formula,data=dat[dat$sex=="2",], family = binomial))
  }
  
  else{
    formula <-as.formula(paste(eval(substitute(varOutcome)), "~heightdiff_rel++height69+SITAR_size_height+SITAR_size_tempo+SITAR_size_velocity+Excess.weight+education+socialclass+region+smoking+ fruits+exercise"))
    mod <- summary(glm(formula,data=dat[dat$sex=="2",], family = binomial))
    
  }
  
  return(mod)
}

with(summary(reg), 1 - deviance/null.deviance)
