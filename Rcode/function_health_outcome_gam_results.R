
function_health_outcome_gam_results <- function (varOutcome) {

  dat_gam <- dat %>%
    select(eval(substitute(varOutcome)),sex,heightdiff_rel,height69,SITAR_size_height, Excess.weight, region,education,socialclass,smoking, fruits,exercise) %>%
    filter(complete.cases(.)) %>%
    filter(!heightdiff_rel > 6) %>%
    mutate(Excess.weight = factor(Excess.weight, levels=c("0","1")),
           education  = factor(education, levels=c("0","1")),
           socialclass  = factor(socialclass, levels=c("low","medium","high")),
           region = factor(region, levels=c("South","Middle","North")),
           smoking = factor(smoking, levels=c("0", "1")),
           fruits= factor(fruits, levels=c("0", "1")),
           exercise = factor(exercise, levels=c("0", "1")))

  
    formula_no <-as.formula(paste(eval(substitute(varOutcome)),"~s(heightdiff_rel)"))
    mod_no <- summary(gam(formula_no,data=dat_gam[dat_gam$sex=="1",], family = binomial))
    mod_no_d <- mod_no$s.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Model="Model 1") %>%
      select(Model, Var, p.value)
    
    formula_h <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+education+socialclass+region"))
    mod_h <- summary(gam(formula_h,data=dat_gam[dat_gam$sex=="1",], family = binomial))
    mod_h_f <-  mod_h$p.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Est = round(exp(Estimate),2),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             `OR 95% CI` = paste0(Est," (", CIl,"-",CIu,")"),
             pvalue=NA, 
             Model= "Model 2") %>%
      select(Model, Var, `OR 95% CI`)
    
    mod_h_s <-  mod_h$s.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Model="Model 2") %>%
      select(Model, Var, p.value)
    
    mod_h_d <- bind_rows(mod_h_f, mod_h_s)


    formula_a <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+education+socialclass+region+smoking+fruits+exercise"))
    mod_a <- summary(gam(formula_a,data=dat_gam[dat_gam$sex=="1",], family = binomial))

    mod_a_f <-  mod_a$p.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Est = round(exp(Estimate),2),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             `OR 95% CI` = paste0(Est," (", CIl,"-",CIu,")"),
             pvalue=NA, 
             Model= "Model 3") %>%
      select(Model, Var, `OR 95% CI`)
    
    mod_a_s <-  mod_a$s.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Model="Model 3") %>%
      select(Model, Var, p.value)
    
    mod_a_d <- bind_rows(mod_a_f, mod_a_s)
    results_gam_m <- bind_rows(mod_no_d, mod_h_d, mod_a_d) %>%
      mutate(Sex="male")
    
    formula_no <-as.formula(paste(eval(substitute(varOutcome)),"~s(heightdiff_rel)"))
    mod_no <- summary(gam(formula_no,data=dat_gam[dat_gam$sex=="2",], family = binomial))
    mod_no_d <- mod_no$s.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Model="Model 1") %>%
      select(Model, Var, p.value)
    
    formula_h <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+education+region+socialclass"))
    mod_h <- summary(gam(formula_h,data=dat_gam[dat_gam$sex=="2",], family = binomial))
    mod_h_f <-  mod_h$p.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Est = round(exp(Estimate),2),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             `OR 95% CI` = paste0(Est," (", CIl,"-",CIu,")"),
             pvalue=NA, 
             Model= "Model 2") %>%
      select(Model, Var, `OR 95% CI`)
    
    mod_h_s <-  mod_h$s.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Model="Model 2") %>%
      select(Model, Var, p.value)
    
    mod_h_d <- bind_rows(mod_h_f, mod_h_s)
    
    formula_a <-as.formula(paste(eval(substitute(varOutcome)), "~s(heightdiff_rel)+s(SITAR_size_height)+Excess.weight+education+socialclass+region+smoking+ fruits+exercise"))
    mod_a <- summary(gam(formula_a,data=dat_gam[dat_gam$sex=="2",], family = binomial))
    
    mod_a_f <-  mod_a$p.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Est = round(exp(Estimate),2),
             CIl = round(exp(Estimate - 1.96*`Std..Error`),2),
             CIu = round(exp(Estimate + 1.96*`Std..Error`),2),
             `OR 95% CI` = paste0(Est," (", CIl,"-",CIu,")"),
             pvalue=NA, 
             Model= "Model 3") %>%
      select(Model, Var, `OR 95% CI`)
    
    mod_a_s <-  mod_a$s.table %>%
      data.frame() %>%
      mutate(Var = rownames(.),
             Model="Model 3") %>%
      select(Model, Var, p.value)
    
    mod_a_d <- bind_rows(mod_a_f, mod_a_s) 
    
    results_gam_f <- bind_rows(mod_no_d, mod_h_d, mod_a_d) %>%
      mutate(Sex="female")
  
    results_gam <- bind_rows(results_gam_m, results_gam_f) %>%
      filter(!Var=="(Intercept)")
 
 write.table(results_gam,file=paste0("Analysis/output/results_gam_",varOutcome,".csv"),row.names=FALSE, sep=";")
}

function_health_outcome_gam_results("health.score.sum")
function_health_outcome_gam_results("chronicDiseaseScore")
function_health_outcome_gam_results("osteo")
function_health_outcome_gam_results("pain.walking")
