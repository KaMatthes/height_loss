dat <- read.csv("Analysis/input/KasparStaubZZfdoeziSCRAMBLED.csv", sep=",") %>%
  dplyr::select(height_father=fht52,
         birth_weight = mbwtu,
         height_mother = mht52,
         SITAR_size_height =ah_als2,
         # SITAR_size_tempo = bh_als2,
         region = newreg46,
         
         # SITAR_size_velcity=ch_als2,
         height02 =  ht48u,
         height04 =ht50u,
         height06 =ht52u,
         height07 = ht53u,
         height11 = ht57u,
         height15 = ht61u,
         height36 = ht82u,
         height43 = ht89u,
         height53 = ht99u,
         height69= htn15x,
         osteo = osteo15x,
         osteoporosis =ostpor15x,
         chronicDiseaseScore = chron19tot15x,
         rheuma = rheum15x, 
         health.score.sum=summhealth15x,
         sex,
         education =toteduc1543,
         fruits =fru09,
         smoking =smor09,
         exercise = exer4915x,
         socialclass =chsc,
         alcohol=avalc82,
         pain.walking = wlkpain15x,
         weight = clswp1wt36) %>%
  mutate(
         height02 = ifelse(height02 > 200, NA, height02),
         height04 = ifelse(height04 > 200, NA, height04),
         height06 = ifelse(height06 > 200, NA, height06),
         height07 = ifelse(height07 > 200, NA, height07),
         height11 = ifelse(height11 > 200, NA, height11),
         height15 = ifelse(height15 > 200, NA, height15),
         height36 = ifelse(height36 > 200, NA, height36),
         height43 = ifelse(height43 > 200, NA, height43),
         height53 = ifelse(height53 > 200, NA, height53),
         height69 = ifelse(height69 < 100, NA, height69),
         heightgain2_7 = height07- height02,
         heightgain7_11 = height11- height07,
         heightgain11_15 = height15- height11,
         heightdiff = height36- height69,
         BMI = weight/((height36/100)^2),
         Excess.weight = ifelse(BMI >25, 1, 0),
         Excess.weight = as.factor(Excess.weight),
         education = ifelse(education> 4, NA, education),
         education = ifelse(education <3, 0, 1),
         education = as.factor(education),
         region = as.character(region),
         region = recode(region,
                         "0" = "Middle",
                         "1" = "North",
                         "2" = "North",
                         "3" = "North",
                         "4" = "Middle",
                         "5" = "Middle",
                         "6" = "Middle",
                         "7" = "South",
                         "8" = "South",
                         "9" = "North"),
         fruits  = ifelse(fruits  >3, NA, fruits),
         fruits  = ifelse(fruits  >2, 1, 0),
         fruits = as.factor(fruits),
         exercise = ifelse(exercise <0, NA, exercise),
         exercise = ifelse(exercise >1, 1, 0),
         exercise = as.factor(exercise),
         alcohol = round(alcohol,2),
         alcohol1 = alcohol,
         alcohol1 = ifelse(alcohol1==0, NA, alcohol1),
         alcohol_m = median(alcohol1, na.rm=TRUE),
         alcohol_g = alcohol,
         alcohol_g = case_when(
           alcohol_g <  alcohol_m   ~ "Below Average",
           alcohol_g >= alcohol_m ~ "Above Average"),
         alcohol_g = ifelse(alcohol_g=="Below Average" & alcohol==0, "none", alcohol_g),
         exercise = as.factor(exercise),
         
         socialclass = ifelse(socialclass >6, NA, socialclass),
         socialclass = replace(socialclass, socialclass==1,"low"),
         socialclass = replace(socialclass, socialclass==2,"low"),
         socialclass = replace(socialclass, socialclass==3,"medium"),
         socialclass = replace(socialclass, socialclass==4,"medium"),
         socialclass = replace(socialclass, socialclass==5,"high"),
         socialclass = replace(socialclass, socialclass==6,"high"),
         smoking = ifelse( smoking >1, NA,  smoking),
         smoking = as.factor(smoking),
         health.score.sum=ifelse( health.score.sum<1, NA,health.score.sum),
         health.score.sum=ifelse( health.score.sum>2, 1,0),
         health.score.sum = as.factor(health.score.sum),
         osteo = ifelse( osteo <0, NA,  osteo),
         osteo = as.factor(osteo),
         chronicDiseaseScore=ifelse(chronicDiseaseScore<0,NA,chronicDiseaseScore),
         chronicDiseaseScore=ifelse(chronicDiseaseScore<3,0,1),
         chronicDiseaseScore = as.factor(chronicDiseaseScore),
         pain.walking=ifelse(pain.walking <0,NA,pain.walking),
         pain.walking = as.factor(pain.walking)) %>%
  filter(!heightdiff > 15 | is.na(heightdiff)) %>%  
 filter(!heightdiff < -5 | is.na(heightdiff)) %>%

  # filter(!heightdiff < 0) %>%
  droplevels %>%
  group_by(sex) %>%
  mutate(height02_z = scale(height02),
         height07_z = scale(height07),
         height11_z = scale(height11),
         height15_z = scale(height15),
         height36_z = scale(height36),
         heightgain2_7_z = scale(heightgain2_7),
         heightgain7_11_z = scale(heightgain7_11),
         heightgain11_15_z = scale(heightgain11_15),
         heightdiff_z = scale(heightdiff),
         SITAR_size_height_z= scale(SITAR_size_height)
         # SITAR_size_tempo_z = scale(SITAR_size_tempo),
         # SITAR_size_velocity_z = scale(SITAR_size_velocity)
         ) %>%
  ungroup() %>%
  mutate(heightdiff = ifelse(heightdiff<0, 0, heightdiff),
         heightdiff_rel = heightdiff/height69*100)

# 
# num <- nrow(dat)
# mean_v <- -0.0037
# sd_v <- 0.09
# 
# dat_velocity <- rnorm(num, mean_v,sd_v )

# dat <- dat %>%
  # mutate(SITAR_size_velocity = dat_velocity) %>%
  # group_by(sex) %>%
  # mutate( SITAR_size_velocity_z = scale(SITAR_size_velocity)) %>%
  # ungroup() %>%
