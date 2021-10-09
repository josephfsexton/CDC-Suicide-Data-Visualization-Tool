
'''
NVSS Suicide Compiler
Joseph Sexton, Rashmi Jha, Mark Raj
'''

library("dplyr")
library("ggplot2")
library("tidyr")
library("tidyverse")
library("stringr")
library("readxl")
# install.packages("rapport")
library("rapport")
library("rlang")

for (year_int in 2009:2019){
  deaths1 <- read.csv(paste("C:\\rProjects\\suicide\\suicides2.0\\mort", toString(year_int), "us\\CDC",
                            toString(year_int), "pt1.csv", sep=""), header=FALSE)
  deaths2 <- read.csv(paste("C:\\rProjects\\suicide\\suicides2.0\\mort", toString(year_int), "us\\CDC",
                            toString(year_int), "pt2.csv", sep=""), header=FALSE)
  deaths3 <- read.csv(paste("C:\\rProjects\\suicide\\suicides2.0\\mort", toString(year_int), "us\\CDC",
                            toString(year_int), "pt3.csv", sep=""), header=FALSE)
  col_needed <- c("resident.status", "education",
                  "month.of.death", "sex", "age", "age.flag", "age.recode52", "age.recode27",
                  "age.recode12", "infant.age", "place.of.death", "marital.status",
                  "day.of.week", "death.year", "work.injury", "manner.of.death",
                  "disposition.method", "autopsy", "activity.code", "place",
                  "icd.code", "icd.recode358", "icd.recode113", "icd.recode130",
                  "icd.recode39", "number.of.causes", "cause1", "cause2", "cause3",
                  "cause4", "cause5", "cause6", "cause7", "cause8", "cause9",
                  "cause10", "cause11", "cause12", "cause13", "cause14", "cause15",
                  "cause16", "cause17", "cause18", "cause19", "cause20", "number.of.conditions",
                  "condition1", "condition2", "condition3", "condition4", "condition5",
                  "condition6", "condition7", "condition8", "condition9", "condition10",
                  "condition11", "condition12", "condition13", "condition14", "condition15",
                  "condition16", "condition17", "condition18", "condition19", "condition20",
                  "race", "bridged.race.flag", "race.imputation.flag", "race.recode3",
                  "race.recode5", "hispanic.origin", "hispanic.origin.recode")
  correctinfo1 <- deaths1[, c(1:74)]
  colnames(correctinfo1) <- col_needed
  correctinfo2 <- deaths2[, c(1:74)]
  colnames(correctinfo2) <- col_needed
  correctinfo3 <- deaths3[, c(1:74)]
  colnames(correctinfo3) <- col_needed
  remove(deaths1, deaths2, deaths3)
  
  # ICD Code Sorting
  # X60 Intentional self-poisoning by and exposure to nonopioid
  # analgesics, antipyretics and antirheumatics
  
  x60a <- correctinfo1 %>% filter(str_detect(icd.code, 'X60'))
  x60b <- correctinfo2 %>% filter(str_detect(icd.code, 'X60'))
  x60c <- correctinfo3 %>% filter(str_detect(icd.code, 'X60'))
  x60total <- rbind(x60a, x60b, x60c)
  rm(x60a, x60b, x60c)
  
  # X61 Intentional self-poisoning by and exposure to antiepileptics,
  # sedative-hypnotic, antiparkinsonism and psychotropic drugs, not
  # elsewhere classified
  
  x61a <- correctinfo1 %>% filter(str_detect(icd.code, 'X61'))
  x61b <- correctinfo2 %>% filter(str_detect(icd.code, 'X61'))
  x61c <- correctinfo3 %>% filter(str_detect(icd.code, 'X61'))
  x61total <- rbind(x61a, x61b, x61c)
  rm(x61a, x61b, x61c)
  
  # X62 Intentional self-poisoning by and exposure to narcotics and
  # psychodysletpics [hallucinogens], not elsewhere classified
  
  x62a <- correctinfo1 %>% filter(str_detect(icd.code, 'X62'))
  x62b <- correctinfo2 %>% filter(str_detect(icd.code, 'X62'))
  x62c <- correctinfo3 %>% filter(str_detect(icd.code, 'X62'))
  x62total <- rbind(x62a, x62b, x62c)
  rm(x62a, x62b, x62c)
  
  # X63 Intentional self-poisoning by and exposure to other drugs
  # acting on the autonomic nervous system
  
  x63a <- correctinfo1 %>% filter(str_detect(icd.code, 'X63'))
  x63b <- correctinfo2 %>% filter(str_detect(icd.code, 'X63'))
  x63c <- correctinfo3 %>% filter(str_detect(icd.code, 'X63'))
  x63total <- rbind(x63a, x63b, x63c)
  rm(x63a, x63b, x63c)
  
  # X64 Intentional self-poisoning by and exposure to other and
  # unspecified drugs, medicaments and biological substances
  
  x64a <- correctinfo1 %>% filter(str_detect(icd.code, 'X64'))
  x64b <- correctinfo2 %>% filter(str_detect(icd.code, 'X64'))
  x64c <- correctinfo3 %>% filter(str_detect(icd.code, 'X64'))
  x64total <- rbind(x64a, x64b, x64c)
  rm(x64a, x64b, x64c)
  
  # X65 Intentional self-poisoning by and exposure to alcohol
  
  x65a <- correctinfo1 %>% filter(str_detect(icd.code, 'X65'))
  x65b <- correctinfo2 %>% filter(str_detect(icd.code, 'X65'))
  x65c <- correctinfo3 %>% filter(str_detect(icd.code, 'X65'))
  x65total <- rbind(x65a, x65b, x65c)
  rm(x65a, x65b, x65c)
  
  # X66 Intentional self-poisoning by and exposure to organic solvents
  # and halogenated hydrocarbons and their vapors
  
  x66a <- correctinfo1 %>% filter(str_detect(icd.code, 'X66'))
  x66b <- correctinfo2 %>% filter(str_detect(icd.code, 'X66'))
  x66c <- correctinfo3 %>% filter(str_detect(icd.code, 'X66'))
  x66total <- rbind(x66a, x66b, x66c)
  rm(x66a, x66b, x66c)
  
  # X67 Intentional self-poisoning by and exposure to other gases
  # and vapors
  
  x67a <- correctinfo1 %>% filter(str_detect(icd.code, 'X67'))
  x67b <- correctinfo2 %>% filter(str_detect(icd.code, 'X67'))
  x67c <- correctinfo3 %>% filter(str_detect(icd.code, 'X67'))
  x67total <- rbind(x67a, x67b, x67c)
  rm(x67a, x67b, x67c)
  
  # X68 Intentional self-poisoning by and exposure to pesticides
  
  x68a <- correctinfo1 %>% filter(str_detect(icd.code, 'X68'))
  x68b <- correctinfo2 %>% filter(str_detect(icd.code, 'X68'))
  x68c <- correctinfo3 %>% filter(str_detect(icd.code, 'X68'))
  x68total <- rbind(x68a, x68b, x68c)
  rm(x68a, x68b, x68c)
  
  # X69 Intentional self-poisoning by and exposure to other and
  # unspecified chemicals and noxious substances
  
  x69a <- correctinfo1 %>% filter(str_detect(icd.code, 'X69'))
  x69b <- correctinfo2 %>% filter(str_detect(icd.code, 'X69'))
  x69c <- correctinfo3 %>% filter(str_detect(icd.code, 'X69'))
  x69total <- rbind(x69a, x69b, x69c)
  rm(x69a, x69b, x69c)
  
  # X70 Intentional self-harm by hanging, strangulation and
  # suffocation
  
  x70a <- correctinfo1 %>% filter(str_detect(icd.code, 'X70'))
  x70b <- correctinfo2 %>% filter(str_detect(icd.code, 'X70'))
  x70c <- correctinfo3 %>% filter(str_detect(icd.code, 'X70'))
  x70total <- rbind(x70a, x70b, x70c)
  rm(x70a, x70b, x70c)
  
  # X71 Intentional self-harm by drowning and submersion
  
  x71a <- correctinfo1 %>% filter(str_detect(icd.code, 'X71'))
  x71b <- correctinfo2 %>% filter(str_detect(icd.code, 'X71'))
  x71c <- correctinfo3 %>% filter(str_detect(icd.code, 'X71'))
  x71total <- rbind(x71a, x71b, x71c)
  rm(x71a, x71b, x71c)
  
  # X72 Intentional self-harm by handgun discharge
  
  x72a <- correctinfo1 %>% filter(str_detect(icd.code, 'X72'))
  x72b <- correctinfo2 %>% filter(str_detect(icd.code, 'X72'))
  x72c <- correctinfo3 %>% filter(str_detect(icd.code, 'X72'))
  x72total <- rbind(x72a, x72b, x72c)
  rm(x72a, x72b, x72c)
  
  # X73 Intentional self-harm by rifle, shotfun, and larger firearm
  # discharge
  
  x73a <- correctinfo1 %>% filter(str_detect(icd.code, 'X73'))
  x73b <- correctinfo2 %>% filter(str_detect(icd.code, 'X73'))
  x73c <- correctinfo3 %>% filter(str_detect(icd.code, 'X73'))
  x73total <- rbind(x73a, x73b, x73c)
  rm(x73a, x73b, x73c)
  
  # X74 Intentional self-harm by other and unspecified firearm discharge
  
  x74a <- correctinfo1 %>% filter(str_detect(icd.code, 'X74'))
  x74b <- correctinfo2 %>% filter(str_detect(icd.code, 'X74'))
  x74c <- correctinfo3 %>% filter(str_detect(icd.code, 'X74'))
  x74total <- rbind(x74a, x74b, x74c)
  rm(x74a, x74b, x74c)
  
  # X75 Intentional self-harm by explosive material
  
  x75a <- correctinfo1 %>% filter(str_detect(icd.code, 'X75'))
  x75b <- correctinfo2 %>% filter(str_detect(icd.code, 'X75'))
  x75c <- correctinfo3 %>% filter(str_detect(icd.code, 'X75'))
  x75total <- rbind(x75a, x75b, x75c)
  rm(x75a, x75b, x75c)
  
  # X76 Intentional self-harm by smoke, fire and flames
  
  x76a <- correctinfo1 %>% filter(str_detect(icd.code, 'X76'))
  x76b <- correctinfo2 %>% filter(str_detect(icd.code, 'X76'))
  x76c <- correctinfo3 %>% filter(str_detect(icd.code, 'X76'))
  x76total <- rbind(x76a, x76b, x76c)
  rm(x76a, x76b, x76c)
  
  # X77 Intentional self-harm by steam, hot vapors and hot objects
  
  x77a <- correctinfo1 %>% filter(str_detect(icd.code, 'X77'))
  x77b <- correctinfo2 %>% filter(str_detect(icd.code, 'X77'))
  x77c <- correctinfo3 %>% filter(str_detect(icd.code, 'X77'))
  x77total <- rbind(x77a, x77b, x77c)
  rm(x77a, x77b, x77c)
  
  # X78 Intentional self-harm by sharp object
  
  x78a <- correctinfo1 %>% filter(str_detect(icd.code, 'X78'))
  x78b <- correctinfo2 %>% filter(str_detect(icd.code, 'X78'))
  x78c <- correctinfo3 %>% filter(str_detect(icd.code, 'X78'))
  x78total <- rbind(x78a, x78b, x78c)
  rm(x78a, x78b, x78c)
  
  # X79 Intentional self-harm by blunt object
  
  x79a <- correctinfo1 %>% filter(str_detect(icd.code, 'X79'))
  x79b <- correctinfo2 %>% filter(str_detect(icd.code, 'X79'))
  x79c <- correctinfo3 %>% filter(str_detect(icd.code, 'X79'))
  x79total <- rbind(x79a, x79b, x79c)
  rm(x79a, x79b, x79c)
  
  # X80 Intentional self-harm by jumping from a high place
  
  x80a <- correctinfo1 %>% filter(str_detect(icd.code, 'X80'))
  x80b <- correctinfo2 %>% filter(str_detect(icd.code, 'X80'))
  x80c <- correctinfo3 %>% filter(str_detect(icd.code, 'X80'))
  x80total <- rbind(x80a, x80b, x80c)
  rm(x80a, x80b, x80c)
  
  # X81 Intentional self-harm by jumping or lying before moving object
  
  x81a <- correctinfo1 %>% filter(str_detect(icd.code, 'X81'))
  x81b <- correctinfo2 %>% filter(str_detect(icd.code, 'X81'))
  x81c <- correctinfo3 %>% filter(str_detect(icd.code, 'X81'))
  x81total <- rbind(x81a, x81b, x81c)
  remove(x81a, x81b, x81c)
  
  # X82 Intentional self-harm by crashing of motor vehicle
  
  x82a <- correctinfo1 %>% filter(str_detect(icd.code, 'X82'))
  x82b <- correctinfo2 %>% filter(str_detect(icd.code, 'X82'))
  x82c <- correctinfo3 %>% filter(str_detect(icd.code, 'X82'))
  x82total <- rbind(x82a, x82b, x82c)
  remove(x82a, x82b, x82c)
  
  # X83 Intentional self-harm by other specified means
  
  x83a <- correctinfo1 %>% filter(str_detect(icd.code, 'X83'))
  x83b <- correctinfo2 %>% filter(str_detect(icd.code, 'X83'))
  x83c <- correctinfo3 %>% filter(str_detect(icd.code, 'X83'))
  x83total <- rbind(x83a, x83b, x83c)
  remove(x83a, x83b, x83c)
  
  # X84 Intentional self-harm by unspecified means
  
  x84a <- correctinfo1 %>% filter(str_detect(icd.code, 'X84'))
  x84b <- correctinfo2 %>% filter(str_detect(icd.code, 'X84'))
  x84c <- correctinfo3 %>% filter(str_detect(icd.code, 'X84'))
  x84total <- rbind(x84a, x84b, x84c)
  remove(x84a, x84b, x84c)
  
  # Y87.0 Sequelae of intentional self-harm
  
  y87a <- correctinfo1 %>% filter(str_detect(icd.code, 'X87.0'))
  y87b <- correctinfo2 %>% filter(str_detect(icd.code, 'X87.0'))
  y87c <- correctinfo3 %>% filter(str_detect(icd.code, 'X87.0'))
  y87total <- rbind(y87a, y87b, y87c)
  remove(y87a, y87b, y87c)
  
  # ----- Combine All Suicides, Sort by Marital Status and Sex -----
  suicides2018 <- rbind(x60total, x61total, x62total, x63total, x64total, x65total, x66total,
                        x67total, x68total, x69total, x70total, x71total, x72total, x73total,
                        x74total, x75total, x76total, x77total, x78total, x79total, x80total,
                        x81total, x82total, x83total, x84total, y87total)
  assign(paste0(toString(year_int), 'suicides'), suicides2018)
}

all_suicides = rbind(`2009suicides`, `2010suicides`, `2011suicides`, `2012suicides`,
                     `2013suicides`, `2014suicides`, `2015suicides`, `2016suicides`,
                     `2017suicides`, `2018suicides`, `2019suicides`)

for(i in 1:length(all_suicides)){
  
  if(all_suicides$race[i]==1) all_suicides$race_cat[i] = 'white'
  else if(all_suicides$race[i]==2) all_suicides$race_cat[i] = 'black'
  else if(all_suicides$race[i]==3) all_suicides$race_cat[i] = 'aian'
  else if(all_suicides$race[i]==4|all_suicides$race[i]==5|all_suicides$race[i]==18|
          all_suicides$race[i]==28|all_suicides$race[i]==48) all_suicides$race_cat[i] = 'asian'
  else if(all_suicides$race[i]==6|all_suicides$race[i]==7|all_suicides$race[i]==38|
          all_suicides$race[i]==58|all_suicides$race[i]==68|all_suicides$race[i]==78)
    all_suicides$race_cat[i] = 'pacific'
  
  if(all_suicides$hispanic.origin[i]<200) all_suicides$hispanic_cat[i] = 'non-hispanic'
  else if(all_suicides$hispanic.origin[i]>=200) all_suicides$hispanic_cat[i] = 'hispanic'
  
  all_suicides$age[i] == as.integer(all_suicides$age[i]) - 1000
}
all_suicides$race_cat

?write.csv
write.csv(all_suicides, "C:\\rProjects\\suicide\\suicides2.0\\all_suicides.csv")
