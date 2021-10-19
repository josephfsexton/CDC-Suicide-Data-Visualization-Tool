

# ACS Population Compiler
# Joseph Sexton, Rashmi Jha

population <- data.frame(matrix(nrow=104720,ncol=7))
colnames(population) <- c("year", "sex", "race", "hisp", "marst", "age", "pop")
population$year <- c(rep(2009,104720/11),
                     rep(2010,104720/11),
                     rep(2011,104720/11),
                     rep(2012,104720/11),
                     rep(2013,104720/11),
                     rep(2014,104720/11),
                     rep(2015,104720/11),
                     rep(2016,104720/11),
                     rep(2017,104720/11),
                     rep(2018,104720/11),
                     rep(2019,104720/11))
population$sex <- c(rep('M', 104720/11/2),
                    rep('F', 104720/11/2))
population$race <- c(rep('white', 104720/11/2/7),
                     rep('black', 104720/11/2/7),
                     rep('aian', 104720/11/2/7),
                     rep('asian', 104720/11/2/7),
                     rep('pacific', 104720/11/2/7),
                     rep('mixed', 104720/11/2/7),
                     rep('mixed', 104720/11/2/7))
population$hisp <- c(rep('hispanic', 104720/11/2/7/2),
                     rep('nonhispanic', 104720/11/2/7/2))
population$marst <- c(rep('M', 104720/11/2/7/2/4),
                      rep('W',104720/11/2/7/2/4),
                      rep('D',104720/11/2/7/2/4),
                      rep('S',104720/11/2/7/2/4))                     
population$age <- c(rep(0:84))

for(year_int in 2009:2019){
  assign(paste0("census", toString(year_int)),
         read.csv(paste("C:\\rProjects\\suicide\\suicides2.0\\mort", toString(year_int),
                        "us\\", toString(year_int), "acs.csv", sep=""), header=FALSE,
                  stringsAsFactors=FALSE))
}


census_early <- rbind(census2009, census2010, census2011)
census_late <- rbind(census2012, census2013, census2014, census2015,
                     census2016, census2017, census2018, census2019)


find_pop <- function(demo){
  race_mult =  sex_add = marst_add = hisp_mult = 0
  if(demo$race == 'white') race_mult = 0
  else if(demo$race == 'black') race_mult = 1
  else if(demo$race == 'aian') race_mult = 2:4
  else if(demo$race == 'asian') race_mult = 5
  else if(demo$race == 'pacific') race_mult = 6
  else if(demo$race == 'other') race_mult = 7
  else if(demo$race == 'mixed') race_mult = 8
  if(demo$sex == 'M') sex_add = 0
  else if(demo$sex == 'F') sex_add = 1
  if(demo$marst == 'M') marst_add = 0
  else if(demo$marst == 'W') marst_add = 1
  else if(demo$marst == 'D') marst_add = 2
  else if(demo$marst == 'S') marst_add = 4
  if(demo$hisp == 'nonhispanic') hisp_mult = 0
  else if(demo$hisp == 'hispanic') hisp_mult = 1:23
  pop_x = c()
  for(mult in hisp_mult){
    pop_x = append(pop_x, 4 + (mult)*6 + marst_add)
  }
  if(demo$year > 2011){
    pop_x = pop_x + 1
    app1 = pop_x + 145
    app2 = pop_x + 290
    pop_x = append(pop_x, app1)
    pop_x = append(pop_x, app2)
  }
  pop_y = 11 + 301*race_mult + 3*demo$age + sex_add + 2717*(demo$year-2009)
  if(demo$year > 2011){
    pop_y = pop_y + 2 - 2717*(demo$year-2009) + 2719*(demo$year-2012)
  }
  pop = 0
  for(x in pop_x){
    for(y in pop_y){
      if(demo$year <= 2011){
        pop = pop + as.numeric(census_early[y,x])
      }
      else{
        pop = pop + as.numeric(census_late[y,x])
      }
    }
  }
  return(pop)
}

for(i in 1:nrow(population)){
  print(i)
  population[i,]$pop = find_pop(population[i,])
}

nrow(population)

population[1,]$hisp

us_pop_size <- read.csv("C:\\rProjects\\suicide\\US_Suicide_Compiler\\us_pop_size.csv")
colnames(us_pop_size)[1] <- c("year")
census_pop_size <- data.frame(matrix(0,nrow=11,ncol=2))
colnames(census_pop_size) <- c("year", "cens_size")
census_pop_size$year <- 2009:2019
for (i in 1:11){
  year_int = 2009 + i - 1
  census_pop_size$cens_size[i] = sum(filter(population, year == year_int)$pop)
}

us_pop_size$multiplier = us_pop_size$pop_size / census_pop_size$cens_size

population$true = population$pop * us_pop_size$multiplier[population$year%%2008]

write.csv(population, "C:\\rProjects\\suicide\\suicides2.0\\pop_param.csv")


