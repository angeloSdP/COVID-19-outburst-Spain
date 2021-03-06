Merging death and population data
================
2021/03/19

## Merge population and death datasets

We have two files, one with population and other with death data
downloaded from the Spanish Statistical Office (INE). Original data have
been cleaned and organized by year, autonomous community, sex and age
group. Here we merge both datasets. For each autonomous community, age
group, sex and month, number of deaths is presented paired with the
population size in that group in the reference month for each six-months
period. For months January to June of any year, the reference population
is the one that the INE calculates for January of that year, and for
July to December the reference population is the calculated for July.

``` r
load("rdata/population7120.rdata") # Population data from 1971 to 2020
load("rdata/deaths80_19.rdata")    # Death data from 1980 to 2019
load("rdata/deaths2020.rdata")    # Death data from 2020

# Death data for 2020 are provisional and are provided by the INE as weekly counts.

# Dataset deaths2020A1 contains provisional data for 2020 with monthly number of 
# deaths calculated from weekly deaths by assigning all data of a week to the month 
# in which that week has more days:
deathPop8020A1 <- bind_rows(deaths80_19,deaths2020A1) %>% 
  mutate(mesReferencia=ifelse(mes<=6,1,7)) %>% 
  left_join(population7120)

# Dataset deaths2020A2 contains provisional data for 2020 with monthly number of 
# deaths calculated from weekly deaths, assigning to each month a number of deaths
# proportional to the number of days that the week has in that month.
deathPop8020A2 <- bind_rows(deaths80_19,deaths2020A2)  %>% 
  mutate(mesReferencia=ifelse(mes<=6,1,7)) %>% 
  left_join(population7120)

save(deathPop8020A1,deathPop8020A2,file="rdata/deathPop8020.rdata")
```

## Mortality during the period March 1st - June 30 from 2008 to 2020

For our analysis we use data of the four-months periods from March 1st -
June 30 in the years 2008 to 2020.

The following code extracts data of those periods and rename variables.
Until now we have used Spanish names for the variables as they came in
the INE files, and here we simply translate them into English:

``` r
dbPop <- population7120 %>% 
  filter(a??o>=2008&mesReferencia==1) %>% 
  select(a??o,ca,codigoCA,sexo,grEdad,poblacion)

# Dataset deaths2020A1 contains provisional data for 2020 with monthly number of 
# deaths calculated from weekly deaths by assigning all data of a week to the month 
# in which that week has more days:
dbDeathA1 <- bind_rows(deaths80_19,deaths2020A1) %>% 
  filter(a??o>=2008&mes>=3&mes<7) %>% 
  group_by(a??o,ca,codigoCA,sexo,grEdad) %>% 
  summarize(numDefun=sum(numDefun)) %>% 
  ungroup()

db <- full_join(dbPop,dbDeathA1) %>% 
  mutate(poblacion=round(poblacion)) %>% 
  rename(Year=a??o,CA=codigoCA,death=numDefun,pop=poblacion) %>% 
  mutate(Age=factor(grEdad),
         Male=ifelse(sexo=="Hombres",1,0)) %>% 
  select(-c(ca,sexo,grEdad)) %>% 
  select(Year,CA,Male,Age,death,pop) %>% 
  arrange(Year,CA,Male,Age)

# Dataset deaths80_20A2 contains provisional data for 2020 with monthly number of 
# deaths calculated from weekly deaths, assigning to each month a number of deaths
# proportional to the number of days that the week has in that month.
dbDeathA2 <- bind_rows(deaths80_19,deaths2020A2) %>% 
  filter(a??o>=2008&mes>=3&mes<7) %>% 
  group_by(a??o,ca,codigoCA,sexo,grEdad) %>% 
  summarize(numDefun=sum(numDefun)) %>% 
  ungroup()

dbA2 <- full_join(dbPop,dbDeathA2) %>% 
  mutate(poblacion=round(poblacion)) %>% 
  rename(Year=a??o,CA=codigoCA,death=numDefun,pop=poblacion) %>% 
  mutate(Age=factor(grEdad),
         Male=ifelse(sexo=="Hombres",1,0)) %>% 
  select(-c(ca,sexo,grEdad)) %>% 
  select(Year,CA,Male,Age,death,pop) %>% 
  arrange(Year,CA,Male,Age)

save(db,file="rdata/db.rdata")
```

Although the only difference between datasets dbA1 and dbA2 is the way
in which deaths have been assigned to months from the weekly data
provided by the INE, the total number of deaths in the first six months
of 2020 is 261804 people in the file dbA1 and 261562 people in dbA2
(difference of 242 people). In a [latest press release published in
2021/01/26 (in spanish)](https://www.ine.es/prensa/mnp_1s2020_p.pdf) the
INE estimates in 262373 the total number of deaths in the first semester
of 2020. For that reason we have used our first approximation to 2020
monthly data, as it gives a value closer to that provided by the INE.
