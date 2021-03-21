Spain Mortality Data Download
================
2021/03/19

 

This file implements the code necessary for downloading mortality data
of Spain during the years 2008-2019 grouped by sex, age and autonomous
community.

 

# Data source

## Mortality data until 2019

Mortality data of Spain can be downloaded from the [Spanish Statistical
Office INE (*Instituto Nacional de
Estadística*)](https://www.ine.es/en/) in the section [Demography and
population &gt; Demographic Phenomena &gt; Death Statistics. Vital
statistics.
Results](https://www.ine.es/dyngs/INEbase/en/operacion.htm?c=Estadistica_C&cid=1254736177008&menu=resultados&idp=1254735573002).
As stated in this web page:

> > Death Statistics have been prepared since 1858 and constitute one of
> > the most traditional works in the INE. It collects data on deaths
> > occurring in Spain, as well as the socio-demographic characteristics
> > of the deceased.

INE does not offer the possibility of directly download tables with the
number of deaths by autonomous community, sex and age. To this aim it is
necessary to download microdata files and build necessary tables from
them.

Microdata files can be downloaded from [this
link](https://www.ine.es/dyngs/INEbase/en/operacion.htm?c=Estadistica_C&cid=1254736177008&menu=resultados&idp=1254735573002#!tabs-1254736195450).
There is a microdata file per year. The structure of the data in each
file depends on the year. Register design and valid variable values are
specified in excel files which can be downloaded by ftp from that page
in the following links:

-   Years 1980-1998:
    <ftp://www.ine.es/temas/mnp_defun/diseno_defun_80-98.zip>

-   Years 1999-2008:
    <ftp://www.ine.es/temas/mnp_defun/diseno_defun_99.zip>

-   Years 2009-2011:
    <ftp://www.ine.es/temas/mnp_defun/disreg_defun15.zip>

-   Years 2012 onwards:
    <ftp://www.ine.es/temas/mnp_defun/disreg_defunedu.zip>

 

We have extracted the relevant information of these files in the
following files which can be found in this repository: `dise12_19.xlsx`,
`dise09_11.xlsx`, `dise99_08.xlsx`, `dise80_98.xlsx`.

 

The following code allows for direct download of the microdata files via
ftp from the INE:

``` r
require(RCurl)
for (k in 12:19) download.file(paste("ftp://www.ine.es/temas/mnp_defun/datos_20",k,".zip",sep=""), 
                             destfile = paste("microdata/datos_20",k,".zip",sep=""))
for (k in 10:11) download.file(paste("ftp://www.ine.es/temas/mnp_defun/sin/datos_20",k,".zip",sep=""), 
                             destfile = paste("microdata/datos_20",k,".zip",sep=""))
download.file(paste("ftp://www.ine.es/temas/mnp_defun/sin/datos_2009.zip",sep=""), 
                             destfile = paste("microdata/datos_2009.zip",sep=""))
for (k in 0:8) download.file(paste("ftp://www.ine.es/temas/mnp_defun/datos defuncio0",k,".zip",sep=""), 
                             destfile = paste("microdata/datos_200",k,".zip",sep=""))
for (k in 80:99) download.file(paste("ftp://www.ine.es/temas/mnp_defun/datos defuncio",k,".zip",sep=""), 
                             destfile = paste("microdata/datos_19",k,".zip",sep=""))
```

 

Now we read the register design files and define a function which uses
the adequate register file for reading mortality data in each one of the
previous zip files:

``` r
dise12_19 <- read_excel("microdata/dise12_19.xlsx") %>% 
  filter(!is.na(Variable)) %>% 
  filter(!is.na(Inic.))

dise09_11 <- read_excel("microdata/dise09_11.xlsx") %>% 
  filter(!is.na(Variable)) %>% 
  filter(!is.na(Inic.))

dise99_08 <- read_excel("microdata/dise99_08.xlsx") %>% 
  filter(!is.na(Variable)) %>% 
  filter(!is.na(Inic.))

dise80_98 <- read_excel("microdata/dise80_98.xlsx") %>% 
  filter(!is.na(Variable)) %>% 
  filter(!is.na(Inic.))

# Number of deaths are counted by province in microdata files. In this function
# we use the file `comProvs.xlsx` to match each province with the autonomous 
# community to which it belongs.

leeMicrodata <- function(año,dise){
  comuProv <- read_excel("comsProvs.xlsx") %>% 
  select(codigoCA=CODAUTO,ca=`Comunidad Autónoma`,CPRORE=CPRO)
  read_fwf(file.path("microdata",paste("datos_",año,".zip",sep="")),
           fwf_widths(dise$Long.,dise$Variable),col_types = cols(.default="n")) %>% 
    select(CPRORE, sexo=SEXO,mes=MESDEF,edad=AÑOSC) %>% 
    full_join(comuProv) %>% 
    mutate(ca=ifelse(is.na(CPRORE)|CPRORE>52,"Extranjero",ca)) %>% 
    mutate(sexo=factor(sexo,levels=c(1,6),labels=c("Hombres","Mujeres")),
           grEdad=ifelse(edad<=50,"edad 0-49",
                       ifelse(edad<=64,"edad 50-64",
                              ifelse(edad<=74,"edad 65-74","edad 75 o más")))) %>% 
    group_by(mes,codigoCA,ca,sexo,grEdad) %>% 
    summarize(numDefun=n()) %>% 
    ungroup() %>% 
    mutate(año=año) %>% 
    select(año,everything())
}
```

Finally we use the previous register files and function for reading
microdata files and merge the data in a single data.frame which accounts
for the mortality by year, autonomous community, age group and sex.

``` r
defun <- NULL
for (a in 2019:2012) defun <- bind_rows(defun,leeMicrodata(a,dise12_19))
for (a in 2011:2009) defun <- bind_rows(defun,leeMicrodata(a,dise09_11))
for (a in 2008:1999) defun <- bind_rows(defun,leeMicrodata(a,dise99_08))
for (a in 1998:1980) defun <- bind_rows(defun,leeMicrodata(a,dise80_98))

deaths80_19 <- defun %>% 
  filter(ca!="Extranjero") %>% 
  mutate(ca=if_else(ca=="Castilla-La Mancha","Castilla - La Mancha",ca), 
         sexo=as.character(sexo))
  
save(deaths80_19,file="rdata/deaths80_19.rdata")
```

 

 

## Mortality data in 2020

The process of obtaining mortality data is complex, so the official
values are normally published one year late. Definitive official values
of mortality in 2020 will be published in january 2022. Fortunately, the
Spanish Office of Statistics (INE) publishes provisional values in the
section [Weekly death estimates (EDeS) during the covid-19
outbreak](https://www.ine.es/en/experimental/defunciones/experimental_defunciones.htm).
As stated in this page:

> > The EDeS project aims to carry out a weekly study of deaths
> > occurring during the covid-19 pandemic, and to compare this with the
> > historical data on deaths since the year 2000. This will allow us to
> > interpret the data using a necessary historical perspective, given
> > the variability in deaths over time.

Weekly and accumulated deaths by sex and autonomous community in
2019-2021 can be downloaded from [this
link](https://www.ine.es/jaxiT3/Tabla.htm?t=35179) in that web page. The
following code downloads the data of year 2020 directly to a data.frame:

``` r
# First create a data.frame with the age groups that allows for recoding ages in the mortality data file
edades <- tibble(e1=seq(0,90,by=5), e2=seq(4,94,by=5)) %>% 
  mutate(claseEdad=(e1+e2)/2,
         edad=paste("De",e1,"a",e2,"años"),
         edad=ifelse(claseEdad==92,"90 y más años",edad),
         grEdad=ifelse(claseEdad<=47,"edad 0-49",
                       ifelse(claseEdad<=62,"edad 50-64",
                              ifelse(claseEdad<=72,"edad 65-74","edad 75 o más")))) %>%  
  select(claseEdad,edad,grEdad)

# Read the codes and names of the autonomous communities
cacodes <- readxl::read_excel("CCAA codes.xlsx") %>% 
  select(codigoCA,ca=CA_INE)

# Download and read week mortality data from 2019:
deathData <- pxR::read.px("https://www.ine.es/jaxiT3/files/t/es/px/35179.px?nocab=1") %>% 
  as.data.frame

# Conversion of data to a tidy tibble with age group and autonomous community; for 
# each ISO week dates of monday, sunday and thursday are computed.
weekly1921Deaths <- deathData %>% 
  filter(Tipo.de.dato=="Dato base",Sexo!="Total",
           Edad..grupos.quinquenales.!="Todas las edades",
           Comunidades.autónomas!="Total Nacional") %>% 
  select(periodo=Periodo, edad=Edad..grupos.quinquenales., sexo=Sexo,
         ca=Comunidades.autónomas,numDefun=value) %>% 
  mutate(numDefun=ifelse(is.na(numDefun),0,numDefun),
         ca=toupper(ca)) %>% 
  filter(!(edad=="No consta"&numDefun==0)) %>% 
  mutate(codigoCA=as.numeric(substr(ca,1,2)),
         edad = as.character(edad),
         sexo=as.character(sexo)) %>% 
  select(periodo,edad,sexo,codigoCA,numDefun) %>% 
  full_join(cacodes) %>% 
  full_join(edades) %>% 
  group_by(periodo,codigoCA,ca,sexo,grEdad) %>%   
  summarize(numDefun=sum(numDefun)) %>% 
  filter(!is.na(grEdad)) %>% 
  mutate(date1=ISOweek::ISOweek2date(paste(gsub("SM","-W",periodo),1,sep="-")),
         date7=ISOweek::ISOweek2date(paste(gsub("SM","-W",periodo),7,sep="-")),
         date4=ISOweek::ISOweek2date(paste(gsub("SM","-W",periodo),4,sep="-"))) %>% 
  ungroup() %>% 
  arrange(desc(periodo))
  
# Group data by month. As INE does not provide data by month, we can follow two 
# approximations: assign number of deaths in a week to the month in which the 
# week has more days, or assign number of deaths to each month proportionally to 
# the number of days of each week into that month.

# First filter deaths of the year 2020. Take into account that ISO weeks are
# numbered according to the month in which Thursday falls
week2020Deaths <- weekly1921Deaths %>% 
  filter(year(date4)==2020) 

# Approx 1: each week is assigned to the month in which there are more days:
deaths2020A1 <- week2020Deaths %>%
  mutate(año=2020,
         mes=lubridate::month(date4)) %>% 
  group_by(año,mes,codigoCA,ca,sexo,grEdad) %>% 
  summarize(numDefun=sum(numDefun)) %>% 
  ungroup() 

# Approx 2: Assign number of deaths proportionally to the number of days of
# month that fall inside each week

# The following function computes how many days of the same week fall in two
# different months and distribute the number of deaths proportionally:

ndd <- function(date1,date7,numDefun,...){
  dia <- seq.Date(date1,date7,by=1)
  mes <- lubridate::month(dia)
  año <- lubridate::year(dia)
  n <- numDefun/7
  df <- data.frame(dia,mes,año,n) %>% 
    group_by(año,mes) %>% 
    summarize(nDef=sum(n),.groups = 'drop')
  bind_cols(...,df)
}

# Apply this function to weekly mortality data
deaths2020A2 <- pmap_dfr(week2020Deaths,ndd)
deaths2020A2 <- deaths2020A2 %>% 
  filter(año==2020) %>% 
  group_by(año,mes,codigoCA,ca,sexo,grEdad) %>% 
  summarize(numDefun=round(sum(nDef))) %>% 
  ungroup()

save(deaths2020A1,deaths2020A2, file="rdata/deaths2020.rdata")
save(weekly1921Deaths,file="rdata/weekly1921Deaths.rdata")
```
