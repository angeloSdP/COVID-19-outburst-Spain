Spain Population Data Download
================
2021/03/19

 

This file implements the code necessary for downloading the population
data of Spain during the years 2008-2019 grouped by sex, age and
autonomous community.

 

# Data source

Population data of Spain can be downloaded from the [Spanish Statistical
Office INE (*Instituto Nacional de
Estadística*)](https://www.ine.es/en/) in the section [Demography and
population &gt; Population figures and Demographic Censuses &gt;
Population
figures](https://www.ine.es/dyngs/INEbase/en/operacion.htm?c=Estadistica_C&cid=1254736176951&menu=ultiDatos&idp=1254735572981).
As stated in this web page:

> > The **Population Figures** provide a quantitative measurement of the
> > population resident in Spain, in each Autonomous Community, in each
> > province and on each island (in the island province), broken down
> > according to basic demographic characteristics, such as sex, year of
> > birth, age, nationality and country of birth. The population series
> > is obtained from the intercensus population estimates for the
> > 1971-2012 period, and from the Population Figures operation from
> > 2012 onward.

> > This data is used as reference population figures in all of the
> > statistical operations of the INE (surveys, National Accounts,
> > indicators, etc.) and is transmitted on an international level as
> > the official population data for Spain, for all intents and
> > purposes.

 

Data can be downloaded in several formats (excel, csv, PC-Axis, Json or
plain text) in the section
[Results](https://www.ine.es/dyngs/INEbase/en/operacion.htm?c=Estadistica_C&cid=1254736176951&menu=resultados&idp=1254735572981)
of this page. We will use the R library `pxR` that allows us to directly
download data from the web in pc-Axis format, which is an INE’s own
format.

 

# Population data downloading and tidying

Direct access to data in Pc-Axis format is through the link
<https://www.ine.es/jaxiT3/files/t/es/px/10262.px>:

``` r
data <- pxR::read.px("https://www.ine.es/jaxiT3/files/t/es/px/10262.px")
```

The following code reads the data and prepares it in a tidy format for
further processing with R. For each year we have the population size by
autonomous community, sex and age group, evaluated in the months of
January and July:

``` r
# Codes and names of Autonomous Communities in several languages
cacodes <- readxl::read_excel("CCAA codes.xlsx") %>% 
  select(codigoCA,ca=CA_INE)

# Resident population
population7120 <- data %>% 
  as.data.frame() %>% 
  filter(Comunidades.y.ciudades.autónomas!="Total Nacional", Edad!="Total",
         Sexo!="Ambos sexos") %>% 
  mutate(edad=gsub("[a-z ñ]","",Edad),
         edad=gsub("100á","100",edad),
         edad=gsub("85á","-1",edad),
         edad=as.numeric(edad)) %>% 
  filter(edad<85) %>% 
  rename(cca=Comunidades.y.ciudades.autónomas,
         sexo=Sexo,poblacion=value, periodo=Periodo) %>% 
  mutate(cca=as.character(cca),
         sexo=as.character(sexo),
         periodo=as.Date(periodo, format="%d de %b de %Y"),
         Edad=as.character(Edad),
         edad=ifelse(edad==-1,85,edad),
         grEdad=ifelse(edad<=50,"edad 0-49",
                       ifelse(edad<=64,"edad 50-64",
                              ifelse(edad<=74,"edad 65-74","edad 75 o más"))),
         año=lubridate::year(periodo),
         mesReferencia=lubridate::month(periodo),
         codigoCA=as.numeric(substr(cca,1,2))) %>% 
  full_join(cacodes) %>% 
  select(-c(cca)) %>% 
  group_by(año,periodo,mesReferencia,codigoCA,ca,sexo,grEdad) %>% 
  summarize(poblacion=sum(poblacion)) %>% 
  ungroup()

save(population7120,file="rdata/population7120.rdata")
```
