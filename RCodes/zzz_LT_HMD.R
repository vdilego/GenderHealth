# -----------------------------------------------------------------------------------------------------#
# Codes for replicating the paper Gender Disparities in Healthy Aging: A Cross-National Comparison
# author: Vanessa di Lego, Marília Nepomuceno, Cássio M. Turra
# Wittgenstein Centre for Demography and Global Human Capital(IIASA/OeAW/UniWien)
# Vienna Institute of Demography at the Austrian Academy of Sciences
# -----------------------------------------------------------------------------------------------------#

## cleaning the workspace
rm(list=ls(all=TRUE))

# loading necessary packages
library(MortalityLaws)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(purrr)
library(styler)
library(forcats)
library(broom)
library(here)
library(ggpubr)
library(ggExtra)
library(ggthemes)
library(ggrepel)
library(Hmisc)
#install_github("alburezg/suffrager")   # this is for using the suffragette palette : )
library(suffrager)
library(devtools)
library(forcats)
library(wesanderson)
library(ggthemes)
library(colorspace)
library(cowplot)
library(grid)
library(DemoDecomp) 
library(gridExtra)
library(DemoTools)
library(DemoToolsData)
require(openxlsx)
library(readxl)
# Loading useful functions into environment
source(here("Gender_health","Rcodes","0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved

lt.folder  <- here("Gender_health","Rcodes","LT")
# make in-out directories

dir.create(lt.folder , showWarnings = FALSE, recursive = TRUE)

# Life tables from the Human Mortality Database, for European Countries and USA

countries<-c("AUT","BEL","BGR","CZE","DEUTNP","DNK","EST","GRC","ESP","FRATNP",
             "HRV", "ITA","LVA","POL","SWE","SVN","SVK","KOR","CHE","ISR","LUX","PRT","USA")
# substitute the values in lx after age 90 with small numbers to be able to compute things
# because in the comparison we have to compute the logs of differences if the lx is 0 than
# values are not possible to be computed. This is a mere technical fact and does not affect
# the estimates

# woman
lt_f<- fread(here("Gender_health","Rcodes","LT","lt_fem.csv")) %>% 
  filter(Year%in%2015 & country%in% countries) %>% 
  mutate(sex="f") 
# man
lt_m<- fread(here("Gender_health","Rcodes","LT","lt_men.csv")) %>% 
  filter(Year%in%2015 & country%in% countries) %>% 
  mutate(sex="m")

lt_t<-rbind(lt_f,lt_m)

# closing the life tables at age 90

lt_90<-lt_t %>%
  group_by(country,sex) %>%
  mutate(deaths=case_when(Age>=90~sum(dx[Age>=90]),TRUE~dx),
         exposure=case_when(Age>=90~sum(Lx[Age>=90]),TRUE~Lx),
      #   lx=case_when(Age>=90~sum(lx[Age>=90]),TRUE~lx),
         nmx=deaths/exposure)%>%
  filter(Age<91) %>%
  ungroup() %>%
  arrange(country,Year,Age) %>%
  group_by(country,Year,sex) %>%
  group_modify(~ life.table(.x$nmx), .keep=T) %>%
  mutate(Age=0:90) %>%
  relocate(Age, .after = Year) %>%
  #select(1:4,8,10,12) %>%
  rename(Country=country,Sex=sex) %>% 
  as.data.frame()

# renaming countries

lt_90$Country<-as.factor(lt_90$Country)

lt_90$Country <- factor(lt_90$Country,
                        labels=c("Austria","Belgium","Bulgaria","Switzerland","Czechia",
                                 "Germany","Denmark","Spain","Estonia","France","Greece",
                                 "Croatia","Israel","Italy","Korea","Luxembourg","Latvia",
                                 "Poland","Portugal","Slovakia","Slovenia","Sweden","USA"),
                        levels=c( "AUT", "BEL","BGR","CHE","CZE","DEUTNP","DNK","ESP","EST","FRATNP","GRC",
                                  "HRV" , "ISR" ,"ITA" ,"KOR" , "LUX","LVA" ,"POL" ,"PRT","SVK","SVN","SWE","USA"))

saveRDS(lt_90, file = file.path(lt.folder, "lt_90.rds"))
write.table(lt_90, here(lt.folder, "lt_90.csv"), sep=",",row.names = F)

# now transforming the life tables into abridged ones, to match the disability profiles, that are 
# into 5 year age groups.

lt_abridged<-lt_90%>%
  group_by(Country,Sex) %>%
  group_modify(~lt_single2abridged(lx=.x$lx, nLx=.x$Lx, ex=.x$ex, Age = 1:length(.x$lx) - 1)) 


saveRDS(lt_abridged, file = file.path(lt.folder, "lt_abridged.rds"))
write.table(lt_abridged, here(lt.folder, "lt_abridged.csv"), sep=",",row.names = F)


# Lifetables from UN - For China, India and Mexico

#women - reading and saving life tables from UN
download.file(url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Life_Table_Complete_Medium_Female_1950-2021.zip",
              destfile = file.path(lt.folder, "lt_un_f.zip"))

#men - reading and saving life tables from UN
download.file(url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Life_Table_Complete_Medium_Male_1950-2021.zip",
              destfile = file.path(lt.folder, "lt_un_m.zip")) 

#women - unzip, read and loading life tables from UN
lt_un_f <- fread(unzip(here(lt.folder,"lt_un_f.zip"), files = "WPP2022_Life_Table_Complete_Medium_Female_1950-2021.csv"))

#men - unzip, read and loading life tables from UN
lt_un_m <- fread(unzip(here(lt.folder,"lt_un_m.zip"), files = "WPP2022_Life_Table_Complete_Medium_Male_1950-2021.csv"))


#women - select only countries of interest and year 2015
lt_un_f<- lt_un_f%>% 
  filter(Location %in% c("China","India","Mexico") & Time%in% 2015) %>% 
  select(c("Location","Time","Sex","AgeGrpStart","mx","qx","px","lx","dx","Lx","Tx","ex","ax"))

#men - select only countries of interest and year 2015
lt_un_m<- lt_un_m%>% 
  filter(Location %in% c("China","India","Mexico") & Time%in% 2015) %>% 
  select(c("Location","Time","Sex","AgeGrpStart","mx","qx","px","lx","dx","Lx","Tx","ex","ax"))
  

# combining both tables

lt_un<-rbind(lt_un_f,lt_un_m)


# These life tables are also single years of age. In addition, they end at age 100 and we need age 90+
# So, first closing the life table at age 90+ and then Applying function to make it abridged.


lt_un_90<-lt_un %>%
  group_by(Location,Sex) %>%
  mutate(deaths=case_when(AgeGrpStart>=90~sum(dx[AgeGrpStart>=90]),TRUE~dx),
         exposure=case_when(AgeGrpStart>=90~sum(Lx[AgeGrpStart>=90]),TRUE~Lx),
         #   lx=case_when(Age>=90~sum(lx[Age>=90]),TRUE~lx),
         nmx=deaths/exposure)%>%
  filter(AgeGrpStart<91) %>%
  ungroup() %>%
  arrange(Location,Time,AgeGrpStart) %>%
  group_by(Location,Time,Sex) %>%
  group_modify(~ life.table(.x$nmx), .keep=T) %>%
  mutate(Age=0:90) %>%
  relocate(Age, .after = Time) %>%
  #select(1:4,8,10,12) %>%
  rename(Country=Location) %>% 
  as.data.frame()



lt_un_abridged<-lt_un_90%>%
  group_by(Country,Sex) %>%
  group_modify(~lt_single2abridged(lx=.x$lx, nLx=.x$Lx, ex=.x$ex, Age = 1:length(.x$lx) - 1)) 



saveRDS(lt_un_abridged, file = file.path(lt.folder, "lt_un_abridged.rds"))
write.table(lt_un_abridged, here(lt.folder, "lt_un_abridged.csv"), sep=",",row.names = F)



# Life table England comes from ONS

lt_en<- fread(here("Gender_health","Rcodes","LT","LT_England.csv")) %>% 
  group_by(sex) %>%
  group_modify(~ life.table(.x$mx), .keep=T) %>%
  mutate(Age=0:100) %>%
  relocate(Age, .before = sex) %>%
  #select(1:4,8,10,12) %>%
#  rename(Country=country,Sex=sex) %>% 
  as.data.frame()

# closing at age 90

lt_en_90<-lt_en %>%
group_by(sex) %>%
  mutate(deaths=case_when(Age>=90~sum(dx[Age>=90]),TRUE~dx),
         exposure=case_when(Age>=90~sum(Lx[Age>=90]),TRUE~Lx),
         #   lx=case_when(Age>=90~sum(lx[Age>=90]),TRUE~lx),
         nmx=deaths/exposure)%>%
  filter(Age<91) %>%
  ungroup() %>%
  arrange(Age) %>%
  group_by(sex) %>%
  group_modify(~ life.table(.x$nmx), .keep=T) %>%
  mutate(Age=0:90) %>%
  relocate(Age, .before = sex) %>%
  #select(1:4,8,10,12) %>%
  rename(Sex=sex) %>% 
  as.data.frame()

# transforming into abridged life table

lt_eng_abridged<-lt_en_90%>%
  group_by(Sex) %>%
  group_modify(~lt_single2abridged(lx=.x$lx, nLx=.x$Lx, ex=.x$ex, Age = 1:length(.x$lx) - 1)) 


write.table(lt_eng_abridged, here(lt.folder, "lt_eng_abridged.csv"), sep=",",row.names = F)


# Life table for whole Europe - pooled analysis

lt_eu<- fread(here("Gender_health","Rcodes","LT","lt_eu.csv")) %>% 
  pivot_wider( names_from = "INDIC_DE", values_from = "Value" ) %>%
  mutate(mx=as.numeric(`Age specific death rate (Mx)`),
         qx=as.numeric(`Probability of dying between exact ages (qx)`),) %>% 
  group_by(SEX) %>%
  group_modify(~ lt_single_qx(.x$qx, OAnew =90, extrapLaw = "Kannisto_Makeham"), .keep=T)%>%
  rename(Sex=SEX) %>% 
  as.data.frame()

# transforming into abridged 
lt_eu_abridged<-lt_eu %>% 
  group_by(Sex) %>% 
  group_modify(~ lt_single2abridged(lx=.x$lx, nLx=.x$nLx, ex=.x$ex, Age = 1:length(.x$lx) - 1)) %>% 
  as.data.frame()

#saving

write.table(lt_eu_abridged, here(lt.folder, "lt_eu_abridged.csv"), sep=",",row.names = F)

  
