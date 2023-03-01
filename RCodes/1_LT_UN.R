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
library(rio)

# changing some characteristics of the environment

options(scipen=999)
getOption('timeout')
options(timeout=100)

# Loading useful functions into environment
source(here("Gender_health","Rcodes","0_Functions.R"))


# creating directory folders where outputs are saved

lt.folder  <- here("Gender_health","Data","Life_Tables","LT_UN")

# make in-out directories

dir.create(lt.folder , showWarnings = FALSE, recursive = TRUE)


# -----------------------------------------------------------------------------------------------------#
# Life tables from UN - exception is England, where we use the official ONS estimates.
# download here: https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/
# lifeexpectancies/datasets/nationallifetablesenglandreferencetables/current/nationallifetables3yreng.xlsx
# we download here directly, unzip and reload. In this way, we can keep it up-to-date
# -----------------------------------------------------------------------------------------------------#


#women - reading and saving locally life tables from UN
download.file(url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Life_Table_Complete_Medium_Female_1950-2021.zip",
              destfile = file.path(lt.folder, "lt_un_f.zip"))

#men - reading and saving  locally life tables from UN
download.file(url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Life_Table_Complete_Medium_Male_1950-2021.zip",
              destfile = file.path(lt.folder, "lt_un_m.zip")) 

#women - unzip, read and load
lt_un_f <- fread(unzip(here(lt.folder,"lt_un_f.zip"), files = "WPP2022_Life_Table_Complete_Medium_Female_1950-2021.csv"))

#men - unzip, read and load
lt_un_m <- fread(unzip(here(lt.folder,"lt_un_m.zip"), files = "WPP2022_Life_Table_Complete_Medium_Male_1950-2021.csv"))

# both for England, ONS - downloaded complete file. But saved a simplified version in the folder (LT_England). Check.

download.file(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandreferencetables/current/nationallifetables3yreng.xlsx",
               destfile = file.path(lt.folder, "lt_ons_b.xlsx"), mode="wb")

# setting variable for countries of interest 

countries<-c("China","India","Mexico","United States of America", "Republic of Korea",
             "Austria","Belgium","Bulgaria","Switzerland","Czechia",
              "Germany","Denmark","Spain","Estonia","France","Greece",
               "Croatia","Israel","Italy","Korea","Luxembourg","Latvia",
               "Poland","Portugal","Slovakia","Slovenia","Sweden", "Europe")


#women - select only countries of interest and year 2015
lt_un_f<- lt_un_f%>% 
  filter(Location %in% countries & Time%in% 2015) %>% 
  select(c("Location","Time","Sex","AgeGrpStart","mx","qx","px","lx","dx","Lx","Tx","ex","ax"))

#men - select only countries of interest and year 2015
lt_un_m<- lt_un_m%>% 
  filter(Location %in% countries & Time%in% 2015) %>% 
  select(c("Location","Time","Sex","AgeGrpStart","mx","qx","px","lx","dx","Lx","Tx","ex","ax"))

lt_un<-rbind(lt_un_f, lt_un_m)


# These life tables are single years of age. In addition, they end at age 100 and we need age 80+
# So, first closing the life table at age 80+ and then Applying function to make it abridged.
# here we can change the closing of the life table as we prefer later


lt_un_80<-lt_un %>%
  group_by(Location,Sex) %>%
  mutate(deaths=case_when(AgeGrpStart>=80~sum(dx[AgeGrpStart>=80]),TRUE~dx),
         exposure=case_when(AgeGrpStart>=80~sum(Lx[AgeGrpStart>=80]),TRUE~Lx),
         nmx=deaths/exposure)%>%
  filter(AgeGrpStart<81) %>%
  ungroup() %>%
  arrange(Location,Time,AgeGrpStart) %>%
  group_by(Location,Time,Sex) %>%
  group_modify(~ life.table(.x$nmx), .keep=T) %>%
  mutate(Age=0:80) %>%
  relocate(Age, .after = Time) %>%
  rename(Country=Location) %>% 
  as.data.frame()


lt_un_abridged<-lt_un_80%>%
  group_by(Country,Sex) %>%
  group_modify(~lt_single2abridged(lx=.x$lx, nLx=.x$Lx, ex=.x$ex, Age = 1:length(.x$lx) - 1)) 

saveRDS(lt_un_abridged, file = file.path(lt.folder, "lt_un_abridged.rds"))
write.table(lt_un_abridged, here(lt.folder, "lt_un_abridged.csv"), sep=",",row.names = F)


# Life table England comes from ONS - here we do twice the operation of life table construction as they offer restricted
# functions to later create an abridged life table. So we first compute a complete life table only from their mortality
# rates until age 100, later close the life table at age 90 and finally transform into abridged life tables.

lt_en<- fread(here("Gender_health","Data","Life_Tables","LT_UN","LT_England.csv")) %>% 
  group_by(sex) %>%
  group_modify(~ life.table(.x$mx), .keep=T) %>%
  mutate(Age=0:100) %>%
  relocate(Age, .before = sex) %>%
  #select(1:4,8,10,12) %>%
  #  rename(Country=country,Sex=sex) %>% 
  as.data.frame()

# closing at age 80

lt_en_80<-lt_en %>%
  group_by(sex) %>%
  mutate(deaths=case_when(Age>=80~sum(dx[Age>=80]),TRUE~dx),
         exposure=case_when(Age>=80~sum(Lx[Age>=80]),TRUE~Lx),
         #   lx=case_when(Age>=90~sum(lx[Age>=90]),TRUE~lx),
         nmx=deaths/exposure)%>%
  filter(Age<81) %>%
  ungroup() %>%
  arrange(Age) %>%
  group_by(sex) %>%
  group_modify(~ life.table(.x$nmx), .keep=T) %>%
  mutate(Age=0:80) %>%
  relocate(Age, .before = sex) %>%
  #select(1:4,8,10,12) %>%
  rename(Sex=sex) %>% 
  as.data.frame()

# transforming into abridged life table

lt_eng_abridged<-lt_en_80%>%
  group_by(Sex) %>%
  group_modify(~lt_single2abridged(lx=.x$lx, nLx=.x$Lx, ex=.x$ex, Age = 1:length(.x$lx) - 1)) 


saveRDS(lt_eng_abridged, file = file.path(lt.folder, "lt_eng_abridged.rds"))
write.table(lt_eng_abridged, here(lt.folder, "lt_eng_abridged.csv"), sep=",",row.names = F)



# closing at age 90

# Life table for whole Europe from Eurostat alternatively - pooled analysis

#lt_eu<- fread(here("Gender_health","Rcodes","LT","lt_eu.csv")) %>% 
#  pivot_wider( names_from = "INDIC_DE", values_from = "Value" ) %>%
#  mutate(mx=as.numeric(`Age specific death rate (Mx)`),
#         qx=as.numeric(`Probability of dying between exact ages (qx)`),) %>% 
#  group_by(SEX) %>%
#  group_modify(~ lt_single_qx(.x$qx, OAnew =90, extrapLaw = "Kannisto_Makeham"), .keep=T)%>%
#  rename(Sex=SEX) %>% 
#  as.data.frame()

# transforming into abridged 
#lt_eu_abridged<-lt_eu %>% 
#  group_by(Sex) %>% 
#  group_modify(~ lt_single2abridged(lx=.x$lx, nLx=.x$nLx, ex=.x$ex, Age = 1:length(.x$lx) - 1)) %>% 
#  as.data.frame()

#saving

#write.table(lt_eu_abridged, here(lt.folder, "lt_eu_abridged.csv"), sep=",",row.names = F)



