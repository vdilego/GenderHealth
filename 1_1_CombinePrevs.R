# -----------------------------------------------------------------------------------------------------#
# Codes for replicating the paper Going Beyong the Gender Gap
# authors: Vanessa di Lego, Marília Nepomuceno, Cássio M. Turra
# Wittgenstein Centre for Demography and Global Human Capital(IIASA/OeAW/UniWien)
# Vienna Institute of Demography at the Austrian Academy of Sciences
# Max Planck Institute for Demographic Research
# Federal University of Minas Gerais (Cedeplar)
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

# reading and combining all prevalences of ADLs and Chronic conditions into one file

#China

charls_dis<-fread(here("Data","CHARLS","Prevalence","prev_charls_adl_agecat.csv")) %>% 
  mutate(country= "China")

charls_cron<-fread(here("Data","CHARLS","Prevalence","prev_charls_cron_agecat.csv"))  %>% 
  mutate(country= "China")

#USA

hrs_dis<-fread(here("Data","HRS","Prevalence","prev_hrs_adl_agecat.csv")) %>% 
  mutate(country="US")

hrs_cron<-fread(here("Data","HRS","Prevalence","prev_hrs_cron_agecat.csv")) %>% 
  mutate(country="US")

#Korea

klosa_dis<-fread(here("Data","KLOSA","Prevalence","prev_klosa_adl_agecat.csv")) %>% 
  mutate(country="Korea")

klosa_cron<-fread(here("Data","KLOSA","Prevalence","prev_klosa_cron_agecat.csv")) %>% 
  mutate(country="Korea")

#England

elsa_dis<-fread(here("Data","ELSA","Prevalence","prev_elsa_adl_agecat.csv")) %>% 
  mutate(country="England")

elsa_cron<-fread(here("Data","ELSA","Prevalence","prev_elsa_cron_agecat.csv")) %>% 
  mutate(country="England")

#India
lasi_dis<-fread(here("Data","LASI","Prevalence","prev_lasi_adl_agecat.csv")) %>% 
  mutate(country="India")

lasi_cron<-fread(here("Data","LASI","Prevalence","prev_lasi_cron_agecat.csv")) %>% 
  mutate(country="India")

#Mexico

mhas_dis<-fread(here("Data","MHAS","Prevalence","prev_mhas_adl_agecat.csv")) %>% 
  mutate(country="Mexico") %>% 
  select(-10)

mhas_cron<-fread(here("Data","MHAS","Prevalence","prev_mhas_cron_agecat.csv")) %>% 
  mutate(country="Mexico")%>% 
  select(-10)



#Europe

#all countries and then pooled countries

share_dis_c<-fread(here("Data","SHARE","Prevalence","prev_share_country_agecat.csv"))

share_cron_c<-fread(here("Data","SHARE","Prevalence","prev_share_cron_country_agecat.csv"))



share_dis_p<-fread(here("Data","SHARE","Prevalence","prev_share_pooled_agecat.csv")) %>% 
  mutate(country="Europe")

share_cron_p<-fread(here("Data","SHARE","Prevalence","prev_share_cron_pooled_agecat.csv")) %>% 
  mutate(country="Europe")


share_dis_all<- full_join(share_dis_c,share_dis_p)
share_cron_all<- full_join(share_cron_c,share_cron_p)



# joining all countries disability

all_prev_adl<-rbind(charls_dis,hrs_dis,elsa_dis,klosa_dis,lasi_dis,share_dis_all, mhas_dis)

write.table(all_prev_adl, here("Data","All_Prevalence", "all_prev_adl.csv"),
            sep = ",",row.names = F) 
# joining all countries cron

all_prev_cron<-rbind(charls_cron,hrs_cron,elsa_cron,klosa_cron,lasi_cron,share_cron_all, mhas_cron)

write.table(all_prev_cron, here("Data","All_Prevalence", "all_prev_cron.csv"),
            sep = ",",row.names = F) 

