# -----------------------------------------------------------------------------------------------------#
# Codes for replicating the paper Gender Disparities in Healthy Aging: A Cross-National Comparison
# author: Vanessa di Lego, Marília Nepomuceno, Cássio M. Turra
# Wittgenstein Centre for Demography and Global Human Capital(IIASA/OeAW/UniWien)
# Vienna Institute of Demography at the Austrian Academy of Sciences
# -----------------------------------------------------------------------------------------------------#


# Calculation and decomposition of DFLE


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
library(ggthemes)
library(colorspace)
library(cowplot)
library(grid)
library(DemoDecomp) 
library(gridExtra)
library(forcats)


# Loading useful functions into environment
source(here("Rcodes","0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved
figs.folder <- here("Manuscript","Figures")
dat.folder <- here("Manuscript","Data")
figs.app.folder <- here("Appendix","Figures")


#dir.create(figs.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(dat.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(figs.app.folder, showWarnings = FALSE, recursive = TRUE)

# First setting the folders

dis.folder <- here("Data","All_Prevalence") 


# reading disability data for all countries at the same time
dis<-fread(here(dis.folder,"all_prev_adl.csv")) %>%  
  rename(sex=gender) %>% 
  filter(age>=50 )  %>% 
  mutate(type="ADL")%>% 
  select(-2)

cron<-fread(here(dis.folder,"all_prev_cron.csv")) %>%  
  rename(sex=gender) %>% 
  filter(age>=50 ) %>% 
  mutate(type="Chronic") %>% 
  select(-2)

dis_cron<-full_join(dis,cron)


fig_all_conditions<-
  ggplot(dis_cron,
         aes(age, country, group=country, fill=unhealthy))+
  geom_raster(interpolate = F) +
  #  geom_tile (color="grey80") +
  theme_clean(base_size = 14)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=10),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 90))+
  facet_grid(sex~type)+
  scale_fill_distiller(palette = "Spectral", name="%Unhealthy", n.breaks=4)


pdf(here("Gender_health", "Countries","EUROPE","Descriptive","fig_prev_cron_country.pdf"), width = 18, height=8)
fig_all_conditions
dev.off()


# directories of each survey.
prev.dir.share<-here("Data","SHARE","Prevalence") 
prev.dir.elsa<-here("Data","ELSA","Prevalence")
prev.dir.lasi<-here("Data","LASI","Prevalence")
prev.dir.charls<-here("Data","CHARLS","Prevalence")
prev.dir.hrs<-here("Data","HRS","Prevalence")
prev.dir.klosa<-here("Data","KLOSA","Prevalence")
prev.dir.mhas<-here("Data","MHAS","Prevalence")

# reading data
prev.share<-fread(here(prev.dir.share,"prev_share_cron_type_pooled_agecat.csv")) %>% 
  mutate(country="Europe") %>% 
  select(-year)
prev.share.cntry<-fread(here(prev.dir.share,"prev_share_cron_type_country_agecat.csv"))%>% 
  select(-year)

prev.elsa<-fread(here(prev.dir.elsa,"prev_elsa_cron_type_agecat.csv")) %>% 
  mutate(country="England")%>% 
  select(-year)

prev.lasi<-fread(here(prev.dir.lasi,"prev_lasi_cron_type_agecat.csv")) %>% 
  mutate(country="India")%>% 
  select(-year)

prev.charls<-fread(here(prev.dir.charls,"prev_charls_cron_type_agecat.csv")) %>% 
  mutate(country="China")%>% 
  select(-year)

prev.hrs<-fread(here(prev.dir.hrs,"prev_hrs_cron_type_agecat.csv")) %>% 
  mutate(country="US")%>% 
  select(-year)

prev.klosa<-fread(here(prev.dir.klosa,"prev_klosa_cron_type_agecat.csv")) %>% 
  mutate(country="Korea")%>% 
  select(-year)

prev.mhas<-fread(here(prev.dir.mhas,"prev_mhas_cron_type_agecat.csv")) %>% 
  mutate(country="Mexico")%>% 
  select(-year)


prev.all<-rbind(prev.share.cntry,prev.share, prev.elsa,prev.lasi,prev.charls,prev.hrs,
                prev.klosa,prev.mhas)

# plots with all

ggplot()+
  
  geom_line(data=prev.all,aes(age, unhealthy, group=country,color=country),size=1, color="grey90")+
  # geom_line(data=prev.all, aes(age, unhealthy, group=gender,color=gender))+
  # geom_point(size=2.7, alpha=0.7)+
  theme_clean(base_size = 14)+
  facet_grid(gender~type)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  geom_line(data=prev.all,aes(age, unhealthy, group=country,color=gender),size=1)+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 

X11()


# Helper function for string wrapping because names are too large in the facets
# Default 20 character target width.
swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)

prev.all$type = swr(prev.all$type)




fig_prev_all<-ggplot(prev.all,
                     aes(age, country, group=country, fill=unhealthy))+
  geom_raster(interpolate = F) +
  # geom_tile (color="white") +
  theme_clean(base_size = 18)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=10),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 90))+
  facet_grid(gender~type)+
  scale_fill_distiller(palette = "Spectral", 
                       name="%Unhealthy", n.breaks=4)

pdf(here("Gender_health", "Manuscript","Figures","fig_prev_all.pdf"), width = 18, height=8)
fig_prev_all
dev.off()
