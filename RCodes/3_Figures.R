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


