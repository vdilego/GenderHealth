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
  theme_clean(base_size = 26)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90))+
  facet_grid(sex~type)+
  scale_fill_distiller(palette = "Spectral", name="%Unhealthy", n.breaks=4)


pdf(here("Gender_health", "Countries","EUROPE","Descriptive","fig_prev_cron_country.pdf"), width = 18, height=8)
fig_all_conditions
dev.off()


X11()

  ggplot()+
  geom_line(data=dis_cron,
         aes(age, unhealthy, group=country, color=country), color="grey90", size=1.2)+
    
    geom_point(data=dis_cron %>% filter(country%in%"Europe"),
              aes(age, unhealthy, group=country, color=country), size=3.2)+
    geom_line(data=dis_cron %>% filter(country%in%"Europe"),
               aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
    
    geom_point(data=dis_cron %>% filter(country%in%"US"),
              aes(age, unhealthy, group=country, color=country), size=3.2)+
    geom_line(data=dis_cron %>% filter(country%in%"US"),
              aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
   
     geom_point(data=dis_cron %>% filter(country%in%"China"),
              aes(age, unhealthy, group=country, color=country), size=3.2)+
    geom_line(data=dis_cron %>% filter(country%in%"China"),
              aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
    
    geom_point(data=dis_cron %>% filter(country%in%"India"),
              aes(age, unhealthy, group=country, color=country), size=3.2)+
    geom_line(data=dis_cron %>% filter(country%in%"India"),
              aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
    
    geom_point(data=dis_cron %>% filter(country%in%"Korea"),
              aes(age, unhealthy, group=country, color=country), size=3.2)+
    geom_line(data=dis_cron %>% filter(country%in%"Korea"),
              aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
    
    geom_point(data=dis_cron %>% filter(country%in%"Mexico"),
              aes(age, unhealthy, group=country, color=country), size=3.2)+
    geom_line(data=dis_cron %>% filter(country%in%"Mexico"),
              aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
    
    geom_point(data=dis_cron %>% filter(country%in%"Portugal"),
              aes(age, unhealthy, group=country, color=country), size=3.2)+
    geom_line(data=dis_cron %>% filter(country%in%"Portugal"),
              aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
    
  theme_clean(base_size =26)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90))+
  facet_grid(sex~type)+
 #   scale_colour_grafify(palette = "muted") #not sequential colours
     scale_color_manual(values = c("dodgerblue4",
                                    "darkolivegreen4",
                                    "darkorchid3",
                                    "goldenrod1",
                                   "grey30",
                                   "firebrick","black"))
 
  
  
   ggplot() +
    geom_bar(data= dis_cron %>%
               arrange(unhealthy) %>% 
               group_by(type) %>% 
               mutate(country=fct_reorder(country, unhealthy,.desc = T)),
             aes(x=country, y=unhealthy, fill=factor(sex)),
             stat = "identity", position = "dodge")+
    #  ggtitle(bquote(~'Germany (SHARE)' ))+
    xlab("Age") +ylab(" ")+
    theme (plot.title = element_text(size = 10))+
    # geom_bar(stat = "identity", position = "stack")+ 
    #geom_errorbar(data= outgap.ci,
    #              aes(x=Country, ymin=l, ymax=u, 
    #                  color=factor(type, levels=c("Mortality","Disability"))),
    #              width=0.5, alpha=0.5, size=1.2, show.legend = F, position=position_dodge(width=0.5))+
    scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
    scale_color_manual(values=alpha(c("darkred", "blue"),0.7))+
    #scale_fill_manual(values=alpha(c( "#A50026", "#4575B4")))+
    
    # ylim(-1.2, 1.7)+
    geom_hline(yintercept=0, linetype="dashed",  color = "black", size=0.5)+
    labs(fill = "Component")+
    theme_minimal(base_size = 12) +
    # facet_wrap(.~Country, ncol = 4)+
    theme(legend.text=element_text(size=9),
          legend.title=element_text(size=10),
          axis.title =  element_text(size=12),title =  element_text(size=12),
          legend.position = "bottom", 
          legend.background = element_rect(color = NA),
          axis.text.x = element_text( vjust = 0.3, hjust = 1))+
    facet_grid(type~age)+
    coord_flip()
  
  
  library(ggalt)
  library(ggtext)
  library(extrafont)
  
# making  a dumbbell to check
  dis_cron_bell<-dis_cron %>% 
    pivot_wider(id_cols = c(age,country,type), names_from = sex, values_from = unhealthy)
  
  
  ggplot(dis_cron_bell %>%
            arrange(woman) %>%
          #  filter(!location%in%"Canada") %>% 
            mutate(country=fct_reorder(country, woman,.desc = T)))+ 
    geom_dumbbell(size=1, color="black",
                  aes(y = country, x=woman, xend=man, color=type),
                  size_x = 3.5, size_xend = 3.5, colour_x = '#B6407D', colour_xend = '#11718A') +
    facet_grid(type~age)+
    theme_clean()+
    theme(axis.text.x = element_text( vjust = 0.3, hjust = 1, angle = 90))

  
  
  
  
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
# Helper function for string wrapping because names are too large in the facets
# Default 20 character target width.
swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)

prev.all$type = swr(prev.all$type)
prev.all<-prev.all %>% 
  filter(!type %in%c("one or two any\ncondition")) %>% 
  droplevels()


ggplot()+
geom_line(data=prev.all,
          aes(age, unhealthy, group=country,color=country),size=1, color="grey90")+
  
  geom_point(data=prev.all %>% filter(country%in%"Europe" ),
             aes(age, unhealthy, group=country, color=country), size=3.2)+
  geom_line(data=prev.all %>% filter(country%in%"Europe"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
  
  geom_point(data=prev.all%>% filter(country%in%"US"),
             aes(age, unhealthy, group=country, color=country), size=3.2)+
  geom_line(data=prev.all %>% filter(country%in%"US"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
  
  geom_point(data=prev.all %>% filter(country%in%"China"),
             aes(age, unhealthy, group=country, color=country), size=3.2)+
  geom_line(data=prev.all %>% filter(country%in%"China"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
  
  geom_point(data=prev.all %>% filter(country%in%"India"),
             aes(age, unhealthy, group=country, color=country), size=3.2)+
  geom_line(data=prev.all %>% filter(country%in%"India"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
  
  geom_point(data=prev.all %>% filter(country%in%"Korea"),
             aes(age, unhealthy, group=country, color=country), size=3.2)+
  geom_line(data=prev.all %>% filter(country%in%"Korea"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
  
  geom_point(data=prev.all %>% filter(country%in%"Mexico"),
             aes(age, unhealthy, group=country, color=country), size=3.2)+
  geom_line(data=prev.all %>% filter(country%in%"Mexico"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
  
  geom_point(data=prev.all %>% filter(country%in%"Portugal"),
             aes(age, unhealthy, group=country, color=country), size=3.2)+
  geom_line(data=prev.all %>% filter(country%in%"Portugal"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1)+
  
  facet_grid(gender~type)+
  
  theme_clean(base_size =26)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90))+
  #   scale_colour_grafify(palette = "muted") #not sequential colours
  scale_color_manual(values = c("dodgerblue4",
                                "darkolivegreen4",
                                "darkorchid3",
                                "goldenrod1",
                                "grey30",
                                "firebrick","black"))+
  ylab("%Unhealthy")









fig_prev_all<-
  
  ggplot(prev.all,
                     aes(age, country, group=country, fill=unhealthy))+
  geom_raster(interpolate = F) +
  # geom_tile (color="white") +
  theme_clean(base_size = 24)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90))+
  facet_grid(gender~type)+
  scale_fill_distiller(palette = "Spectral", 
                       name="%Unhealthy", n.breaks=4)

pdf(here("Manuscript","Figures","fig_prev_all.pdf"), width = 20, height=24)
fig_prev_all
dev.off()

png(here("Manuscript","Figures","fig_prev_all.png"), width = 2000, height=2200)
fig_prev_all
dev.off()


# now joining all decompositions for ages 60+

decomp.dfle<-fread(here("Manuscript","Data", "decomp_60_dfle.csv")) %>% 
  mutate(Mort.ADL=Mortality) %>% 
  select(Country, Disability,Mort.ADL)


decomp.cfle<-fread(here("Manuscript","Data", "decomp_60_dfle_chronic.csv")) %>% 
  mutate(Mort.Chronic=Mortality)%>% 
  select(Country, Chronic,Mort.Chronic)

decomp.all<-full_join(decomp.cfle,decomp.dfle) %>% 
  pivot_longer(cols=c(2,4), names_to = c("Health"), values_to = "values")

decomp.all.long<-decomp.all %>% 
  pivot_longer(cols=c(2,3), names_to = c("Mortality"), values_to = "values.m")


X11()

library(ggExtra)
library(ggside)
library(tidyquant)


p<-ggplot(decomp.all.long, aes(Country,values, color=Health))+
  geom_point(size = 5, alpha=0.6)+
 #facet_zoom(x = Country %in%c ("US","Europe"))+
#  theme_bw()+
  theme_bw(base_size = 24)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90,vjust = 0.3, hjust = 1))+
  ylab("Contribution to Gender Gap")+
  scale_color_manual(values=c('#B6407D', '#11718A'))+
  geom_hline(yintercept=0, linetype="dashed")


p


p1 <- ggMarginal(p, type="histogram", groupColour = TRUE,  groupFill = TRUE)


p1


p2<-ggplot(decomp.all.long, aes(Country,values.m, color=Mortality))+
  geom_point(size = 5, alpha=0.6)+
  #facet_zoom(x = Country %in%c ("US","Europe"))+
  #  theme_bw()+
  theme_bw(base_size = 24)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90,vjust = 0.3, hjust = 1))+
  ylab("Contribution to Gender Gap")+
  scale_color_manual(values=c('#B6407D', '#11718A'))+
  geom_hline(yintercept=0, linetype="dashed")


p2


p3 <- ggMarginal(p2, type="histogram", groupColour = TRUE,  groupFill = TRUE)


p3




#decomp.all<-full_join(decomp.cfle,decomp.dfle) %>% 
#  mutate(Welf.State=case_when(Country%in%c("Denmark","Finland","Norway","Sweden")~ "Scandinavian",
#                              Country%in%c("England","US")~"Anglo-Saxon",
#                              Country%in%c("Germany","France","Austria","Belgium",
#                                           "Luxembourg")~ "Bismarckian",
#                              Country%in%c("Greece","Italy","Portugal","Spain")~ "Southern",
#                              Country%in%c("Czech Republic","Poland","Estonia", "Croatia",
 #                                          "Slovenia" )~ "Eastern",
#                              Country%in%c("Korea","China","India" )~ "East Asia",
#                              
#                              TRUE                      ~ "other" ))

