# -----------------------------------------------------------------------------------------------------#
# Codes for replicating the paper Going Beyong the Gender Gap
# authors: Vanessa di Lego, Marília Nepomuceno, Cássio M. Turra
# Wittgenstein Centre for Demography and Global Human Capital(IIASA/OeAW/UniWien)
# Vienna Institute of Demography at the Austrian Academy of Sciences
# Max Planck Institute for Demographic Research
# Federal University of Minas Gerais (Cedeplar)
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


dir.create(figs.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(dat.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(figs.app.folder, showWarnings = FALSE, recursive = TRUE)

# First setting the folders
dis.folder <- here("Data","All_Prevalence") 
mort.folder <- here("Data","Life_Tables","LT_UN")

# reading disability data for all countries and selecting individuals aged 60 years old or more
dis<-fread(here(dis.folder,"all_prev_adl.csv")) %>%  
  rename(sex=gender) %>% 
  filter(age>=60 ) 

# creating a vector with countries names (Excluding England because it has a different LT source)
dis_noEngl <- dis %>% 
  filter(country!="England") 

cntr<- unique(dis_noEngl$country)

# creating a vector with each gender
gender<- unique(dis$sex)

# reading life tables for all countries and selecting cntr in mort dataset and individuals aged 60 years old or more
mort<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "woman" = "Female","man" = "Male")) %>% 
  filter(age>=60 ) %>% 
  filter(country %in% cntr)


# 'For Loop' for each country to calculate healthy LE, and HLE decomposition

#outputs lists
out_gap=NULL
out_Age_gap=NULL

# loop for each country 
for (i in 1:length(cntr)) {
  
  #mortality rates
  mort_cntr_f <- mort %>% 
    filter(country == cntr[i],
           sex==gender[1])
  mort_cntr_m <- mort %>% 
    filter(country == cntr[i],
           sex==gender[2])
  
  #disability rates
  dis_cntr_f <- dis %>% 
    filter(country == cntr[i],
           sex==gender[1])
  dis_cntr_m <- dis %>% 
    filter(country == cntr[i],
           sex==gender[2])
  
  # allocating mort and dis in the same vec
  mxwx_f <- c(mort_cntr_f$nMx,dis_cntr_f$unhealthy)
  mxwx_m <- c(mort_cntr_m$nMx,dis_cntr_m$unhealthy)
  
  # allocating mort and dis in low and upper bound vec
  # women
  mxwx_f.l <- c(mort_cntr_f$nMx,dis_cntr_f$CI_low_unhealthy)
  mxwx_f.u <- c(mort_cntr_f$nMx,dis_cntr_f$CI_up_unhealthy)
  
  # men
  mxwx_m.l <- c(mort_cntr_m$nMx,dis_cntr_m$CI_low_unhealthy)
  mxwx_m.u <- c(mort_cntr_m$nMx,dis_cntr_m$CI_up_unhealthy)
  
  # applying Sullivan function
  HL_f = Sullivan.fun(rates=mxwx_f)
  HL_f
  HL_m = Sullivan.fun(rates=mxwx_m)
  HL_m
  
  # applying Sullivan function for lower and upper bounds
  
  HL_f.l = Sullivan.fun(rates=mxwx_f.l)
  HL_f.l
  
  HL_f.u = Sullivan.fun(rates=mxwx_f.u)
  HL_f.u
  
  HL_m.l = Sullivan.fun(rates=mxwx_m.l)
  HL_m.l
  
  HL_m.u = Sullivan.fun(rates=mxwx_m.u)
  HL_m.u
  
  # The gender gap at age 60 in DFLE was:
  gap_DFLE = HL_f - HL_m
  gap_DFLE
  
  # gender gap with CI intervals
  gap_DFLE.l = HL_f.l - HL_m.l
  gap_DFLE.l
  
  gap_DFLE.u = HL_f.u - HL_m.u
  gap_DFLE.u
  
  # The gender gap at age 60 in LE was:
  gap_LE = mort_cntr_f$ex[1] - mort_cntr_m$ex[1]
  gap_LE
  
  # Decomposing the gap in DFLE
  HE_Decomp_Cont <- horiuchi(func=Sullivan.fun,
                             pars1 = mxwx_m, 
                             pars2 = mxwx_f,
                             N=20)
  mort.contr<-HE_Decomp_Cont [1:5]
  dis.contr<-HE_Decomp_Cont [6:10]
  
  sum.mort.contr<-sum(HE_Decomp_Cont [1:5])
  sum.dis.contr<-sum(HE_Decomp_Cont [6:10])
  
  # low CI
  # Decomposing the gap in DFLE low CI
  HE_Decomp_Cont.l <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx_m.l, 
                               pars2 = mxwx_f.l,
                               N=20)
  mort.contr.l<-HE_Decomp_Cont.l [1:5]
  dis.contr.l<-HE_Decomp_Cont.l [6:10]
  
  sum.mort.contr.l<-sum(HE_Decomp_Cont.l [1:5])
  sum.dis.contr.l<-sum(HE_Decomp_Cont.l [6:10])
  
  # Upper CI
  # low CI
  # Decomposing the gap in DFLE Upper CI
  HE_Decomp_Cont.u <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx_m.u, 
                               pars2 = mxwx_f.u,
                               N=20)
  mort.contr.u<-HE_Decomp_Cont.u [1:5]
  dis.contr.u<-HE_Decomp_Cont.u [6:10]
  
  sum.mort.contr.u<-sum(HE_Decomp_Cont.u [1:5])
  sum.dis.contr.u<-sum(HE_Decomp_Cont.u [6:10])
  
  
  
  #Outputs
  out_gap[[i]] <- data.frame(Country=cntr[i],
                             GAP_DFLE=gap_DFLE,
                             GAP_DFLE.l=gap_DFLE.l,
                             GAP_DFLE.u=gap_DFLE.u,
                             GAP_LE=gap_LE,
                             
                             Mortality=sum.mort.contr,
                             Disability=sum.dis.contr,
                             
                             Mortality.l=sum.mort.contr.l,
                             Mortality.u=sum.mort.contr.u,
                             
                             Disability.l=sum.dis.contr.l,
                             Disability.u=sum.dis.contr.u)
  
  out_Age_gap[[i]]<- data.frame(Age=c(60,65,70,75,80),
                                Country=cntr[i],
                                Mortality=mort.contr,
                                Disability=dis.contr,
                                
                                Mortality.l=mort.contr.l,
                                Mortality.u=mort.contr.u,
                                
                                Disability.l=dis.contr.l,
                                Disability.u=dis.contr.u)
}

# end the 'for loop' country

out_gap <- do.call(rbind, out_gap) 
out_Age_gap <- do.call(rbind, out_Age_gap) 



#------------------------------------------------------------------------#
# England
#------------------------------------------------------------------------#

# life table from england comes from the ONS so here we have to use a 
# different one

en_mort_f<-fread(here(mort.folder,"lt_eng_abridged.csv")) %>%  
 # mutate(sex = fct_recode(sex, "woman" = "Female")) %>% 
  mutate(country = "England") %>% 
  filter(country%in%"England" & age>=60 & sex%in%"woman" )

en_mort_m<-fread(here(mort.folder,"lt_eng_abridged.csv")) %>%  
  #mutate(sex = fct_recode(sex, "man" = "Male")) %>% 
  mutate(country = "England") %>% 
  filter(country%in%"England" & age>=60 & sex%in%"man" )

en_dis_f<-fread(here(dis.folder,"all_prev_adl.csv")) %>%  
  rename(sex=gender) %>% 
  filter(country%in%"England" & age>=60 & sex%in%"woman" )

en_dis_m<-fread(here(dis.folder,"all_prev_adl.csv")) %>% 
  rename(sex=gender) %>% 
  filter(country%in%"England" & age>=60 & sex%in%"man" )


# England
mxwx.f.en <- c(en_mort_f$nMx,en_dis_f$unhealthy)
mxwx.m.en <- c(en_mort_m$nMx,en_dis_m$unhealthy)

# CI intevals
mxwx.f.en.l <- c(en_mort_f$nMx,en_dis_f$CI_low_unhealthy)
mxwx.f.en.u <- c(en_mort_f$nMx,en_dis_f$CI_up_unhealthy)

mxwx.m.en.l <- c(en_mort_m$nMx,en_dis_m$CI_low_unhealthy)
mxwx.m.en.u <- c(en_mort_m$nMx,en_dis_m$CI_up_unhealthy)

# men
HL.m.en = Sullivan.fun(rates=mxwx.m.en)
HL.m.en

# CI interval
HL.m.en.l = Sullivan.fun(rates=mxwx.m.en.l)
HL.m.en.l

HL.m.en.u = Sullivan.fun(rates=mxwx.m.en.u)
HL.m.en.u


# woman
HL.f.en = Sullivan.fun(rates=mxwx.f.en)
HL.f.en

# CI interval

HL.f.en.l = Sullivan.fun(rates=mxwx.f.en.l)
HL.f.en.l

HL.f.en.u = Sullivan.fun(rates=mxwx.f.en.u)
HL.f.en.u

# The gender gap in DFLE was:
gap.en = HL.f.en - HL.m.en
gap.en

gap.en.l = HL.f.en.l - HL.m.en.l
gap.en.l

gap.en.u = HL.f.en.u - HL.m.en.u
gap.en.u

# the gender gap in le at age 60 is

gapLE<- en_mort_f$ex[1]-en_mort_m$ex[1]


HE_Decomp_Cont.en <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.en, 
                              pars2 = mxwx.f.en,
                              N=20)
sum.mort.contr.en<- sum(HE_Decomp_Cont.en [1:5])
sum.dis.contr.en<- sum(HE_Decomp_Cont.en [6:10])

mort.contr.en<- HE_Decomp_Cont.en [1:5]
dis.contr.en<- HE_Decomp_Cont.en [6:10]


# CI interval
# lower boung
HE_Decomp_Cont.en.l <- horiuchi(func=Sullivan.fun,
                                pars1 = mxwx.m.en.l, 
                                pars2 = mxwx.f.en.l,
                                N=20)
sum.mort.contr.en.l<- sum(HE_Decomp_Cont.en.l [1:5])
sum.dis.contr.en.l<- sum(HE_Decomp_Cont.en.l [6:10])

mort.contr.en.l<- HE_Decomp_Cont.en.l [1:5]
dis.contr.en.l<- HE_Decomp_Cont.en.l [6:10]

# upper bound

HE_Decomp_Cont.en.u <- horiuchi(func=Sullivan.fun,
                                pars1 = mxwx.m.en.u, 
                                pars2 = mxwx.f.en.u,
                                N=20)
sum.mort.contr.en.u<- sum(HE_Decomp_Cont.en.u [1:5])
sum.dis.contr.en.u<- sum(HE_Decomp_Cont.en.u [6:10])

mort.contr.en.u<- HE_Decomp_Cont.en.u [1:5]
dis.contr.en.u<- HE_Decomp_Cont.en.u [6:10]



Eng <- data.frame(Country="England", 
                  GAP_DFLE=gap.en,
                  GAP_DFLE.l=gap.en.l,
                  GAP_DFLE.u=gap.en.u,
                  GAP_LE=gapLE,
                  Mortality=sum.mort.contr.en,
                  Disability=sum.dis.contr.en,
                  
                  Mortality.l=sum.mort.contr.en.l,
                  Mortality.u=sum.mort.contr.en.u,
                  
                  Disability.l=sum.dis.contr.en.l,
                  Disability.u=sum.dis.contr.en.u
                  
)

Eng.Age <- data.frame(Age=c(60,65,70,75,80),
                      Country="England",
                      Mortality=mort.contr.en, 
                      Disability=dis.contr.en,
                      
                      Mortality.l=mort.contr.en.l,
                      Mortality.u=mort.contr.en.u, 
                      
                      Disability.l=dis.contr.en.l,
                      Disability.u=dis.contr.en.u)


# Adding English results in previous outputs

outgap<-rbind(out_gap,Eng)
outAgegap<-rbind(out_Age_gap,Eng.Age)

# save this dataframes

write.table(outgap, here("Manuscript","Data", "decomp_60_dfle.csv"),sep = ",", row.names = F)

write.table(outAgegap, here("Manuscript","Data", "decomp_age_dfle.csv"),sep = ",", row.names = F)

# ---------------------------------------------------------------------------------------------------------#
# plots
# ---------------------------------------------------------------------------------------------------------#

Age=seq(start.age,open.age,5)

outAgegap.long <- outAgegap%>%  
  select(1:4) %>% 
  pivot_longer(!c(Age,Country),names_to="type", values_to = "Contribution" ) 


outgap.ci <- outgap %>%  
  select(c(1,8:11)) %>% 
  pivot_longer(!c(Country),
               names_sep  = "\\.",
               names_to=c("type","ci"),
               values_to = c("Contribution")) 


outgap.ci <- outgap.ci%>%  
  pivot_wider(  names_from = "ci",
              values_from = "Contribution")


outgap.long <- outgap %>%  
  select(c(1,6:7))%>% 
  pivot_longer(!c(Country),
               names_to=c("type"),
               values_to = c("Contribution" )) 


# Plotting contributions to the gender gap
# All countries together 

plot_all_age<- ggplot(data=outAgegap.long , aes(x=factor(Age), y=Contribution, 
                                   fill=factor(type, levels=c("Mortality","Disability"))))+
  #  ggtitle(bquote(~'Germany (SHARE)' ))+
  xlab("Age") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.2, 1.7)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal(base_size = 24) +
  facet_wrap(.~Country)+
  theme(legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        axis.title =  element_text(size=18),title =  element_text(size=18),
        legend.position = "bottom", 
        legend.background = element_rect(color = NA),
      #  axis.text.x = element_text( vjust = 0.3, hjust = 1),
        strip.text.x = element_text(size=16))#+

plot_all_age

pdf(here(figs.folder,"Decomp_all.pdf"), width = 10, height=20)
plot_all_age
dev.off()

png(here(figs.folder,"Decomp_all.png"), width = 1500, height=1700, res=100)
plot_all_age
dev.off()

# summing age-contributions (60+)

plot_all_sum60<- ggplot() +
geom_bar(data= outgap.long %>%
                          arrange(Contribution) %>% 
                          group_by(type) %>% 
                          mutate(Country=fct_reorder(Country, Contribution,.desc = T)),
         aes(x=Country, y=Contribution, fill=factor(type, levels=c("Mortality","Disability"))),
         stat = "identity", position = position_dodge(width = 0.5))+
  xlab("Age") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_errorbar(data= outgap.ci,
                aes(x=Country, ymin=l, ymax=u, 
                    color=factor(type, levels=c("Mortality","Disability"))),
                width=0.5, alpha=0.5, size=1.2, show.legend = F, position=position_dodge(width=0.5))+
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  scale_color_manual(values=alpha(c("darkred", "blue"),0.7))+
  geom_hline(yintercept=0, linetype="dashed",  color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal(base_size = 25) +
  theme(legend.text=element_text(size=18),
        legend.title=element_text(size=18),
        axis.title =  element_text(size=18),title =  element_text(size=12),
        legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        axis.text.x = element_text( vjust = 0.3, hjust = 1))+
  coord_flip()


plot_all_sum60

# Plotting all countries pooled
# without error bars

plot_all_sum60_w<- ggplot() +
  geom_bar(data= outgap.long %>%
             arrange(Contribution) %>% 
             group_by(type) %>% 
             mutate(Country=fct_reorder(Country, Contribution,.desc = T)),
           aes(x=Country, y=Contribution, fill=factor(type, levels=c("Mortality","Disability"))),
           stat = "identity", position = "stack")+
  xlab("Age") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  scale_color_manual(values=alpha(c("darkred", "blue"),0.7))+
  geom_hline(yintercept=0, linetype="dashed",  color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal(base_size = 12) +
  theme(legend.text=element_text(size=9),
        legend.title=element_text(size=10),
        axis.title =  element_text(size=12),title =  element_text(size=12),
        legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        axis.text.x = element_text( vjust = 0.3, hjust = 1, angle=90))


plot_all_sum60_w

# Another plot (Test)
ggplot(data=outAgegap , aes(x=Disability, y=Mortality,color=Age, fill=Age))+
  #  ggtitle(bquote(~'Germany (SHARE)' ))+
  #xlab("Age") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_point(size=4, alpha=0.7)+ 
   scale_color_distiller(palette = "PuOr", n.breaks=4)+
  scale_fill_distiller(palette = "PuOr", n.breaks=4)+
#  scale_color_brewer(palette = "PuOr")+
 # scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  #ylim(-1.2, 1.7)+
  geom_vline(xintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
 # labs(fill = "Component")+
  theme_minimal(base_size = 12) +
 # facet_grid(.~Country)+
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title =  element_text(size=12),title =  element_text(size=12),
        legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1))+
  scale_x_reverse()+
  scale_y_continuous(expand = c(0, 0))

# plots for individual countries
# MN: eu acho que podemos tirar os codigos abaixo

# USA
HE_cont.US <- outAgegap.long %>%  
  filter(Country=="US") 

plot_US<-ggplot(data=HE_cont.US, aes(x=as.factor(Age), y=Contribution, 
                                     fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'US (HRS)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )


# China

HE_cont.Ch <- outAgegap.long %>%  
  filter(Country=="China")

plot_Ch<-ggplot(data=HE_cont.Ch, aes(x=as.factor(Age), y=Contribution,
                                     fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'China (CHARLS)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )



# Mexico

HE_cont.mex <-outAgegap.long %>%  
  filter(Country=="Mexico")

plot_mex<-ggplot(data=HE_cont.mex, aes(x=as.factor(Age), y=Contribution, 
                                       fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'Mexico (MHAS)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )

# India

HE_cont.ind <- outAgegap.long %>%  
  filter(Country=="India") 

plot_ind<-ggplot(data=HE_cont.ind, aes(x=as.factor(Age), y=Contribution,
                                       fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'India (LASI)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )


# Sweden

HE_cont.swe <- outAgegap.long %>%  
  filter(Country=="Sweden") 

plot_swe<-ggplot(data=HE_cont.swe, aes(x=as.factor(Age), y=Contribution, 
                                       fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'Sweden (SHARE)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1, 1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )


# Italy

HE_cont.ita <-outAgegap.long %>%  
  filter(Country=="Italy") 

plot_ita<-ggplot(data=HE_cont.ita, aes(x=as.factor(Age), y=Contribution, 
                                       fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'Italy (SHARE)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )


# Denmark

HE_cont.den <- outAgegap.long %>%  
  filter(Country=="Denmark") 

plot_den<-ggplot(data=HE_cont.den , aes(x=as.factor(Age), y=Contribution, 
                                        fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'Denmark (SHARE)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )


# Korea

HE_cont.kor <- outAgegap.long %>%  
  filter(Country=="Korea") 

plot_kor<-ggplot(data=HE_cont.kor , aes(x=as.factor(Age), y=Contribution, 
                                        fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'Korea (KLoSA)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.7)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )

# Portugal

HE_cont.pt <- outAgegap.long %>%  
  filter(Country=="Portugal") 

plot_pt<-ggplot(data=HE_cont.pt , aes(x=as.factor(Age), y=Contribution, 
                                      fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'Portugal (SHARE)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )

# Czechia

HE_cont.cz <- outAgegap.long %>%  
  filter(Country=="Czech Republic") 

plot_cz<-ggplot(data=HE_cont.cz, aes(x=as.factor(Age), y=Contribution, 
                                     fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'Czechia (SHARE)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )

# GErmany

# putting into matrix format
HE_cont.de <- outAgegap.long %>%  
  filter(Country=="Germany") 

plot_de<-ggplot(data=HE_cont.de , aes(x=as.factor(Age), y=Contribution, 
                                      fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'Germany (SHARE)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )


# England

# putting into matrix format
HE_cont.en <- outAgegap.long %>%  
  filter(Country=="England")

plot_en<-ggplot(data=HE_cont.en , aes(x=as.factor(Age), y=Contribution, 
                                      fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'England (ELSA)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )


# Europe (pooled)

# putting into matrix format
HE_cont.en <- outAgegap.long %>%  
  filter(Country=="Europe")

plot_en<-ggplot(data=HE_cont.en , aes(x=as.factor(Age), y=Contribution, 
                                      fill=factor(type, levels=c("Mortality","Disability"))))+
  ggtitle(bquote(~'Europe (SHARE)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )

