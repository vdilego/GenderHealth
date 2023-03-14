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
figs.app.folder <- here("Appendix","Figures")


dir.create(figs.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(figs.app.folder, showWarnings = FALSE, recursive = TRUE)

# First setting the folders

dis.folder <- here("Data","All_Prevalence") 
mort.folder <- here("Data","Life_Tables","LT_UN")

# reading disability data for all countries at the same time
dis<-fread(here(dis.folder,"all_prev_adl.csv")) %>%  
  rename(sex=gender) %>% 
  filter(age>=60 ) 

# creating a vector with countries names (No England)
dis_noEngl <- dis %>% 
  filter(country!="England") 

cntr<- unique(dis_noEngl$country)

# creating a vector with each gender
gender<- unique(dis$sex)

# reading life tables for all countries at the same time and selecting cntr in mort dataset

mort<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "woman" = "Female","man" = "Male")) %>% 
  filter(age>=60 ) %>% 
  filter(country %in% cntr)


# loop for each country and gender

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
    
    # applying Sullivan function
    HL_f = Sullivan.fun(rates=mxwx_f)
    HL_f
    HL_m = Sullivan.fun(rates=mxwx_m)
    HL_m
    
    # The gender gap at age 60 in DFLE was:
    gap_DFLE = HL_f - HL_m
    gap_DFLE
    
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
    
    #Outputs
    out_gap[[i]] <- data.frame(Country=cntr[i],
                           GAP_DFLE=gap_DFLE,GAP_LE=gap_LE,
                           Mortality=sum.mort.contr,
                           Disability=sum.dis.contr)
    
    out_Age_gap[[i]]<- data.frame(Age=c(60,65,70,75,80),
                                       Country=cntr[i],
                                       Mortality=mort.contr,
                                       Disability=dis.contr)
}# end loop country

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

# men
HL.m.en = Sullivan.fun(rates=mxwx.m.en)
HL.m.en

# woman
HL.f.en = Sullivan.fun(rates=mxwx.f.en)
HL.f.en

# The gender gap in DFLE was:
gap.en = HL.f.en - HL.m.en
gap.en

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


Eng <- data.frame(Country="England", GAP_DFLE=gap.en, GAP_LE=gapLE,
                  Mortality=sum.mort.contr.en,Disability=sum.dis.contr.en )

Eng.Age <- data.frame(Age=c(60,65,70,75,80),Country="England",
                      Mortality=mort.contr.en, Disability=dis.contr.en)


# Adding English results in previous outputs

outgap<-rbind(out_gap,Eng)
outAgegap<-rbind(out_Age_gap,Eng.Age)


# ---------------------------------------------------------------------------------------------------------#
# plots
# ---------------------------------------------------------------------------------------------------------#

Age=seq(start.age,open.age,5)

outAgegap.long <- outAgegap%>%  
  pivot_longer(!c(Age,Country),names_to="type", values_to = "Contribution" ) 

# I think we only need one and arrange everthing here:


plot_all<-ggplot(data=outAgegap.long , aes(x=as.factor(Age), y=Contribution, 
                                      fill=factor(type, levels=c("Mortality","Disability"))))+
#  ggtitle(bquote(~'Germany (SHARE)' ))+
  xlab("Age") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-1.7, 1.7)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal(base_size = 16) +
  facet_wrap(.~Country)+
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title =  element_text(size=12),title =  element_text(size=12),
        legend.position = "right", 
        legend.background = element_rect(color = NA))

# fig.folder

pdf(here(figs.folder,"Decomp_all.pdf"), width = 15, height=17)
plot_all
dev.off()

# plots for individual countries

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

