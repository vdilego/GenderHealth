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
library(ggthemes)
library(colorspace)
library(cowplot)
library(grid)
library(DemoDecomp) 
library(gridExtra)
library(forcats)


# Loading useful functions into environment
source(here("Gender_health","Rcodes","0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved
figs.folder <- here("Gender_health","Rcodes","Manuscript","Figures")
figs.app.folder <- here("Gender_health","Rcodes","Appendix","Figures")

# make in-out directories

#dir.create(figs.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(figs.app.folder, showWarnings = FALSE, recursive = TRUE)


# First setting the folders

dis.folder <- here("Gender_health","Data","All_Prevalence") 

mort.folder <- here("Gender_health","Data","Life_Tables","LT_UN")



# reading countries one by one - here we can improve the codes...

#------------------------------------------------------------------------#
# US
#------------------------------------------------------------------------#

us_mort_f<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "woman" = "Female")) %>% 
  filter(country%in%"US" & age>=60 & sex%in%"woman" )

us_mort_m<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "man" = "Male")) %>% 
  filter(country%in%"US" & age>=60 & sex%in%"man" )


us_dis_f<-fread(here(dis.folder,"all_prev_adl.csv")) %>%  
  rename(sex=gender) %>% 
  filter(country%in%"US" & age>=60 & sex%in%"woman" )

us_dis_m<-fread(here(dis.folder,"all_prev_adl.csv")) %>% 
  rename(sex=gender) %>% 
  filter(country%in%"US" & age>=60 & sex%in%"man" )


# USA
mxwx.f.US <- c(us_mort_f$nMx,us_dis_f$unhealthy)

mxwx.m.US <- c(us_mort_m$nMx,us_dis_m$unhealthy)


# men
HL.m.US = Sullivan.fun(rates=mxwx.m.US)
HL.m.US

# woman
HL.f.US = Sullivan.fun(rates=mxwx.f.US)
HL.f.US

# The gender gap in DFLE was:
gap.US = HL.f.US - HL.m.US
gap.US

# the gender gap in le at age 60 is

us_mort_f$ex[1]-us_mort_m$ex[1]


HE_Decomp_Cont.US <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.US, 
                              pars2 = mxwx.f.US,
                              N=20)
sum(HE_Decomp_Cont.US [1:5])
sum(HE_Decomp_Cont.US [6:10])


#------------------------------------------------------------------------#
# China
#------------------------------------------------------------------------#

china_mort_f<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "woman" = "Female")) %>% 
  filter(country%in%"China" & age>=60 & sex%in%"woman" )

china_mort_m<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "man" = "Male")) %>% 
  filter(country%in%"China" & age>=60 & sex%in%"man" )



china_dis_f<-fread(here(dis.folder,"all_prev_adl.csv")) %>%  
  rename(sex=gender) %>% 
  filter(country%in%"China" & age>=60 & sex%in%"woman" )

china_dis_m<-fread(here(dis.folder,"all_prev_adl.csv")) %>% 
  rename(sex=gender) %>% 
  filter(country%in%"China" & age>=60 & sex%in%"man" )


# China
mxwx.f.china <- c(china_mort_f$nMx,china_dis_f$unhealthy)

mxwx.m.china <- c(china_mort_m$nMx,china_dis_m$unhealthy)


# men
HL.m.china = Sullivan.fun(rates=mxwx.m.china)
HL.m.china

# woman
HL.f.china = Sullivan.fun(rates=mxwx.f.china)
HL.f.china

# The gender gap in DFLE was:
gap.china = HL.f.china - HL.m.china
gap.china

# the gender gap in le at age 60 is

china_mort_f$ex[1]-china_mort_m$ex[1]


HE_Decomp_Cont.china <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.china, 
                              pars2 = mxwx.f.china,
                              N=20)
sum(HE_Decomp_Cont.china [1:5])
sum(HE_Decomp_Cont.china [6:10])


#------------------------------------------------------------------------#
# Mexico
#------------------------------------------------------------------------#

mex_mort_f<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "woman" = "Female")) %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"woman" )

mex_mort_m<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "man" = "Male")) %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"man" )


mex_dis_f<-fread(here(dis.folder,"all_prev_adl.csv")) %>%  
  rename(sex=gender) %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"woman" )

mex_dis_m<-fread(here(dis.folder,"all_prev_adl.csv")) %>% 
  rename(sex=gender) %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"man" )


# Mexico
mxwx.f.mex <- c(mex_mort_f$nMx,mex_dis_f$unhealthy)

mxwx.m.mex <- c(mex_mort_m$nMx,mex_dis_m$unhealthy)


# men
HL.m.mex = Sullivan.fun(rates=mxwx.m.mex)
HL.m.mex

# woman
HL.f.mex = Sullivan.fun(rates=mxwx.f.mex)
HL.f.mex

# The gender gap in DFLE was:
gap.mex = HL.f.mex - HL.m.mex
gap.mex

# the gender gap in le at age 60 is

mex_mort_f$ex[1]-mex_mort_m$ex[1]


HE_Decomp_Cont.mex <- horiuchi(func=Sullivan.fun,
                                 pars1 = mxwx.m.mex, 
                                 pars2 = mxwx.f.mex,
                                 N=20)
sum(HE_Decomp_Cont.mex [1:5])
sum(HE_Decomp_Cont.mex [6:10])

#------------------------------------------------------------------------#
# India
#------------------------------------------------------------------------#

ind_mort_f<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "woman" = "Female")) %>% 
  filter(country%in%"India" & age>=60 & sex%in%"woman" )

ind_mort_m<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "man" = "Male")) %>% 
  filter(country%in%"India" & age>=60 & sex%in%"man" )


ind_dis_f<-fread(here(dis.folder,"all_prev_adl.csv")) %>%  
  rename(sex=gender) %>% 
  filter(country%in%"India" & age>=60 & sex%in%"woman" )

ind_dis_m<-fread(here(dis.folder,"all_prev_adl.csv")) %>% 
  rename(sex=gender) %>% 
  filter(country%in%"India" & age>=60 & sex%in%"man" )


# India
mxwx.f.ind <- c(ind_mort_f$nMx,ind_dis_f$unhealthy)

mxwx.m.ind <- c(ind_mort_m$nMx,ind_dis_m$unhealthy)


# men
HL.m.ind = Sullivan.fun(rates=mxwx.m.ind)
HL.m.ind

# woman
HL.f.ind = Sullivan.fun(rates=mxwx.f.ind)
HL.f.ind

# The gender gap in DFLE was:
gap.ind = HL.f.ind - HL.m.ind
gap.ind

# the gender gap in le at age 60 is

ind_mort_f$ex[1]-ind_mort_m$ex[1]


HE_Decomp_Cont.ind <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.ind, 
                               pars2 = mxwx.f.ind,
                               N=20)
sum(HE_Decomp_Cont.ind [1:5])
sum(HE_Decomp_Cont.ind [6:10])



#------------------------------------------------------------------------#
# Sweden
#------------------------------------------------------------------------#




#------------------------------------------------------------------------#
# Italy
#------------------------------------------------------------------------#





#------------------------------------------------------------------------#
# Denmark
#------------------------------------------------------------------------#



#------------------------------------------------------------------------#
# Korea
#------------------------------------------------------------------------#



#------------------------------------------------------------------------#
# Portugal
#------------------------------------------------------------------------#



#------------------------------------------------------------------------#
# Czech Republic
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Germany
#------------------------------------------------------------------------#



#------------------------------------------------------------------------#
# Austria
#------------------------------------------------------------------------#



#------------------------------------------------------------------------#
# Belgium
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Croatia
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Estonia
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# France
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Greece
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Israel
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Luxembourg
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Poland
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Slovenia
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Spain
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Switzerland
#------------------------------------------------------------------------#



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

en_mort_f$ex[1]-en_mort_m$ex[1]


HE_Decomp_Cont.en <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.en, 
                              pars2 = mxwx.f.en,
                              N=20)
sum(HE_Decomp_Cont.en [1:5])
sum(HE_Decomp_Cont.en [6:10])


#------------------------------------------------------------------------#
# Europe - Pooled countries
#------------------------------------------------------------------------#









# ---------------------------------------------------------------------------------------------------------#
# plots
# ---------------------------------------------------------------------------------------------------------#

Age=seq(start.age,open.age,5)

# USA
# putting into matrix format
HE_cont.US <- matrix(HE_Decomp_Cont.US,nrow=(length(HE_Decomp_Cont.US)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.US) <- c("Mortality","Disability")

HE_cont_df.US <- mutate(as.data.frame(HE_cont.US),Age=c(seq(start.age,open.age,5)))
HE_cont_res.US <- melt(HE_cont_df.US,id.vars="Age")
colnames(HE_cont_res.US) <- c("Age","type","Contribution")

plot_US<-ggplot(data=HE_cont_res.US, aes(x=as.factor(Age), y=Contribution, fill=type))+
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

# putting into matrix format
HE_cont.Ch <- matrix(HE_Decomp_Cont.Ch,nrow=(length(HE_Decomp_Cont.Ch)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.Ch) <- c("Mortality","Disability")

HE_cont_df.Ch <- mutate(as.data.frame(HE_cont.Ch),Age=c(seq(start.age,open.age,5)))
HE_cont_res.Ch <- melt(HE_cont_df.Ch,id.vars="Age")
colnames(HE_cont_res.Ch) <- c("Age","type","Contribution")

plot_Ch<-ggplot(data=HE_cont_res.Ch, aes(x=as.factor(Age), y=Contribution, fill=type))+
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

# putting into matrix format
HE_cont.mex <- matrix(HE_Decomp_Cont.mex,nrow=(length(HE_Decomp_Cont.mex)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.mex) <- c("Mortality","Disability")

HE_cont_df.mex <- mutate(as.data.frame(HE_cont.mex),Age=c(seq(start.age,open.age,5)))
HE_cont_res.mex <- melt(HE_cont_df.mex,id.vars="Age")
colnames(HE_cont_res.mex) <- c("Age","type","Contribution")

plot_mex<-ggplot(data=HE_cont_res.mex, aes(x=as.factor(Age), y=Contribution, fill=type))+
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

# putting into matrix format
HE_cont.ind <- matrix(HE_Decomp_Cont.ind,nrow=(length(HE_Decomp_Cont.ind)/2),
                      ncol=2,byrow=F)
colnames(HE_cont.ind) <- c("Mortality","Disability")

HE_cont_df.ind <- mutate(as.data.frame(HE_cont.ind),Age=c(seq(start.age,open.age,5)))
HE_cont_res.ind <- melt(HE_cont_df.ind,id.vars="Age")
colnames(HE_cont_res.ind) <- c("Age","type","Contribution")

plot_ind<-ggplot(data=HE_cont_res.ind, aes(x=as.factor(Age), y=Contribution, fill=type))+
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

# putting into matrix format
HE_cont.swe <- matrix(HE_Decomp_Cont.swe,nrow=(length(HE_Decomp_Cont.swe)/2),
                      ncol=2,byrow=F)
colnames(HE_cont.swe) <- c("Mortality","Disability")

HE_cont_df.swe <- mutate(as.data.frame(HE_cont.swe),Age=c(seq(start.age,open.age,5)))
HE_cont_res.swe <- melt(HE_cont_df.swe,id.vars="Age")
colnames(HE_cont_res.swe) <- c("Age","type","Contribution")

plot_swe<-ggplot(data=HE_cont_res.swe, aes(x=as.factor(Age), y=Contribution, fill=type))+
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

# putting into matrix format
HE_cont.ita <- matrix(HE_Decomp_Cont.ita,nrow=(length(HE_Decomp_Cont.ita)/2),
                      ncol=2,byrow=F)
colnames(HE_cont.ita) <- c("Mortality","Disability")

HE_cont_df.ita <- mutate(as.data.frame(HE_cont.ita),Age=c(seq(start.age,open.age,5)))
HE_cont_res.ita <- melt(HE_cont_df.ita,id.vars="Age")
colnames(HE_cont_res.ita) <- c("Age","type","Contribution")

plot_ita<-ggplot(data=HE_cont_res.ita, aes(x=as.factor(Age), y=Contribution, fill=type))+
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

# putting into matrix format
HE_cont.den <- matrix(HE_Decomp_Cont.den,nrow=(length(HE_Decomp_Cont.den)/2),
                      ncol=2,byrow=F)
colnames(HE_cont.den) <- c("Mortality","Disability")

HE_cont_df.den <- mutate(as.data.frame(HE_cont.den),Age=c(seq(start.age,open.age,5)))
HE_cont_res.den <- melt(HE_cont_df.den,id.vars="Age")
colnames(HE_cont_res.den) <- c("Age","type","Contribution")

plot_den<-ggplot(data=HE_cont_res.den , aes(x=as.factor(Age), y=Contribution, fill=type))+
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

# putting into matrix format
HE_cont.kor <- matrix(HE_Decomp_Cont.kor,nrow=(length(HE_Decomp_Cont.kor)/2),
                      ncol=2,byrow=F)
colnames(HE_cont.kor) <- c("Mortality","Disability")

HE_cont_df.kor <- mutate(as.data.frame(HE_cont.kor),Age=c(seq(start.age,open.age,5)))
HE_cont_res.kor <- melt(HE_cont_df.kor,id.vars="Age")
colnames(HE_cont_res.kor) <- c("Age","type","Contribution")

plot_kor<-ggplot(data=HE_cont_res.kor , aes(x=as.factor(Age), y=Contribution, fill=type))+
  ggtitle(bquote(~'Korea (KLoSA)' ))+ xlab(" ") +ylab(" ")+
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

# Portugal

# putting into matrix format
HE_cont.pt <- matrix(HE_Decomp_Cont.pt,nrow=(length(HE_Decomp_Cont.pt)/2),
                      ncol=2,byrow=F)
colnames(HE_cont.pt) <- c("Mortality","Disability")

HE_cont_df.pt <- mutate(as.data.frame(HE_cont.pt),Age=c(seq(start.age,open.age,5)))
HE_cont_res.pt <- melt(HE_cont_df.pt,id.vars="Age")
colnames(HE_cont_res.pt) <- c("Age","type","Contribution")

plot_pt<-ggplot(data=HE_cont_res.pt , aes(x=as.factor(Age), y=Contribution, fill=type))+
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

# putting into matrix format
HE_cont.cz <- matrix(HE_Decomp_Cont.cz,nrow=(length(HE_Decomp_Cont.cz)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.cz) <- c("Mortality","Disability")

HE_cont_df.cz <- mutate(as.data.frame(HE_cont.cz),Age=c(seq(start.age,open.age,5)))
HE_cont_res.cz <- melt(HE_cont_df.cz,id.vars="Age")
colnames(HE_cont_res.cz) <- c("Age","type","Contribution")

plot_cz<-ggplot(data=HE_cont_res.cz , aes(x=as.factor(Age), y=Contribution, fill=type))+
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
HE_cont.de <- matrix(HE_Decomp_Cont.de,nrow=(length(HE_Decomp_Cont.de)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.de) <- c("Mortality","Disability")

HE_cont_df.de <- mutate(as.data.frame(HE_cont.de),Age=c(seq(start.age,open.age,5)))
HE_cont_res.de <- melt(HE_cont_df.de,id.vars="Age")
colnames(HE_cont_res.de) <- c("Age","type","Contribution")

plot_de<-ggplot(data=HE_cont_res.de , aes(x=as.factor(Age), y=Contribution, fill=type))+
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
HE_cont.en <- matrix(HE_Decomp_Cont.en,nrow=(length(HE_Decomp_Cont.en)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.en) <- c("Mortality","Disability")

HE_cont_df.en <- mutate(as.data.frame(HE_cont.en),Age=c(seq(start.age,open.age,5)))
HE_cont_res.en <- melt(HE_cont_df.en,id.vars="Age")
colnames(HE_cont_res.en) <- c("Age","type","Contribution")

plot_en<-ggplot(data=HE_cont_res.en , aes(x=as.factor(Age), y=Contribution, fill=type))+
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




# Europe - Pooled results

# putting into matrix format
HE_cont.eu <- matrix(HE_Decomp_Cont.eu,nrow=(length(HE_Decomp_Cont.eu)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.eu) <- c("Mortality","Disability")

HE_cont_df.eu <- mutate(as.data.frame(HE_cont.eu),Age=c(seq(start.age,open.age,5)))
HE_cont_res.eu <- melt(HE_cont_df.eu,id.vars="Age")
colnames(HE_cont_res.eu) <- c("Age","type","Contribution")

plot_eu<-ggplot(data=HE_cont_res.eu , aes(x=as.factor(Age), y=Contribution, fill=type))+
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


library(ggpubr)


X11()
ggarrange(plot_Ch, plot_ind,plot_mex,plot_kor, plot_US, plot_swe,plot_den,plot_ita, plot_pt,
          common.legend = T, legend = "bottom")


# combining all HE decomps

HE_cont_res.Ch$Country<-"China"
HE_cont_res.cz$Country<-"Czechia"
HE_cont_res.de$Country<-"Germany"
HE_cont_res.den$Country<-"Denmark"
HE_cont_res.en$Country<-"England"
HE_cont_res.eu$Country<-"Europe"
HE_cont_res.ind$Country<-"India"
HE_cont_res.ita$Country<-"Italy"
HE_cont_res.kor$Country<-"Korea"
HE_cont_res.mex$Country<-"Mexico"
HE_cont_res.pt$Country<-"Portugal"
HE_cont_res.swe$Country<-"Sweden"
HE_cont_res.US$Country<-"USA"
  
  

he_decomp_all<-rbind(HE_cont_res.Ch,HE_cont_res.cz,HE_cont_res.de,HE_cont_res.den,HE_cont_res.en,
                     HE_cont_res.eu,HE_cont_res.ind,HE_cont_res.ita,HE_cont_res.kor,HE_cont_res.mex,
                     HE_cont_res.pt,HE_cont_res.swe,HE_cont_res.US)



X11()
p1<-ggplot(he_decomp_all %>% filter(Country%in%c("Europe","Denmark","Portugal")),aes(Age,Contribution, fill=type))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-0.7, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal(base_size = 24) +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=16),title =  element_text(size=14) )+
  facet_grid(.~Country)


p2<- ggplot(he_decomp_all %>% filter(Country%in%c("China","India","Mexico")),aes(Age,Contribution, fill=type))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-0.7, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal(base_size = 24) +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )+
  facet_grid(.~Country)


p3<- ggplot(he_decomp_all %>% filter(Country%in%c("USA","England","Korea")),aes(Age,Contribution, fill=type))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-0.7, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal(base_size = 24) +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )+
  facet_grid(.~Country)




#all countries

ggplot(he_decomp_all,aes(Age,Contribution, fill=type))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  ylim(-0.7, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal(base_size = 12) +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )+
  facet_wrap(.~Country)


# looking into the gender index - no relationship

gi<-fread(here("Gender_health","Manuscript","genderIndex.csv")) %>% 
  mutate(corr_dfle= cor.test(DFLE,Index, method = "pearson", 
                             conf.level = 0.95)$estimate,
         corr_dfle_stats= cor.test(DFLE,Index, method = "pearson", 
                             conf.level = 0.95)$p.value,
         corr_lewd= cor.test(LEWD,Index, method = "pearson", 
                             conf.level = 0.95)$estimate,
         corr_lewd_stats= cor.test(LEWD,Index, method = "pearson", 
                                   conf.level = 0.95)$p.value)
View(gi)


ggplot(dis,aes(age,unhealthy, color=country,group=country))+
 # geom_line(size=1, color="grey75")+
  #facet_grid(.~sex)+
  geom_line(data=dis %>% 
              filter(country%in%c("USA","China","Mexico","India","Korea","England","Europe")),
            aes(age, unhealthy, group=country,color=country), 
            size=1.5)+
  facet_grid(.~sex)+
  theme_minimal(base_size = 16)+
  ylab("Prevalence Unhealthy")+
  xlab("Age")+
  theme(legend.position = "bottom")+
 # scale_color_colorblind()
        #panel.grid.major.y=elemaent_line(color="grey70",linetype="dashed",size=0.3)) +
# scale_color_brewer()#+
 # scale_color_manual(values = colorspace::divergex_hcl(n =7,palette="PuOr"))
scale_color_manual(values = colorspace::diverging_hcl(n=7,palette = "Berlin"))
# facet_grid(.~gender)+
#  scale_color_manual(values=c("#B85EC2", "#00C9C8", "#CB6427" ,"black", 
 #                            "#8E3200","#B5A7B6","#009153"))

#822433
#+
  geom_line(data=prev_unhealthy_share_country %>% filter(country%in%"Portugal"),
            aes(age, unhealthy, group=country,color=country), size=1.5)+
  facet_grid(.~sex)+
  
  theme_classic(base_size = 16)+
  ylab("Prevalence Unhealthy")+
  xlab("Age")+
  theme(legend.position = "bottom",
        panel.grid.major.y=element_line(color="grey70",linetype="dashed",size=0.3)) +
  # facet_grid(.~gender)+
  scale_color_manual(values=c("#B35806", "#F1A340", "#FEE0B6" ,"black", 
                              "#D8DAEB","#998EC3","forestgreen" ))

  
X11()
ggplot(data=dis %>% 
         filter(country%in%c("USA","China","Mexico","India","Korea","England","Europe")),
                aes(x=country, y=unhealthy,fill=age), size=10)+
    geom_bar(stat = "identity", position=position_dodge())+
   # geom_errorbar(aes(ymin=CI_l, ymax=CI_u),
    #              width=.2, position=position_dodge(.9))+
    ggtitle(bquote(~'' ))+ xlab(" ") +ylab("Prevalence")+
  #  theme (plot.title = element_text(size = 12))+
   scale_fill_brewer(palette = "RdBu", direction = -1)+
    labs(fill = " ")+
  facet_grid(.~sex)+
   # ylim(0, .35)+
    theme_minimal(base_size = 16)


ggplot(data=dis %>% 
         filter(country%in%c("USA","China","Mexico","India","Korea","England","Europe")),
       aes(x=country, y=age,fill=unhealthy), size=10)+
  #geom_bar(stat = "identity", position=position_dodge())+
  geom_raster(interpolate = T)+
  # geom_errorbar(aes(ymin=CI_l, ymax=CI_u),
  #              width=.2, position=position_dodge(.9))+
  ggtitle(bquote(~'' ))+ xlab(" ") +ylab("Prevalence")+
  #  theme (plot.title = element_text(size = 12))+
  scale_fill_distiller(palette = "Spectral", direction = -1)+
 # scale_disti_brewer(palette = "RdBu", direction = -1)+
  labs(fill = " ")+
  facet_grid(.~sex)+
  # ylim(0, .35)+
  theme_minimal()
  
  
