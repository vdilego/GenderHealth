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


mort_un<-fread(here("Gender_health","RCodes","LT","lt_un_abridged.csv")) 
mort_england<-fread(here("Gender_health","RCodes","LT","lt_eng_abridged.csv"))
mort_eu<-fread(here("Gender_health","RCodes","LT","lt_eu_abridged.csv"))

#------------------------------------------------------------------------#
# US
#------------------------------------------------------------------------#

US_dis_f<-dis %>% 
  filter(country%in%"USA" & age>=60 & sex%in%"woman" )

US_dis_m<-dis %>% 
  filter(country%in%"USA" & age>=60 & sex%in%"man" )


US_mort_f<-mort %>% 
  filter(Country%in%"USA" & Age>=60 & Sex%in% "f")

US_mort_m<-mort %>% 
  filter(Country%in%"USA" & Age>=60 & Sex%in% "m")

# USA
mxwx.f.US <- c(US_mort_f$nMx,US_dis_f$unhealthy)
mxwx.m.US <- c(US_mort_m$nMx,US_dis_m$unhealthy)


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

US_mort_f$ex[1]-US_mort_m$ex[1]


HE_Decomp_Cont.US <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.US, 
                              pars2 = mxwx.f.US,
                              N=20)
sum(HE_Decomp_Cont.US [1:7])
sum(HE_Decomp_Cont.US [8:14])


#------------------------------------------------------------------------#
# China
#------------------------------------------------------------------------#

Ch_dis_f<-dis %>% 
  filter(country%in%"China" & age>=60 & sex%in%"woman" )

Ch_dis_m<-dis %>% 
  filter(country%in%"China" & age>=60 & sex%in%"man" )


Ch_mort_f<-mort_un %>% 
  filter(Country%in%"China" & Age>=60 & Sex%in% "Female")

Ch_mort_m<-mort_un %>% 
  filter(Country%in%"China" & Age>=60 & Sex%in% "Male")

# rates
mxwx.f.Ch <- c(Ch_mort_f$nMx,Ch_dis_f$unhealthy)
mxwx.m.Ch <- c(Ch_mort_m$nMx,Ch_dis_m$unhealthy)


# men
HL.m.Ch = Sullivan.fun(rates=mxwx.m.Ch)
HL.m.Ch

# woman
HL.f.Ch = Sullivan.fun(rates=mxwx.f.Ch)
HL.f.Ch

# The gender gap in DFLE was:
gap.Ch = HL.f.Ch - HL.m.Ch
gap.Ch

# the gender gap in le at age 60 is

Ch_mort_f$ex[1]-Ch_mort_m$ex[1]


HE_Decomp_Cont.Ch <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.Ch, 
                              pars2 = mxwx.f.Ch,
                              N=20)
sum(HE_Decomp_Cont.Ch [1:7])
sum(HE_Decomp_Cont.Ch [8:14])


#------------------------------------------------------------------------#
# Mexico
#------------------------------------------------------------------------#

mex_dis_f<-dis %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"woman" )

mex_dis_m<-dis %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"man" )


mex_mort_f<-mort_un %>% 
  filter(Country%in%"Mexico" & Age>=60 & Sex%in% "Female")

mex_mort_m<-mort_un %>% 
  filter(Country%in%"Mexico" & Age>=60 & Sex%in% "Male")

# rates
mxwx.f.mex <- c(mex_mort_f$nMx,mex_dis_f$unhealthy)
mxwx.m.mex<- c(mex_mort_m$nMx,mex_dis_m$unhealthy)


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
sum(HE_Decomp_Cont.mex [1:7])
sum(HE_Decomp_Cont.mex [8:14])

#------------------------------------------------------------------------#
# India
#------------------------------------------------------------------------#

ind_dis_f<-dis %>% 
  filter(country%in%"India" & age>=60 & sex%in%"woman" )

ind_dis_m<-dis %>% 
  filter(country%in%"India" & age>=60 & sex%in%"man" )


ind_mort_f<-mort_un %>% 
  filter(Country%in%"India" & Age>=60 & Sex%in% "Female")

ind_mort_m<-mort_un %>% 
  filter(Country%in%"India" & Age>=60 & Sex%in% "Male")

# rates
mxwx.f.ind <- c(ind_mort_f$nMx,ind_dis_f$unhealthy)
mxwx.m.ind<- c(ind_mort_m$nMx,ind_dis_m$unhealthy)


# men
HL.m.ind = Sullivan.fun(rates=mxwx.m.ind)
HL.m.ind

# woman
HL.f.ind = Sullivan.fun(rates=mxwx.f.ind)
HL.f.ind

# The gender gap in DFLE was:
gap.ind = HL.f.ind - HL.m.ind
gap.ind

ind_mort_f$ex[1]-ind_mort_m$ex[1]

HE_Decomp_Cont.ind <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.ind, 
                               pars2 = mxwx.f.ind,
                               N=20)
sum(HE_Decomp_Cont.ind [1:7])
sum(HE_Decomp_Cont.ind [8:14])

#------------------------------------------------------------------------#
# Sweden
#------------------------------------------------------------------------#

swe_dis_f<-dis %>% 
  filter(country%in%"Sweden" & age>=60 & sex%in%"woman" )

swe_dis_m<-dis %>% 
  filter(country%in%"Sweden" & age>=60 & sex%in%"man" )


swe_mort_f<-mort %>% 
  filter(Country%in%"Sweden" & Age>=60 & Sex%in% "f")

swe_mort_m<-mort %>% 
  filter(Country%in%"Sweden" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.swe <- c(swe_mort_f$nMx,swe_dis_f$unhealthy)
mxwx.m.swe<- c(swe_mort_m$nMx,swe_dis_m$unhealthy)


# men
HL.m.swe = Sullivan.fun(rates=mxwx.m.swe)
HL.m.swe

# woman
HL.f.swe = Sullivan.fun(rates=mxwx.f.swe)
HL.f.swe

# The gender gap in DFLE was:
gap.swe = HL.f.swe - HL.m.swe
gap.swe

swe_mort_f$ex[1]-swe_mort_m$ex[1]


HE_Decomp_Cont.swe <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.swe, 
                               pars2 = mxwx.f.swe,
                               N=20)
sum(HE_Decomp_Cont.swe [1:7])
sum(HE_Decomp_Cont.swe [8:14])


#------------------------------------------------------------------------#
# Italy
#------------------------------------------------------------------------#

ita_dis_f<-dis %>% 
  filter(country%in%"Italy" & age>=60 & sex%in%"woman" )

ita_dis_m<-dis %>% 
  filter(country%in%"Italy" & age>=60 & sex%in%"man" )


ita_mort_f<-mort %>% 
  filter(Country%in%"Italy" & Age>=60 & Sex%in% "f")

ita_mort_m<-mort %>% 
  filter(Country%in%"Italy" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.ita <- c(ita_mort_f$nMx,ita_dis_f$unhealthy)
mxwx.m.ita<- c(ita_mort_m$nMx,ita_dis_m$unhealthy)


# men
HL.m.ita = Sullivan.fun(rates=mxwx.m.ita)
HL.m.ita

# woman
HL.f.ita = Sullivan.fun(rates=mxwx.f.ita)
HL.f.ita

# The gender gap in DFLE was:
gap.ita = HL.f.ita - HL.m.ita
gap.ita

ita_mort_f$ex[1]-ita_mort_m$ex[1]

HE_Decomp_Cont.ita <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.ita, 
                               pars2 = mxwx.f.ita,
                               N=20)
sum(HE_Decomp_Cont.ita [1:7])
sum(HE_Decomp_Cont.ita [8:14])


#------------------------------------------------------------------------#
# Denmark
#------------------------------------------------------------------------#

den_dis_f<-dis %>% 
  filter(country%in%"Denmark" & age>=60 & sex%in%"woman" )

den_dis_m<-dis %>% 
  filter(country%in%"Denmark" & age>=60 & sex%in%"man" )


den_mort_f<-mort %>% 
  filter(Country%in%"Denmark" & Age>=60 & Sex%in% "f")

den_mort_m<-mort %>% 
  filter(Country%in%"Denmark" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.den <- c(den_mort_f$nMx,den_dis_f$unhealthy)
mxwx.m.den<- c( den_mort_m$nMx,den_dis_m$unhealthy)


# men
HL.m.den = Sullivan.fun(rates=mxwx.m.den)
HL.m.den

# woman
HL.f.den = Sullivan.fun(rates=mxwx.f.den)
HL.f.den

# The gender gap in DFLE was:
gap.den = HL.f.den - HL.m.den
gap.den

den_mort_f$ex[1]-den_mort_m$ex[1]

HE_Decomp_Cont.den <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.den, 
                               pars2 = mxwx.f.den,
                               N=20)
sum(HE_Decomp_Cont.den [1:7])
sum(HE_Decomp_Cont.den [8:14])


#------------------------------------------------------------------------#
# Korea
#------------------------------------------------------------------------#

kor_dis_f<-dis %>% 
  filter(country%in%"Korea" & age>=60 & sex%in%"woman" )

kor_dis_m<-dis %>% 
  filter(country%in%"Korea" & age>=60 & sex%in%"man" )


kor_mort_f<-mort %>% 
  filter(Country%in%"Korea" & Age>=60 & Sex%in% "f")

kor_mort_m<-mort %>% 
  filter(Country%in%"Korea" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.kor <- c(kor_mort_f$nMx,kor_dis_f$unhealthy)
mxwx.m.kor<- c(kor_mort_m$nMx,kor_dis_m$unhealthy)


# men
HL.m.kor = Sullivan.fun(rates=mxwx.m.kor)
HL.m.kor

# woman
HL.f.kor = Sullivan.fun(rates=mxwx.f.kor)
HL.f.kor

# The gender gap in DFLE was:
gap.kor = HL.f.kor - HL.m.kor
gap.kor

kor_mort_f$ex[1]-kor_mort_m$ex[1]

HE_Decomp_Cont.kor <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.kor, 
                               pars2 = mxwx.f.kor,
                               N=20)
sum(HE_Decomp_Cont.kor [1:7])
sum(HE_Decomp_Cont.kor [8:14])


#------------------------------------------------------------------------#
# Portugal
#------------------------------------------------------------------------#

pt_dis_f<-dis %>% 
  filter(country%in%"Portugal" & age>=60 & sex%in%"woman" )

pt_dis_m<-dis %>% 
  filter(country%in%"Portugal" & age>=60 & sex%in%"man" )


pt_mort_f<-mort %>% 
  filter(Country%in%"Portugal" & Age>=60 & Sex%in% "f")

pt_mort_m<-mort %>% 
  filter(Country%in%"Portugal" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.pt <- c(pt_mort_f$nMx,pt_dis_f$unhealthy)
mxwx.m.pt<- c( pt_mort_m$nMx,pt_dis_m$unhealthy)


# men
HL.m.pt = Sullivan.fun(rates=mxwx.m.pt)
HL.m.pt

# woman
HL.f.pt = Sullivan.fun(rates=mxwx.f.pt)
HL.f.pt

# The gender gap in DFLE was:
gap.pt = HL.f.pt - HL.m.pt
gap.pt

pt_mort_f$ex[1]-pt_mort_m$ex[1]

HE_Decomp_Cont.pt <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.pt, 
                               pars2 = mxwx.f.pt,
                               N=20)
sum(HE_Decomp_Cont.pt [1:7])
sum(HE_Decomp_Cont.pt [8:14])


#------------------------------------------------------------------------#
# Czechia
#------------------------------------------------------------------------#

cz_dis_f<-dis %>% 
  filter(country%in%"Czechia" & age>=60 & sex%in%"woman" )

cz_dis_m<-dis %>% 
  filter(country%in%"Czechia" & age>=60 & sex%in%"man" )


cz_mort_f<-mort %>% 
  filter(Country%in%"Czechia" & Age>=60 & Sex%in% "f")

cz_mort_m<-mort %>% 
  filter(Country%in%"Czechia" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.cz <- c(cz_mort_f$nMx,cz_dis_f$unhealthy)
mxwx.m.cz<- c( cz_mort_m$nMx,cz_dis_m$unhealthy)


# men
HL.m.cz = Sullivan.fun(rates=mxwx.m.cz)
HL.m.cz

# woman
HL.f.cz = Sullivan.fun(rates=mxwx.f.cz)
HL.f.cz

# The gender gap in DFLE was:
gap.cz = HL.f.cz - HL.m.cz
gap.cz

cz_mort_f$ex[1]-cz_mort_m$ex[1]

HE_Decomp_Cont.cz <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.cz, 
                              pars2 = mxwx.f.cz,
                              N=20)
sum(HE_Decomp_Cont.cz [1:7])
sum(HE_Decomp_Cont.cz [8:14])


#------------------------------------------------------------------------#
# Germany
#------------------------------------------------------------------------#

de_dis_f<-dis %>% 
  filter(country%in%"Germany" & age>=60 & sex%in%"woman" )

de_dis_m<-dis %>% 
  filter(country%in%"Germany" & age>=60 & sex%in%"man" )


de_mort_f<-mort %>% 
  filter(Country%in%"Germany" & Age>=60 & Sex%in% "f")

de_mort_m<-mort %>% 
  filter(Country%in%"Germany" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.de <- c(de_mort_f$nMx,de_dis_f$unhealthy)
mxwx.m.de<- c(de_mort_m$nMx,de_dis_m$unhealthy)


# men
HL.m.de = Sullivan.fun(rates=mxwx.m.de)
HL.m.de

# woman
HL.f.de = Sullivan.fun(rates=mxwx.f.de)
HL.f.de

# The gender gap in DFLE was:
gap.de = HL.f.de - HL.m.de
gap.de

de_mort_f$ex[1]-de_mort_m$ex[1]

HE_Decomp_Cont.de <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.de, 
                              pars2 = mxwx.f.de,
                              N=20)
sum(HE_Decomp_Cont.de [1:7])
sum(HE_Decomp_Cont.de [8:14])



#------------------------------------------------------------------------#
# Austria
#------------------------------------------------------------------------#

at_dis_f<-dis %>% 
  filter(country%in%"Austria" & age>=60 & sex%in%"woman" )

at_dis_m<-dis %>% 
  filter(country%in%"Austria" & age>=60 & sex%in%"man" )


at_mort_f<-mort %>% 
  filter(Country%in%"Austria" & Age>=60 & Sex%in% "f")

at_mort_m<-mort %>% 
  filter(Country%in%"Austria" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.at <- c(at_mort_f$nMx,at_dis_f$unhealthy)
mxwx.m.at<- c(at_mort_m$nMx,at_dis_m$unhealthy)


# men
HL.m.at = Sullivan.fun(rates=mxwx.m.at)
HL.m.at

# woman
HL.f.at = Sullivan.fun(rates=mxwx.f.at)
HL.f.at

# The gender gap in DFLE was:
gap.at = HL.f.at - HL.m.at
gap.at

at_mort_f$ex[1]-at_mort_m$ex[1]

HE_Decomp_Cont.at <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.at, 
                              pars2 = mxwx.f.at,
                              N=20)
sum(HE_Decomp_Cont.at [1:7])
sum(HE_Decomp_Cont.at [8:14])

#------------------------------------------------------------------------#
# Belgium
#------------------------------------------------------------------------#

be_dis_f<-dis %>% 
  filter(country%in%"Belgium" & age>=60 & sex%in%"woman" )

be_dis_m<-dis %>% 
  filter(country%in%"Belgium" & age>=60 & sex%in%"man" )


be_mort_f<-mort %>% 
  filter(Country%in%"Belgium" & Age>=60 & Sex%in% "f")

be_mort_m<-mort %>% 
  filter(Country%in%"Belgium" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.be <- c(be_mort_f$nMx,be_dis_f$unhealthy)
mxwx.m.be<- c(be_mort_m$nMx,be_dis_m$unhealthy)


# men
HL.m.be = Sullivan.fun(rates=mxwx.m.be)
HL.m.be

# woman
HL.f.be = Sullivan.fun(rates=mxwx.f.be)
HL.f.be

# The gender gap in DFLE was:
gap.be = HL.f.be - HL.m.be
gap.be

be_mort_f$ex[1]-be_mort_m$ex[1]

HE_Decomp_Cont.be <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.be, 
                              pars2 = mxwx.f.be,
                              N=20)
sum(HE_Decomp_Cont.be [1:7])
sum(HE_Decomp_Cont.be [8:14])

#------------------------------------------------------------------------#
# Croatia
#------------------------------------------------------------------------#

cr_dis_f<-dis %>% 
  filter(country%in%"Croatia" & age>=60 & sex%in%"woman" )

cr_dis_m<-dis %>% 
  filter(country%in%"Croatia" & age>=60 & sex%in%"man" )


cr_mort_f<-mort %>% 
  filter(Country%in%"Croatia" & Age>=60 & Sex%in% "f")

cr_mort_m<-mort %>% 
  filter(Country%in%"Croatia" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.cr <- c(cr_mort_f$nMx,cr_dis_f$unhealthy)
mxwx.m.cr<- c(cr_mort_m$nMx,cr_dis_m$unhealthy)


# men
HL.m.cr = Sullivan.fun(rates=mxwx.m.cr)
HL.m.cr

# woman
HL.f.cr = Sullivan.fun(rates=mxwx.f.cr)
HL.f.cr

# The gender gap in DFLE was:
gap.cr = HL.f.cr - HL.m.cr
gap.cr

cr_mort_f$ex[1]-cr_mort_m$ex[1]

HE_Decomp_Cont.cr <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.cr, 
                              pars2 = mxwx.f.cr,
                              N=20)
sum(HE_Decomp_Cont.cr [1:7])
sum(HE_Decomp_Cont.cr[8:14])


#------------------------------------------------------------------------#
# Estonia
#------------------------------------------------------------------------#

st_dis_f<-dis %>% 
  filter(country%in%"Estonia" & age>=60 & sex%in%"woman" )

st_dis_m<-dis %>% 
  filter(country%in%"Estonia" & age>=60 & sex%in%"man" )


st_mort_f<-mort %>% 
  filter(Country%in%"Estonia" & Age>=60 & Sex%in% "f")

st_mort_m<-mort %>% 
  filter(Country%in%"Estonia" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.st <- c(st_mort_f$nMx,st_dis_f$unhealthy)
mxwx.m.st<- c(st_mort_m$nMx,st_dis_m$unhealthy)


# men
HL.m.st= Sullivan.fun(rates=mxwx.m.st)
HL.m.st

# woman
HL.f.st = Sullivan.fun(rates=mxwx.f.st)
HL.f.st

# The gender gap in DFLE was:
gap.st = HL.f.st - HL.m.st
gap.st

st_mort_f$ex[1]-st_mort_m$ex[1]

HE_Decomp_Cont.st <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.st, 
                              pars2 = mxwx.f.st,
                              N=20)
sum(HE_Decomp_Cont.st [1:7])
sum(HE_Decomp_Cont.st[8:14])

#------------------------------------------------------------------------#
# France
#------------------------------------------------------------------------#

fr_dis_f<-dis %>% 
  filter(country%in%"France" & age>=60 & sex%in%"woman" )

fr_dis_m<-dis %>% 
  filter(country%in%"France" & age>=60 & sex%in%"man" )


fr_mort_f<-mort %>% 
  filter(Country%in%"France" & Age>=60 & Sex%in% "f")

fr_mort_m<-mort %>% 
  filter(Country%in%"France" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.fr <- c(fr_mort_f$nMx,fr_dis_f$unhealthy)
mxwx.m.fr<- c(fr_mort_m$nMx,fr_dis_m$unhealthy)


# men
HL.m.fr= Sullivan.fun(rates=mxwx.m.fr)
HL.m.fr

# woman
HL.f.fr = Sullivan.fun(rates=mxwx.f.fr)
HL.f.fr

# The gender gap in DFLE was:
gap.fr = HL.f.fr - HL.m.fr
gap.fr

fr_mort_f$ex[1]-fr_mort_m$ex[1]

HE_Decomp_Cont.fr <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.fr, 
                              pars2 = mxwx.f.fr,
                              N=20)
sum(HE_Decomp_Cont.fr[1:7])
sum(HE_Decomp_Cont.fr[8:14])

#------------------------------------------------------------------------#
# Greece
#------------------------------------------------------------------------#

gr_dis_f<-dis %>% 
  filter(country%in%"Greece" & age>=60 & sex%in%"woman" )

gr_dis_m<-dis %>% 
  filter(country%in%"Greece" & age>=60 & sex%in%"man" )


gr_mort_f<-mort %>% 
  filter(Country%in%"Greece" & Age>=60 & Sex%in% "f")

gr_mort_m<-mort %>% 
  filter(Country%in%"Greece" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.gr<- c(gr_mort_f$nMx,gr_dis_f$unhealthy)
mxwx.m.gr<- c(gr_mort_m$nMx,gr_dis_m$unhealthy)


# men
HL.m.gr= Sullivan.fun(rates=mxwx.m.gr)
HL.m.gr

# woman
HL.f.gr = Sullivan.fun(rates=mxwx.f.gr)
HL.f.gr

# The gender gap in DFLE was:
gap.gr = HL.f.gr - HL.m.gr
gap.gr

gr_mort_f$ex[1]-gr_mort_m$ex[1]

HE_Decomp_Cont.gr <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.gr, 
                              pars2 = mxwx.f.gr,
                              N=20)
sum(HE_Decomp_Cont.gr[1:7])
sum(HE_Decomp_Cont.gr[8:14])


#------------------------------------------------------------------------#
# Israel
#------------------------------------------------------------------------#

is_dis_f<-dis %>% 
  filter(country%in%"Israel" & age>=60 & sex%in%"woman" )

is_dis_m<-dis %>% 
  filter(country%in%"Israel" & age>=60 & sex%in%"man" )


is_mort_f<-mort %>% 
  filter(Country%in%"Israel" & Age>=60 & Sex%in% "f")

is_mort_m<-mort %>% 
  filter(Country%in%"Israel" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.is<- c(is_mort_f$nMx,is_dis_f$unhealthy)
mxwx.m.is<- c(is_mort_m$nMx,is_dis_m$unhealthy)


# men
HL.m.is= Sullivan.fun(rates=mxwx.m.is)
HL.m.is

# woman
HL.f.is = Sullivan.fun(rates=mxwx.f.is)
HL.f.is

# The gender gap in DFLE was:
gap.is = HL.f.is - HL.m.is
gap.is

is_mort_f$ex[1]-is_mort_m$ex[1]

HE_Decomp_Cont.is <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.is, 
                              pars2 = mxwx.f.is,
                              N=20)
sum(HE_Decomp_Cont.is[1:7])
sum(HE_Decomp_Cont.is[8:14])


#------------------------------------------------------------------------#
# Luxembourg
#------------------------------------------------------------------------#

lx_dis_f<-dis %>% 
  filter(country%in%"Luxembourg" & age>=60 & sex%in%"woman" )

lx_dis_m<-dis %>% 
  filter(country%in%"Luxembourg"  & age>=60 & sex%in%"man" )


lx_mort_f<-mort %>% 
  filter(Country%in%"Luxembourg" & Age>=60 & Sex%in% "f")

lx_mort_m<-mort %>% 
  filter(Country%in%"Luxembourg" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.lx<- c(lx_mort_f$nMx,lx_dis_f$unhealthy)
mxwx.m.lx<- c(lx_mort_m$nMx,lx_dis_m$unhealthy)


# men
HL.m.lx= Sullivan.fun(rates=mxwx.m.lx)
HL.m.lx

# woman
HL.f.lx = Sullivan.fun(rates=mxwx.f.lx)
HL.f.lx

# The gender gap in DFLE was:
gap.lx = HL.f.lx - HL.m.lx
gap.lx

lx_mort_f$ex[1]-lx_mort_m$ex[1]

HE_Decomp_Cont.lx <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.lx, 
                              pars2 = mxwx.f.lx,
                              N=20)
sum(HE_Decomp_Cont.lx[1:7])
sum(HE_Decomp_Cont.lx[8:14])


#------------------------------------------------------------------------#
# Poland
#------------------------------------------------------------------------#

pol_dis_f<-dis %>% 
  filter(country%in%"Poland" & age>=60 & sex%in%"woman" )

pol_dis_m<-dis %>% 
  filter(country%in%"Poland"  & age>=60 & sex%in%"man" )


pol_mort_f<-mort %>% 
  filter(Country%in%"Poland" & Age>=60 & Sex%in% "f")

pol_mort_m<-mort %>% 
  filter(Country%in%"Poland" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.pol<- c(pol_mort_f$nMx,pol_dis_f$unhealthy)
mxwx.m.pol<- c(pol_mort_m$nMx,pol_dis_m$unhealthy)


# men
HL.m.pol= Sullivan.fun(rates=mxwx.m.pol)
HL.m.pol

# woman
HL.f.pol = Sullivan.fun(rates=mxwx.f.pol)
HL.f.pol

# The gender gap in DFLE was:
gap.pol = HL.f.pol - HL.m.pol
gap.pol

pol_mort_f$ex[1]-pol_mort_m$ex[1]

HE_Decomp_Cont.pol <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.pol, 
                              pars2 = mxwx.f.pol,
                              N=20)
sum(HE_Decomp_Cont.pol[1:7])
sum(HE_Decomp_Cont.pol[8:14])


#------------------------------------------------------------------------#
# Slovenia
#------------------------------------------------------------------------#

slo_dis_f<-dis %>% 
  filter(country%in%"Slovenia" & age>=60 & sex%in%"woman" )

slo_dis_m<-dis %>% 
  filter(country%in%"Slovenia"  & age>=60 & sex%in%"man" )


slo_mort_f<-mort %>% 
  filter(Country%in%"Slovenia" & Age>=60 & Sex%in% "f")

slo_mort_m<-mort %>% 
  filter(Country%in%"Slovenia" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.slo<- c(slo_mort_f$nMx,slo_dis_f$unhealthy)
mxwx.m.slo<- c(slo_mort_m$nMx,slo_dis_m$unhealthy)


# men
HL.m.slo= Sullivan.fun(rates=mxwx.m.slo)
HL.m.slo

# woman
HL.f.slo = Sullivan.fun(rates=mxwx.f.slo)
HL.f.slo

# The gender gap in DFLE was:
gap.slo = HL.f.slo - HL.m.slo
gap.slo

slo_mort_f$ex[1]-slo_mort_m$ex[1]

HE_Decomp_Cont.slo <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.slo, 
                               pars2 = mxwx.f.slo,
                               N=20)
sum(HE_Decomp_Cont.slo[1:7])
sum(HE_Decomp_Cont.slo[8:14])

#------------------------------------------------------------------------#
# Spain
#------------------------------------------------------------------------#

spa_dis_f<-dis %>% 
  filter(country%in%"Spain" & age>=60 & sex%in%"woman" )

spa_dis_m<-dis %>% 
  filter(country%in%"Spain"  & age>=60 & sex%in%"man" )


spa_mort_f<-mort %>% 
  filter(Country%in%"Spain" & Age>=60 & Sex%in% "f")

spa_mort_m<-mort %>% 
  filter(Country%in%"Spain" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.spa<- c(spa_mort_f$nMx,spa_dis_f$unhealthy)
mxwx.m.spa<- c(spa_mort_m$nMx,spa_dis_m$unhealthy)


# men
HL.m.spa= Sullivan.fun(rates=mxwx.m.spa)
HL.m.spa

# woman
HL.f.spa = Sullivan.fun(rates=mxwx.f.spa)
HL.f.spa

# The gender gap in DFLE was:
gap.spa = HL.f.spa - HL.m.spa
gap.spa

spa_mort_f$ex[1]-spa_mort_m$ex[1]

HE_Decomp_Cont.spa <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.spa, 
                               pars2 = mxwx.f.spa,
                               N=20)
sum(HE_Decomp_Cont.spa[1:7])
sum(HE_Decomp_Cont.spa[8:14])

#------------------------------------------------------------------------#
# Switzerland
#------------------------------------------------------------------------#

swi_dis_f<-dis %>% 
  filter(country%in%"Switzerland" & age>=60 & sex%in%"woman" )

swi_dis_m<-dis %>% 
  filter(country%in%"Switzerland"  & age>=60 & sex%in%"man" )


swi_mort_f<-mort %>% 
  filter(Country%in%"Switzerland" & Age>=60 & Sex%in% "f")

swi_mort_m<-mort %>% 
  filter(Country%in%"Switzerland" & Age>=60 & Sex%in% "m")

# rates
mxwx.f.swi<- c(swi_mort_f$nMx,swi_dis_f$unhealthy)
mxwx.m.swi<- c(swi_mort_m$nMx,swi_dis_m$unhealthy)


# men
HL.m.swi= Sullivan.fun(rates=mxwx.m.swi)
HL.m.swi

# woman
HL.f.swi = Sullivan.fun(rates=mxwx.f.swi)
HL.f.swi

# The gender gap in DFLE was:
gap.swi = HL.f.swi - HL.m.swi
gap.swi

swi_mort_f$ex[1]-swi_mort_m$ex[1]

HE_Decomp_Cont.swi <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.swi, 
                               pars2 = mxwx.f.swi,
                               N=20)
sum(HE_Decomp_Cont.swi[1:7])
sum(HE_Decomp_Cont.swi[8:14])

#------------------------------------------------------------------------#
# England
#------------------------------------------------------------------------#

# life table from england comes from the ONS

en_dis_f<-dis %>% 
  filter(country%in%"England" & age>=60 & sex%in%"woman" )

en_dis_m<-dis %>% 
  filter(country%in%"England" & age>=60 & sex%in%"man" )


en_mort_f<-mort_england %>% 
  filter(Age>=60 & Sex%in% "woman")

en_mort_m<-mort_england %>% 
  filter(Age>=60 & Sex%in% "man")

# rates
mxwx.f.en <- c(en_mort_f$nMx,en_dis_f$unhealthy)
mxwx.m.en<- c(en_mort_m$nMx,en_dis_m$unhealthy)


# men
HL.m.en = Sullivan.fun(rates=mxwx.m.en)
HL.m.en

# woman
HL.f.en = Sullivan.fun(rates=mxwx.f.en)
HL.f.en

# The gender gap in DFLE was:
gap.en = HL.f.en - HL.m.en
gap.en

en_mort_f$ex[1]-en_mort_m$ex[1]

HE_Decomp_Cont.en <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.en, 
                              pars2 = mxwx.f.en,
                              N=20)
sum(HE_Decomp_Cont.en [1:7])
sum(HE_Decomp_Cont.en [8:14])


#------------------------------------------------------------------------#
# Europe - Pooled countries
#------------------------------------------------------------------------#

eu_dis_f<-dis %>% 
  filter(country%in%"Europe" & age>=60 & sex%in%"woman" )

eu_dis_m<-dis %>% 
  filter(country%in%"Europe" & age>=60 & sex%in%"man" )


eu_mort_f<-mort_eu %>% 
  filter(Age>=60 & Sex%in% "Females")

eu_mort_m<-mort_eu %>% 
  filter( Age>=60 & Sex%in% "Males")

# rates
mxwx.f.eu <- c(eu_mort_f$nMx,eu_dis_f$unhealthy)
mxwx.m.eu<- c(eu_mort_m$nMx,eu_dis_m$unhealthy)


# men
HL.m.eu = Sullivan.fun(rates=mxwx.m.eu)
HL.m.eu

# woman
HL.f.eu = Sullivan.fun(rates=mxwx.f.eu)
HL.f.eu

# The gender gap in DFLE was:
gap.eu = HL.f.eu - HL.m.eu
gap.eu



HE_Decomp_Cont.eu <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.eu, 
                              pars2 = mxwx.f.eu,
                              N=20)
sum(HE_Decomp_Cont.eu [1:7])
sum(HE_Decomp_Cont.eu [8:14])




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
          plot_eu,
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
  
  
