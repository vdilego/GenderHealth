#-----------------------------------------------------------#
# Data prep for LASI   --------------------------------------#
# Author: Vanessa di Lego, Marília Nepomuceno, Cássio Turra #
#-----------------------------------------------------------#

library(purrr)
library(tidyverse)
library(labelled)
library(ipumsr)
library(haven)
library(dplyr)
library(splitstackshape)
library(survey)
library(here)
library(ggthemes)
library(AMR)

# For LASI we will be using WAVE 1 - year 2016/2017

lasi <- read_dta(here("Gender_health","Data","LASI","Harmonized","reves_lasi.dta"))  # read-in stata file from merge procedure

df1<-as_factor(lasi)                         # transform all using as_factor from haven   

df1<-df1 %>% 
  mutate(year = case_when(wave ==1 ~ '2016',
                          TRUE ~ 'NA'))
View(df1)

df2<-cSplit(df1, splitCols = c(6:7), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  filter(!is.na(rwtresp))           # filtering only the eligible cases: non-eligibiles have a missing id


#renaming columns for standardizing with other datasets

colnames(df3)<-c("id","wave","ind_wgt","birth_year","age","adl", "wave_year",
                 "gender_1","gender_2","educ_1","educ_2")

df3<-df3 %>% 
 # filter(wave%in%7) %>% 
  mutate(age_cat=cut(age, breaks = c(seq(0,85,5), 90, Inf),
                     labels = c("0-5","5-10", "10-15",  
                                "15-20","20-25","25-30",
                                "30-35","35-40","40-45","45-50",
                                "50-55", "55-60", "60-65","65-70",
                                "70-75",  "75-80","80-85","85-90","90+"), right=F)) %>% 
  droplevels()

# converting to survey data to account for weights. Using weights greater than 0;

df_lasi_surv <- svydesign(
  ids = ~1,                
 # strata= ~cluster_wgt ,               # cluster weights, sample design
  weights = ~ind_wgt,                 # individual, cross-sectional calibrated weights
  nest = T,
  data = subset( df3 , ind_wgt > 0 & age>=50)) # weights greater than 0 because 0: people in institutions

summary(df_lasi_surv)


# differences between weighted and unweighted when estimating proportions 

prop.table(table(df3$gender_1))
prop.table(svytable(~gender_1, design=df_lasi_surv))

svymean( ~ adl , df_lasi_surv , na.rm = TRUE )

# proportions by age category with standard errors.
prop_healthy<-svyby(formula = ~adl==0, by = ~age_cat+wave_year+gender_2, 
                    design = df_lasi_surv, FUN = svymean,vartype=c("ci"),
                    na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy<-as.data.frame(prop_healthy)
colnames(prop_healthy)<-c("age","year","gender","unhealthy","healthy","CI_low_unhealthy","CI_low_healthy","CI_up_unhealthy",
                          "CI_up_healthy")
prop_healthy<-prop_healthy[, c(1:5,6,8,7,9)]


# proportions by single age with standard errors.
prop_healthy_single<-svyby(formula = ~adl==0, by = ~age+wave_year+gender_2, 
                           design = df_lasi_surv, FUN = svymean,vartype=c("ci"),
                           na.rm = TRUE,prop_method = c("likelihood"))


prop_healthy_single<-as.data.frame(prop_healthy_single)

colnames(prop_healthy_single)<-c("age","year","gender","unhealthy","healthy","CI_low_unhealthy","CI_low_healthy","CI_up_unhealthy",
                                 "CI_up_healthy")

prop_healthy_single<-prop_healthy_single[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prev_unhealthy<-prop_healthy %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prev_unhealthy) <- NULL

# Simple graph to check gender with CI
ggplot(prev_unhealthy,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_line(data=prev_unhealthy,aes(age, CI_low_unhealthy, 
                                    group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prev_unhealthy,aes(age, CI_up_unhealthy, 
                                    group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))


# saving this data as .csv file but you can change it if you prefer later

write.table(prev_unhealthy, here("Gender_health","Data","LASI", "prev_lasi_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")

# now for single ages

prev_unhealthy_single<-prop_healthy_single %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prev_unhealthy_single) <- NULL

# saving this data as .csv file but you can change it if you prefer later

write.table(prev_unhealthy_single, here("Gender_health","Data","LASI", "prev_lasi.csv"), sep = ",", col.names = NA,
            qmethod = "double")


