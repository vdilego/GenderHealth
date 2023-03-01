#-----------------------------------------------------------#
# Data prep for ELSA   --------------------------------------#
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

# For ELSA we will be using WAVE 7 - year 2014/15

elsa <- read_dta(here("Gender_health","Data","ELSA","Harmonized","reves_elsa.dta"))  # read-in stata file from merge procedure

df1<-as_factor(elsa)                         # transform all using as_factor from haven   

df1<-df1 %>% 
  mutate(year = case_when(wave ==7 ~ '2014',
                          TRUE ~ 'NA'))
View(df1)

df2<-cSplit(df1, splitCols = c(8:9), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  filter(!is.na(rcwtresp))           # filtering only the eligible cases: non-eligibiles have a missing id


#renaming columns for standardizing with other datasets

colnames(df3)<-c("id","wave","design_wgt","cluster_wgt","ind_wgt","birth_year","age","adl", "wave_year",
                 "gender_1","gender_2","educ_1","educ_2")

df3<-df3 %>% 
  filter(wave%in%7) %>% 
  mutate(age_cat=cut(age, breaks = c(seq(0,85,5), 90, Inf),
                     labels = c("0-5","5-10", "10-15",  
                                "15-20","20-25","25-30",
                                "30-35","35-40","40-45","45-50",
                                "50-55", "55-60", "60-65","65-70",
                                "70-75",  "75-80","80-85","85-90","90+"), right=F)) %>% 
  droplevels()

# converting to survey data to account for weights. Using weights greater than 0;
# in order to estimate the prevalence we have to use weights and account for survey design.
# design_wgt is the sample design weight, to guarantee representiveness and also
# account for non-response issues. ind_wgt is the person-level analysis weight. Since
# we are only doing person-level analysis we should use these. For ELSA also include 
# cluster_wgt, which is similar to the country-level in SHARE.
# I do not know how you prefer, so I just left here the suggestion for how the data set
# needs to account for its survey design. Then later the prevalence can be estimated according
# to the age groups we find are best. We can talk about this later..
# I also left the data set in wide format and with both numeric and categorical dummy variables,
# so you can choose what you prefer or find more useful.

df_elsa_surv <- svydesign(
  ids = ~design_wgt,                  # sample weights, accounting for responsiveness
  strata= ~cluster_wgt ,               # cluster weights, sample design
  weights = ~ind_wgt,                 # individual, cross-sectional calibrated weights
  nest = T,
  data = subset( df3 , ind_wgt > 0 & age>=50)) # weights greater than 0 because 0: people in institutions

summary(df_elsa_surv)


# differences between weighted and unweighted when estimating proportions 

prop.table(table(df3$gender_1))
prop.table(svytable(~gender_1, design=df_elsa_surv))

svymean( ~ adl , df_elsa_surv , na.rm = TRUE )
#prop.table(svytable(~ diabetes_1 + gender_2 ,  df_elsa_surv ), margin = 1)


# proportions by age category with standard errors.
prop_healthy<-svyby(formula = ~adl==0, by = ~age_cat+wave_year+gender_2, 
                    design = df_elsa_surv, FUN = svymean,vartype=c("ci"),
                    na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy<-as.data.frame(prop_healthy)
colnames(prop_healthy)<-c("age","year","gender","unhealthy","healthy","CI_low_unhealthy","CI_low_healthy","CI_up_unhealthy",
                          "CI_up_healthy")
prop_healthy<-prop_healthy[, c(1:5,6,8,7,9)]


# proportions by single age with standard errors.
prop_healthy_single<-svyby(formula = ~adl==0, by = ~age+wave_year+gender_2, 
                           design = df_elsa_surv, FUN = svymean,vartype=c("ci"),
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

write.table(prev_unhealthy, here("Gender_health","Data","ELSA", "prev_elsa_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")

# now for single ages

prev_unhealthy_single<-prop_healthy_single %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prev_unhealthy_single) <- NULL

# saving this data as .csv file but you can change it if you prefer later

write.table(prev_unhealthy_single, here("Gender_health","Data","ELSA", "prev_elsa.csv"), sep = ",", col.names = NA,
            qmethod = "double")

#the total sample size is 9,666, consistent with the technical report on ELSA for wave 7. 
# see https://www.elsa-project.ac.uk/_files/ugd/540eba_1b12cb61558e4fde917160090c0952af.pdf


