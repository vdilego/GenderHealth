#-----------------------------------------------------------#
# Data prep for KLoSA   --------------------------------------#
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
library(srvyr)

#The Korean Longitudinal Study of Ageing (KLoSA) is a panel survey of people aged 45 and over and their partners, 
# living in private households in Korea. 
# We will focus on Wave 5. However, because we focus on ages >50, the sample is not 7,949, but .
# We use the hamornized Version C that contains 11,174 observations or rows. It is a Respondent-level 
# file so each row represents a unique Respondent. It also adds new variables and makes adjustments and corrections.
# 
# Reading KLoSA for all years

klosa <- read_dta(here("Gender_health","Data","KLOSA","Harmonized","reves_klosa.dta")) # read-in stata file from merge procedure

df1<-as_factor(klosa)                         # transform all using as_factor from haven    


df1<-df1 %>% 
  mutate(year = case_when(wave ==5 ~ '2014',
                          TRUE ~ 'NA'))

df2<-cSplit(df1, splitCols = c(8,9), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  filter(!is.na(rwtresp))#%>%                   # filtering only the eligible cases: non-eligibiles have a missing id
# select(1:6,10:11,15:29)                               # select only the variables for use in prevalence estimation 
#(the others are for sensitivity tests).

#renaming columns for standardizing with other datasets

colnames(df3)<-c("id","wave","cluster_wgt","strata_wgt","ind_wgt","birth_year","age","adl",
                 "wave_year", "gender_1","gender_2","educ_1","educ_2")

#table((df3 %>% filter(ind_wgt%in%0 & age>=50))$gender_2)

df3<-df3 %>% 
#  filter(wave%in%4) %>% 
  mutate(age_cat=cut(age, breaks = c(seq(0,85,5), 90, Inf),
                     labels = c("0-5","5-10", "10-15",  
                                "15-20","20-25","25-30",
                                "30-35","35-40","40-45","45-50",
                                "50-55", "55-60", "60-65","65-70",
                                "70-75",  "75-80","80-85","85-90","90+"), right=F)) %>% 
  droplevels()

# converting to survey data to account for weights. 

options(survey.lonely.psu="adjust")

df_klosa_surv <- svydesign( 
  ids = ~cluster_wgt,  
  weights = ~ind_wgt,
  strata= ~strata_wgt,
  nest = T, 
  data = subset(df3, ind_wgt > 0 & age>=50))            

#All selected respondents who are not age-eligible are assigned 0 as the weight.

# ages equal or above 50 because we only want the main individual and standardize across all countries

summary(df_klosa_surv)

# differences between weighted and unweighted when estimating proportions 

prop.table(table(df3$gender_2))                        #non-weighted
prop.table(table(df3$age_cat))      
prop.table(svytable(~gender_2, design=df_klosa_surv))    #weighted
prop.table(svytable(~age_cat, design=df_klosa_surv))

# Now accounting for more than one variable
svytable(~adl+age, design=df_klosa_surv)

svytable(~adl+age_cat+gender_2, design=df_klosa_surv)


# proportions by age category with standard errors.
prop_healthy<-svyby(formula = ~adl==0, by = ~age_cat+wave_year+gender_2, 
                    design = df_klosa_surv, FUN = svymean,vartype=c("ci"),
                    na.rm = TRUE, prop_method =  c("likelihodd"))

prop_healthy<-as.data.frame(prop_healthy)
colnames(prop_healthy)<-c("age","year","gender","unhealthy","healthy","CI_low_unhealthy","CI_low_healthy","CI_up_unhealthy",
                          "CI_up_healthy")
prop_healthy<-prop_healthy[, c(1:5,6,8,7,9)]


# proportions by single age with standard errors.
prop_healthy_single<-svyby(formula = ~adl==0, by = ~age+wave_year+gender_2, 
                           design = df_klosa_surv, FUN = svymean,vartype=c("ci"),
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

write.table(prev_unhealthy, here("Gender_health","Data","KLOSA", "prev_klosa_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")

# now for single ages

prev_unhealthy_single<-prop_healthy_single %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prev_unhealthy_single) <- NULL

# saving this data as .csv file but you can change it if you prefer later

write.table(prev_unhealthy_single, here("Gender_health","Data","KLOSA", "prev_klosa.csv"), sep = ",", col.names = NA,
            qmethod = "double")
