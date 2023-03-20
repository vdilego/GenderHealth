#-----------------------------------------------------------#
# Data prep for MHAS   --------------------------------------#
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

# We are using the Haronized VERSION B.4 (2001-2015), FEBRUARY 2022, for the MHAS data. 
#The Mexican Health and Aging Study (MHAS) is a longitudinal household survey dataset 
# for the study of health, economic position, and quality of life among the elderly.
# MHAS datasets as of September 2020. The MHAS (Mexican Health and Aging Study)
# Version B.4 incorporates the latest released version of MHAS data, and adds several new variables.
# It contains 22,016 observations or rows. It is a Respondent-level file so each row represents a unique Respondent.
# We will focus on wave4, which is for years 2014/2015.

# Reading HRS for all years

mhas <- read_dta(here("Gender_health","Data","MHAS","Harmonized","reves_mhas.dta")) # read-in stata file from merge procedure

df1<-as_factor(mhas)                         # transform all using as_factor from haven    


df1<-df1 %>% 
  mutate(year = case_when(wave ==1 ~ '2000',
                          wave ==2 ~ '2002',
                          wave ==3 ~ '2012',
                          wave ==4 ~ '2014',
                          TRUE ~ 'NA'))

df2<-cSplit(df1, splitCols = c(6,7,9:13), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  filter(!is.na(rwtresp))#%>%                   # filtering only the eligible cases: non-eligibiles have a missing id
 # select(1:6,10:11,15:29)                               # select only the variables for use in prevalence estimation 
#(the others are for sensitivity tests).

#renaming columns for standardizing with other datasets

colnames(df3)<-c("id","wave","ind_wgt","birth_year","age","adl",
                 "wave_year", "gender_1","gender_2","educ_1","educ_2",
                 "diabetes_1", "diabetes_2","cancer_1","cancer_2",
                 "lung_1","lung_2", "heart_1", "heart_2",
                 "arthritis_1","arthritis_2")

#table((df3 %>% filter(ind_wgt%in%0 & age>=50))$gender_2)

df3<-df3 %>% 
  filter(wave%in%4) %>% 
  mutate(age_cat=cut(age, breaks = c(seq(0,85,5), 90, Inf),
                     labels = c("0-5","5-10", "10-15",  
                                "15-20","20-25","25-30",
                                "30-35","35-40","40-45","45-50",
                                "50-55", "55-60", "60-65","65-70",
                                "70-75",  "75-80","80-85","85-90","90+"), right=F)) %>% 
  droplevels()

# converting to survey data to account for weights. MHAS provides individual-level or household weights 
# that account for non-response adjustment (ind_wgt), but no sample design weights.
# Like all datasets, I left it in wide format and with both numeric and categorical dummy variables,
# so you can choose what you prefer or find more useful.
# we will define non-communicable diseases as 0, 1 or 2, 3+  - only later

# The MHAS is a simple random samples, so there are no specific weights for sample desigg, strata, etc.
# Hence. we just add ~1 to the ids and The response weights will always be the same, 
# equaling the population size divided by the sample size. Then the adjustments comes with the individual-level
# weights.


df_mhas_surv <- svydesign( 
  ids = ~1,  
  weights = ~ind_wgt,                          
  nest = T, 
  data = subset(df3, ind_wgt > 0 & age>=50))            

#All selected respondents who are not age-eligible are assigned 0 as the weight.

# ages equal or above 50 because we only want the main individual and standardize across all countries

summary(df_mhas_surv)

# differences between weighted and unweighted when estimating proportions 

prop.table(table(df3$gender_2))                        #non-weighted
prop.table(table(df3$age_cat))      
prop.table(svytable(~gender_2, design=df_mhas_surv))    #weighted
prop.table(svytable(~age_cat, design=df_mhas_surv))

# Now accounting for more than one variable
svytable(~adl+age, design=df_mhas_surv)

svytable(~adl+age_cat+gender_2, design=df_mhas_surv)


# proportions by age category with standard errors.
prop_healthy<-svyby(formula = ~adl==0, by = ~age_cat+wave_year+gender_2, 
                    design = df_mhas_surv, FUN = svymean,vartype=c("ci"),
                    na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy<-as.data.frame(prop_healthy)
colnames(prop_healthy)<-c("age","year","gender","unhealthy","healthy","CI_low_unhealthy","CI_low_healthy","CI_up_unhealthy",
                          "CI_up_healthy")
prop_healthy<-prop_healthy[, c(1:5,6,8,7,9)]


# proportions by single age with standard errors.
prop_healthy_single<-svyby(formula = ~adl==0, by = ~age+wave_year+gender_2, 
                           design = df_mhas_surv, FUN = svymean,vartype=c("ci"),
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

write.table(prev_unhealthy, here("Gender_health","Data","MHAS", "prev_mhas_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")

# now for single ages

prev_unhealthy_single<-prop_healthy_single %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prev_unhealthy_single) <- NULL

# saving this data as .csv file but you can change it if you prefer later

write.table(prev_unhealthy_single, here("Gender_health","Data","MHAS", "prev_mhas.csv"), sep = ",", col.names = NA,
            qmethod = "double")

