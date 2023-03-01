#-----------------------------------------------------------#
# Data prep for HRS   --------------------------------------#
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

# We are using the Harmonized version B HRS: 37,495 observations. October 2018- 
# There is a new updated version C, until 2019 that was updated now in 2022 and contains 42,233 observations. 
# It is a Respondent level file so each row represents a unique Respondent. 
# We focus only on wave 12 (year 2014) of HRS. This leaves us with 18,747 observations

# Reading HRS for all years

hrs <- read_dta(here("Gender_health","Data","HRS","reves_hrs.dta")) # read-in stata file from merge procedure

df1<-as_factor(hrs)                         # transform all using as_factor from haven    
df1<-df1 %>% 
  mutate(year = case_when(wave ==1 ~ '1992',
                          wave ==2 ~ '1994',
                          wave ==3 ~ '1996',
                          wave ==4 ~ '1998',
                          wave ==5 ~ '2000',
                          wave ==6 ~ '2002',
                          wave ==7 ~ '2004',
                          wave ==8 ~ '2006',
                          wave ==9 ~ '2008',
                          wave ==10 ~ '2010',
                          wave ==11 ~ '2012',
                          wave ==12 ~ '2014',
                          wave ==13 ~ '2016',
                          wave ==14 ~ '2018'  ,
                          TRUE ~ 'NA'))

df2<-cSplit(df1, splitCols = c(3,8:13), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  filter(!is.na(rwtresp) & wave <13)%>%                   # filtering only the eligible cases: non-eligibiles have a missing id
  select(1:6,10:11,15:29)                               # select only the variables for use in prevalence estimation 
#(the others are for sensitivity tests).

#renaming columns for standardizing with other datasets

colnames(df3)<-c("id","wave","birth_year","age","strata_wgt","sample_wgt","ind_wgt","adl",
                 "wave_year", "gender_1","gender_2","diabetes_1", "diabetes_2","cancer_1","cancer_2",
                 "lung_1","lung_2",
                 "heart_1", "heart_2",
                 "arthritis_1","arthritis_2",
                 "educ_1","educ_2")

#table((df3 %>% filter(ind_wgt%in%0 & age>=50))$gender_2)

df3<-df3 %>% 
  filter(wave%in%12) %>% 
  mutate(age_cat=cut(age, breaks = c(seq(0,85,5), 90, Inf),
                     labels = c("0-5","5-10", "10-15",  
                                "15-20","20-25","25-30",
                                "30-35","35-40","40-45","45-50",
                                "50-55", "55-60", "60-65","65-70",
                                "70-75",  "75-80","80-85","85-90","90+"), right=F)) %>% 
  droplevels()

# converting to survey data to account for weights. HRS provides individual-level or household weights 
# that account for non-response adjustment (ind_wgt), and strata and sample weights to account for complex
# survey design ("strata_wgt","sample_wgt").
# Like all datasets, I left it in wide format and with both numeric and categorical dummy variables,
# so you can choose what you prefer or find more useful.
# we will define non-communicable diseases as 0, 1 or 2, 3+  - only later

# we have to use this survey package to account for the standard errors and sample design.
# non-institutionalized

df_hrs_surv <- svydesign( 
  ids = ~sample_wgt,  
  strata = ~strata_wgt,
  weights = ~ind_wgt,                          
  nest = T, 
  data = subset(df3, ind_wgt > 0 & age>=50))            

# weights greater than 0 because 0: people in institutions. Also, From Wave 5 to Wave 12, HRS provides weights for 
# individuals living in a nursing home. These weights are provided in RwWTR_NH. For those not living in a 
# nursing home, these weights are zero. From Wave 13, there are no nursing home weights.

# ages equal or above 50 because we only want the main individual and no spouses

summary(df_hrs_surv)

# differences between weighted and unweighted when estimating proportions 

prop.table(table(df3$gender_2))                        #non-weighted
prop.table(table(df3$age_cat))      
prop.table(svytable(~gender_2, design=df_hrs_surv))    #weighted
prop.table(svytable(~age_cat, design=df_hrs_surv))

# Now accounting for more than one variable
svytable(~adl+age, design=df_hrs_surv)

svytable(~adl+age_cat+gender_2, design=df_hrs_surv)


# proportions by age category with standard errors.
prop_healthy<-svyby(formula = ~adl==0, by = ~age_cat+wave_year+gender_2, 
                    design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                    na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy<-as.data.frame(prop_healthy)
colnames(prop_healthy)<-c("age","year","gender","unhealthy","healthy","CI_low_unhealthy","CI_low_healthy","CI_up_unhealthy",
                          "CI_up_healthy")
prop_healthy<-prop_healthy[, c(1:5,6,8,7,9)]


# proportions by single age with standard errors.
prop_healthy_single<-svyby(formula = ~adl==0, by = ~age+wave_year+gender_2, 
                           design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
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

write.table(prev_unhealthy, here("Gender_health","Data","HRS", "prev_hrs_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")

# now for single ages

prev_unhealthy_single<-prop_healthy_single %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prev_unhealthy_single) <- NULL

# saving this data as .csv file but you can change it if you prefer later

write.table(prev_unhealthy_single, here("Gender_health","Data","HRS", "prev_hrs.csv"), sep = ",", col.names = NA,
            qmethod = "double")

