#-----------------------------------------------------------#
# Data prep for SHARE   --------------------------------------#
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

#This is Version F in the harmonized files and incorporates the latest released version of SHARE data, 
# release 8.0.0, and adds observations from Wave 8. It contains 139,620 observations or rows. 
# It is a Respondent-level file so each row represents a unique Respondent. We will however use Wave 6,
# for year 2014, with 66,877 observations. 
#It also adds new variables and makes adjustments and corrections. We focus on data from SHARE Wave 6,
#with the release 8.0.0 as of February 2022. SHARE uses a multistage stratified sample.
#Its weighting variables make its data representative of the target populations in constituent countries.
# Wave 6 does not still have full coverage of European countries, with the following countries 
# only added in Wave 7: Finland, Lithuania, Latvia, Slovakia, Romania, Bulgaria, Malta and Cyprus.


# Reading SHARE for all years

share <- read_dta(here("Gender_health","Data","SHARE","Harmonized","reves_share.dta")) # read-in stata file from merge procedure

df1<-as_factor(share)                         # transform all using as_factor from haven    


df1<-df1 %>% 
  mutate(year = case_when(wave ==6 ~ '2014',
                          TRUE ~ 'NA'))

df2<-cSplit(df1, splitCols = c(8:9), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  filter(!is.na(rwtresp))#%>%                   # filtering only the eligible cases: non-eligibiles have a missing id
# select(1:6,10:11,15:29)                               # select only the variables for use in prevalence estimation 
#(the others are for sensitivity tests).

#renaming columns for standardizing with other datasets

colnames(df3)<-c("id","wave","country","design_wgt","ind_wgt","birth_year","age","adl",
                 "wave_year", "gender_1","gender_2","educ_1","educ_2")

#table((df3 %>% filter(ind_wgt%in%0 & age>=50))$gender_2)

df3<-df3 %>% 
 # filter(wave%in%4) %>% 
  mutate(age_cat=cut(age, breaks = c(seq(0,85,5), 90, Inf),
                     labels = c("0-5","5-10", "10-15",  
                                "15-20","20-25","25-30",
                                "30-35","35-40","40-45","45-50",
                                "50-55", "55-60", "60-65","65-70",
                                "70-75",  "75-80","80-85","85-90","90+"), right=F)) %>% 
  droplevels()

# converting to survey data to account for weights. SHARE provides individual-level or household weights 
# that account for non-response adjustment (ind_wgt) and sample design weights.
# design_wgt is the sample design weight, to guarantee representiveness and
# account for non-response issues. ind_wgt is the person-level analysis weight. SHARE uses a multistage
# stratified sample. We stratify by country - even though we can do pooled analysis as well.


df_share_surv <- svydesign( 
  ids = ~design_wgt, 
  strata= ~country,
  weights = ~ind_wgt,
  nest = T, 
  data = subset(df3, ind_wgt > 0 & age>=50))            

#All selected respondents who are not age-eligible are assigned 0 as the weight.

# ages equal or above 50 because we only want the main individual and standardize across all countries

summary(df_share_surv)

# differences between weighted and unweighted when estimating proportions 

prop.table(table(df3$gender_2))                        #non-weighted
prop.table(table(df3$age_cat))      
prop.table(svytable(~gender_2, design=df_share_surv))    #weighted
prop.table(svytable(~age_cat, design=df_share_surv))

# Now accounting for more than one variable
svytable(~adl+age, design=df_share_surv)

svytable(~adl+age_cat+gender_2, design=df_share_surv)


# proportions by age category with standard errors- here for all countries pooled together
prop_healthy<-svyby(formula = ~adl==0, by = ~age_cat+wave_year+gender_2, 
                    design = df_share_surv, FUN = svymean,vartype=c("ci"),
                    na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy<-as.data.frame(prop_healthy)
colnames(prop_healthy)<-c("age","year","gender","unhealthy","healthy","CI_low_unhealthy","CI_low_healthy","CI_up_unhealthy",
                          "CI_up_healthy")
prop_healthy<-prop_healthy[, c(1:5,6,8,7,9)]


# proportions by single age - all countries
prop_healthy_single<-svyby(formula = ~adl==0, by = ~age+wave_year+gender_2, 
                           design = df_share_surv, FUN = svymean,vartype=c("ci"),
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

X11()
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

write.table(prev_unhealthy, here("Gender_health","Data","SHARE", "prev_share_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")

# now for single ages

prev_unhealthy_single<-prop_healthy_single %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prev_unhealthy_single) <- NULL

# saving this data as .csv file but you can change it if you prefer later

write.table(prev_unhealthy_single, here("Gender_health","Data","SHARE", "prev_share.csv"), sep = ",", col.names = NA,
            qmethod = "double")

#-----------------------------------------------------------------------------------#
# country specific analysis
#-----------------------------------------------------------------------------------#

# proportions by age category with standard errors by country
prop_healthy_country<-svyby(formula = ~adl==0, by = ~age_cat+wave_year+gender_2+country, 
                    design = df_share_surv, FUN = svymean,vartype=c("ci"),
                    na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_country<-as.data.frame(prop_healthy_country)
colnames(prop_healthy_country)<-c("age","year","gender","country","unhealthy","healthy","CI_low_unhealthy","CI_low_healthy","CI_up_unhealthy",
                          "CI_up_healthy")
prop_healthy_country<-prop_healthy_country[, c(1:6,7,9,8,10)]


# proportions by single age - all countries
prop_healthy_single_country<-svyby(formula = ~adl==0, by = ~age+wave_year+gender_2+country, 
                           design = df_share_surv, FUN = svymean,vartype=c("ci"),
                           na.rm = TRUE,prop_method = c("likelihood"))


prop_healthy_single_country<-as.data.frame(prop_healthy_single_country)

colnames(prop_healthy_single_country)<-c("age","year","gender","country","unhealthy","healthy","CI_low_unhealthy","CI_low_healthy","CI_up_unhealthy",
                                 "CI_up_healthy")

prop_healthy_single_country<-prop_healthy_single_country[, c(1:6,7,9,8,10)]


# Grouping information and checking
# first age groups
prev_unhealthy_country<-prop_healthy_country %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prev_unhealthy_country) <- NULL

X11()
# Simple graph to check gender with CI
ggplot(prev_unhealthy_country,
       aes(age, country, group=country, fill=unhealthy))+
  geom_raster(interpolate = T) +
  theme_clean(base_size = 18)+
   facet_grid(gender~.)+
  scale_fill_distiller(palette = "Spectral", name="%Unhealthy")


# saving this data as .csv file but you can change it if you prefer later

write.table(prev_unhealthy_country, here("Gender_health","Data","SHARE", "prev_share_agecat_country.csv"), sep = ",", col.names = NA,
            qmethod = "double")

# now for single ages

prev_unhealthy_single_country<-prop_healthy_single_country %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prev_unhealthy_single_country) <- NULL

# saving this data as .csv file but you can change it if you prefer later

write.table(prev_unhealthy_single_country, here("Gender_health","Data","SHARE", "prev_share_country.csv"), sep = ",", col.names = NA,
            qmethod = "double")

X11()
# Simple graph to check gender with CI
ggplot(prev_unhealthy_single_country,
       aes(age, country, group=country, fill=unhealthy))+
  geom_raster(interpolate = F) +
  theme_clean(base_size = 18)+
  facet_grid(gender~.)+
  scale_fill_distiller(palette = "Spectral", name="%Unhealthy")




