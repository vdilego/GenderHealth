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
library(metan)
library(gtsummary)
library(corrplot)
library(likert)
library(flextable)
library(naniar)


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


share <- read_dta(here("Gender_health","Data","SHARE","Stata_Data","share_02232023.dta")) # read-in stata file from merge procedure

df1<-as_factor(share)                         # transform all using as_factor from haven    

df1<-df1 %>% 
  mutate(year = case_when(wave ==1 ~ '2004',
                          wave ==2 ~ '2007',
                         # wave ==3 ~ '2010',
                          wave ==4 ~ '2011',
                          wave ==5 ~ '2013',
                          wave ==6 ~ '2015',
                          wave ==7 ~ '2017',
                          wave ==8 ~ '2019',
                          TRUE ~ 'NA'))

df1<-unlabelled(df1)

df2<-cSplit(df1, splitCols = c(8:9,11:16), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  filter(!is.na(rwtresp) & wave%in%c(6))                  # filtering only the eligible cases: non-eligibiles have a missing id

#renaming columns for standardizing with other datasets

#colnames(df3)<-c("id","wave","country","design_wgt","ind_wgt","birth_year","age","adl",
#                 "wave_year", "gender_1","gender_2","educ_1","educ_2")

#table((df3 %>% filter(ind_wgt%in%0 & age>=50))$gender_2)

df3<-df3 %>% 
 # filter(wave%in%4) %>% 
  mutate(age_cat=cut(ragey, breaks = c(seq(0,75,5), 80, Inf),
                     labels = c("0-5","5-10", "10-15",  
                                "15-20","20-25","25-30",
                                "30-35","35-40","40-45","45-50",
                                "50-55", "55-60", "60-65","65-70",
                                "70-75",  "75-80","80+"), right=F)) %>% 
  droplevels()


# let´s have a look at the missing values. The doctor diagnosed disease is actually pretty robust,
# when thinking in terms of missing values. 

plot_missy<-df3 %>%
  # Select the survey items
  select(c("rdiabe_1", "rcancre_1","rlunge_1","rhearte_1","rstroke_1","rarthre_1","radlfive")) %>%
  # Create an UpSet plot
  gg_miss_upset(., nsets = 10 )


pdf(here("Gender_health", "Countries","EUROPE","Descriptive","missing_share.pdf"))
plot_missy
dev.off()


# check all variables  by gender:
pdf(here("Gender_health", "Countries","EUROPE","Descriptive","missing_share_gender.pdf"))
gg_miss_var(df3, ragender_2, show_pct = T)
dev.off()

# -----------------------------------------------------------------------------------------------------
# Construct a new variable for the chronic conditions/diseases
# We have constructed three new variables that capture prevalence of chronic conditions. 
# “chronic” refers to having at least one of the conditions cited above. “chronic_sum” refers to 
# the total number of conditions ever diagnosed. “chronic_severe” is a dummy variable that measures 
# where an individual is diagnosed with ever having had three or more of those conditions and we
# are associating this to a measure of severity of chronic disease or having experienced more than one
# condition through a lifetime.
# -----------------------------------------------------------------------------------------------------

# evaluate missings


df3_na<-df3 %>% 
  drop_na()

df3_var<-df3_na %>% 
  group_by(mergeid) %>% 
  mutate(chronic_free = case_when(rdiabe_1 ==0 & rcancre_1 == 0 &
                                    rlunge_1==0   & rhearte_1 ==  0 &
                                    rstroke_1==0 & rarthre_1 == 0  ~ 0, TRUE ~ 1),
         chronic_free_heart = case_when(rhearte_1 ==  0  ~ 0, TRUE ~ 1),
         chronic_free_diabe = case_when(rdiabe_1 ==0 ~ 0, TRUE ~ 1),
         chronic_free_cancre = case_when(rcancre_1 ==0 ~ 0, TRUE ~ 1),
         chronic_free_stroke = case_when(rstroke_1 ==0 ~ 0, TRUE ~ 1),
         chronic_free_arthre = case_when(rarthre_1 ==0 ~ 0, TRUE ~ 1),
         chronic_free_lunge = case_when(rlunge_1 ==0 ~ 0, TRUE ~ 1)) %>% 
  mutate(chronic_sum=sum(rdiabe_1,rcancre_1,rlunge_1,rhearte_1,rstroke_1,rarthre_1)) %>% 
  ungroup() %>% 
  group_by(mergeid) %>% 
  mutate(adl=case_when(radlfive ==0 ~ 0, radlfive >= 1 ~ 1),
         chronic_severe=case_when((chronic_sum == 0)  ~ 0,
                                  (chronic_sum >= 1 & chronic_sum <3) ~ 1,
                                  (chronic_sum >= 3 & chronic_sum <5) ~ 2,
                                  (chronic_sum >= 5 ~ 3)))

# now let us take make it a common dataframe

df3_var<-as.data.frame(df3_var)


df3_var$chronic_free<-as.character(df3_var$chronic_free)
df3_var$chronic_free_heart<-as.character(df3_var$chronic_free_heart)
df3_var$chronic_free_diabe<-as.character(df3_var$chronic_free_diabe)
df3_var$chronic_free_cancre<-as.character(df3_var$chronic_free_cancre)
df3_var$chronic_free_stroke<-as.character(df3_var$chronic_free_stroke)
df3_var$chronic_free_arthre<-as.character(df3_var$chronic_free_arthre)
df3_var$chronic_free_lunge<-as.character(df3_var$chronic_free_lunge)



df3_var$chronic_severe<-as.character(df3_var$chronic_severe)
df3_var$adl<-as.character(df3_var$adl)


df3_var$chronic_severe_2<-as.factor(df3_var$chronic_severe)
df3_var$chronic_severe_2<-factor(df3_var$chronic_severe, levels = c("0","1","2","3"), labels=c("no chronic",
                                                                                               ">=1 & <3 ",
                                                                                               ">= 3 & <5",
                                                                                               ">= 5") )

df3_var$ragender_2<-as.factor(df3_var$ragender_2)
df3_var$ragender_2 <- relevel(df3_var$ragender_2, ref = "woman")

df3_var$ragender_2<-factor(df3_var$ragender_2, levels = c("woman","man"), labels=c("woman","man") )

# converting to survey data to account for weights. SHARE provides individual-level or household weights 
# that account for non-response adjustment (rwtresp) and sample design weights.
# design_wgt is the sample design weight, to guarantee representiveness and
# account for non-response issues. rwtresp is the person-level analysis weight. SHARE uses a multistage
# stratified sample. We stratify by country - even though we can do pooled analysis as well.


df_share_surv <- svydesign( 
  ids = ~rwtsamp, 
  strata= ~country,
  weights = ~rwtresp,
  nest = T, 
  data = subset(df3_var, rwtresp > 0 & ragey>=50))            

#All selected respondents who are not age-eligible are assigned 0 as the weight.

# ages equal or above 50 because we only want the main individual and standardize across all countries

summary(df_share_surv)

# differences between weighted and unweighted when estimating proportions 

prop.table(table(df3$ragender_2))                        #non-weighted
prop.table(table(df3$age_cat))      
prop.table(svytable(~ragender_2, design=df_share_surv))    #weighted
prop.table(svytable(~age_cat, design=df_share_surv))

# Now accounting for more than one variable
svytable(~adl+ragey, design=df_share_surv)

svytable(~adl+age_cat+ragender_2, design=df_share_surv)

# table with summary statistics

df_share_surv %>% 
  tbl_svysummary(
    # Use include to select variables
    by= ragender_2,
    include = c(ragender_2,adl,ragey, raeducl_2,chronic_free,chronic_free_heart,chronic_free_diabe,
                chronic_free_arthre,chronic_free_lunge,chronic_free_stroke,chronic_free_cancre, chronic_severe_2),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list( all_categorical() ~ c(0, 1)),
    missing="no") %>%
  modify_header(label = "**Variable**") %>%
  add_p() %>% # comparing values by "both" column
  add_overall() %>%
  modify_caption("Weighted descriptive statistics") %>%
  bold_labels()#  %>% # uncomment here to save in the file the table. 
as_flex_table() %>%
flextable::save_as_docx(path=here("Gender_health","Countries","EUROPE","Descriptive","decriptive_share_pooled.docx"))

# now the same table for each country

df_share_surv %>% 
  tbl_svysummary(
    # Use include to select variables
    by= ragender_2,
    include = c(ragender_2,adl,ragey, raeducl_2,chronic_free,chronic_free_heart,chronic_free_diabe,
                chronic_free_arthre,chronic_free_lunge,chronic_free_stroke,chronic_free_cancre, chronic_severe_2,
                country),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list( all_categorical() ~ c(0, 1)),
    missing="no") %>%
  modify_header(label = "**Variable**") %>%
  add_p() %>% # comparing values by "both" column
  add_overall() %>%
  modify_caption("Weighted descriptive statistics") %>%
  bold_labels() # %>% # uncomment here to save in the file the table. 
as_flex_table() %>%
  flextable::save_as_docx(path=here("Gender_health","Countries","EUROPE","Descriptive","decriptive_share_countries.docx"))



#-----------------------------------------------------------------------------------#
# Pooled countries analysis
#-----------------------------------------------------------------------------------#

# Getting the proportions by ADLs
# proportions by age category with standard errors- here for all countries pooled together
# proportions by age category with standard errors. Here, we take the mean because taking the mean of a 
# variable coded 0/1 gives the proportions of 1s, so the mean of this variable is the estimated proportion
# of the population that is health or unhealthy. 


prop_healthy_adl<-svyby(formula = ~adl, by = ~age_cat+year+ragender_2, 
                        design = df_share_surv, FUN = svymean,vartype=c("ci"),
                        na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_adl<-as.data.frame(prop_healthy_adl)

colnames(prop_healthy_adl)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                              "CI_up_unhealthy")
prop_healthy_adl<-prop_healthy_adl[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_adl<-prop_healthy_adl %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_adl) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_adl,
       aes(age, unhealthy, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_adl,aes(age, CI_low_unhealthy, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_adl,aes(age, CI_up_unhealthy, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 



# saving this data as .csv file but you can change it if you prefer later

write.table(prop_healthy_adl, here("Gender_health","Data","SHARE" ,"Prevalence","prev_share_pooled_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")



#  Now for Chronic Conditions
# proportions by age category with standard errors.
# here for at least one chronic condition


prop_healthy_chron<-svyby(formula = ~chronic_free, by = ~age_cat+year+ragender_2, 
                          design = df_share_surv, FUN = svymean,vartype=c("ci"),
                          na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron<-as.data.frame(prop_healthy_chron)


colnames(prop_healthy_chron)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                "CI_up_unhealthy")
prop_healthy_chron<-prop_healthy_chron[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron<-prop_healthy_chron %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron,
       aes(age, unhealthy, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron,aes(age, CI_low_unhealthy, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron,aes(age, CI_up_unhealthy, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 



# saving this data as .csv file but you can change it if you prefer later

write.table(prop_healthy_chron, here("Gender_health","Data","SHARE","Prevalence", "prev_share_cron_pooled_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")


# now doing some sensitivity for different conditions

# here for being free from heart conditions, but not any other.

prop_healthy_chron_heart<-svyby(formula = ~chronic_free_heart, by = ~age_cat+year+ragender_2, 
                                design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_heart<-as.data.frame(prop_healthy_chron_heart)


colnames(prop_healthy_chron_heart)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                      "CI_up_unhealthy")
prop_healthy_chron_heart<-prop_healthy_chron_heart[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_heart<-prop_healthy_chron_heart %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_heart) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_heart,
       aes(age, unhealthy, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_heart,aes(age, CI_low_unhealthy, 
                                        group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_heart,aes(age, CI_up_unhealthy, 
                                        group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 
## gradient reverses.

# let us see arthrites: being free from arthiritis, but not any other.

prop_healthy_chron_arthre<-svyby(formula = ~chronic_free_arthre, by = ~age_cat+year+ragender_2, 
                                 design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                 na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_arthre<-as.data.frame(prop_healthy_chron_arthre)


colnames(prop_healthy_chron_arthre)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                       "CI_up_unhealthy")
prop_healthy_chron_arthre<-prop_healthy_chron_arthre[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_arthre<-prop_healthy_chron_arthre %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_arthre) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_arthre,
       aes(age, unhealthy, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_arthre,aes(age, CI_low_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_arthre,aes(age, CI_up_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 

# gradient goes back to female disadvantage

# cancer
# let us see cancer: being free from cancer, but not any other.

prop_healthy_chron_cancer<-svyby(formula = ~chronic_free_cancre, by = ~age_cat+year+ragender_2, 
                                 design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                 na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_cancer<-as.data.frame(prop_healthy_chron_cancer)


colnames(prop_healthy_chron_cancer)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                       "CI_up_unhealthy")
prop_healthy_chron_cancer<-prop_healthy_chron_cancer[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_cancer<-prop_healthy_chron_cancer %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_cancer) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_cancer,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_cancer,aes(age, CI_low_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_cancer,aes(age, CI_up_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 

#  Stroke
# being free from stroke, but not any other.

prop_healthy_chron_stroke<-svyby(formula = ~chronic_free_stroke, by = ~age_cat+year+ragender_2, 
                                 design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                 na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_stroke<-as.data.frame(prop_healthy_chron_stroke)


colnames(prop_healthy_chron_stroke)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                       "CI_up_unhealthy")
prop_healthy_chron_stroke<-prop_healthy_chron_stroke[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_stroke<-prop_healthy_chron_stroke %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_stroke) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_stroke,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_stroke,aes(age, CI_low_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_stroke,aes(age, CI_up_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 


#  lung disease
# being free from lung disease, but not any other.

prop_healthy_chron_lunge<-svyby(formula = ~chronic_free_lunge, by = ~age_cat+year+ragender_2, 
                                design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_lunge<-as.data.frame(prop_healthy_chron_lunge)


colnames(prop_healthy_chron_lunge)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                      "CI_up_unhealthy")
prop_healthy_chron_lunge<-prop_healthy_chron_lunge[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_lunge<-prop_healthy_chron_lunge%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_lunge) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_lunge,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_lunge,aes(age, CI_low_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_lunge,aes(age, CI_up_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 

#  diabetes
# being free from diabetes, but not any other.

prop_healthy_chron_diabe<-svyby(formula = ~chronic_free_diabe, by = ~age_cat+year+ragender_2, 
                                design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_diabe<-as.data.frame(prop_healthy_chron_diabe)


colnames(prop_healthy_chron_diabe)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                      "CI_up_unhealthy")
prop_healthy_chron_diabe<-prop_healthy_chron_diabe[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_diabe<-prop_healthy_chron_diabe%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_diabe) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_diabe,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_diabe,aes(age, CI_low_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_diabe,aes(age, CI_up_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 



# dummy of severity of conditions - 
# >=1 & <3 conditions

prop_healthy_chron_2<-svyby(formula = ~chronic_severe==1, by = ~age_cat+year+ragender_2, 
                            design = df_share_surv, FUN = svymean,vartype=c("ci"),
                            na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_2<-as.data.frame(prop_healthy_chron_2)


colnames(prop_healthy_chron_2)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                  "CI_up_unhealthy")
prop_healthy_chron_2<-prop_healthy_chron_2[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_2<-prop_healthy_chron_2%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_2) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_2,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_2,aes(age, CI_low_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_2,aes(age, CI_up_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 


# having >= 3 & <5

prop_healthy_chron_3<-svyby(formula = ~chronic_severe==2, by = ~age_cat+year+ragender_2, 
                            design = df_share_surv, FUN = svymean,vartype=c("ci"),
                            na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_3<-as.data.frame(prop_healthy_chron_3)


colnames(prop_healthy_chron_3)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                  "CI_up_unhealthy")
prop_healthy_chron_3<-prop_healthy_chron_3[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_3<-prop_healthy_chron_3%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_3) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_3,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_3,aes(age, CI_low_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_3,aes(age, CI_up_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 


# having 	>= 5

prop_healthy_chron_4<-svyby(formula = ~chronic_severe==3, by = ~age_cat+year+ragender_2, 
                            design = df_share_surv, FUN = svymean,vartype=c("ci"),
                            na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_4<-as.data.frame(prop_healthy_chron_4)


colnames(prop_healthy_chron_4)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                  "CI_up_unhealthy")
prop_healthy_chron_4<-prop_healthy_chron_4[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_4<-prop_healthy_chron_4%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_4) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_4,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_4,aes(age, CI_low_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_4,aes(age, CI_up_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988'))


# joining them all

prop_healthy_chron$type<-"at least one"
prop_healthy_chron_arthre$type<-"arthritis"
prop_healthy_chron_cancer$type<-"cancer"
prop_healthy_chron_heart$type<-"heart"
prop_healthy_chron_lunge$type<-"lung"
prop_healthy_chron_stroke$type<-"stroke"
prop_healthy_chron_diabe$type<-"diabetes"
prop_healthy_chron_diabe$type<-"diabetes"
prop_healthy_chron_2$type<-"one or two any condition"
prop_healthy_adl$type<-"adl"


prop_all_chronic<- rbind(prop_healthy_chron, prop_healthy_adl,prop_healthy_chron_arthre, prop_healthy_chron_cancer, prop_healthy_chron_diabe,
                         prop_healthy_chron_heart, prop_healthy_chron_lunge, prop_healthy_chron_stroke,prop_healthy_chron_2)

prop_all_chronic$country<-"Europe"




X11()
fig_prev_all<- ggplot(prop_all_chronic,
                      aes(age, unhealthy, group=gender,color=gender,shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_all_chronic,aes(age, CI_low_unhealthy, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_all_chronic,aes(age, CI_up_unhealthy, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 14)+
  facet_wrap(.~type)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 


pdf(here("Gender_health", "Countries","EUROPE","Descriptive","fig_prev_cron_share.pdf"), width = 10, height=10)
fig_prev_all
dev.off()


# save this dataset as well.

write.table(prop_all_chronic, here("Gender_health","Data","SHARE","Prevalence", "prev_share_cron_type_pooled_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")

#-----------------------------------------------------------------------------------#
# country specific analysis
#-----------------------------------------------------------------------------------#

# ADLs
# proportions by age category with standard errors by country

prop_healthy_adl_country<-svyby(formula = ~adl==0, by = ~age_cat+year+ragender_2+country, 
                            design = df_share_surv, FUN = svymean,vartype=c("ci"),
                            na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_adl_country<-as.data.frame(prop_healthy_adl_country)
colnames(prop_healthy_adl_country)<-c("age","year","gender","country","unhealthy","healthy","CI_low_unhealthy",
                                       "CI_low_healthy","CI_up_unhealthy", "CI_up_healthy")
prop_healthy_adl_country<-prop_healthy_adl_country[, c(1:6,7,9,8,10)]


# Grouping information and checking
# first age groups
prop_healthy_adl_country<-prop_healthy_adl_country %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_adl_country) <- NULL

X11()
# Simple graph to check gender with CI
fig_prev_adl_country<-ggplot(prop_healthy_adl_country,
       aes(age, country, group=country, fill=unhealthy))+
  geom_raster(interpolate = T) +
  theme_clean(base_size = 14)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=10),
        legend.text = element_text(size=10))+
  facet_grid(gender~.)+
  scale_fill_distiller(palette = "Spectral", name="%Unhealthy", n.breaks=4)

pdf(here("Gender_health", "Countries","EUROPE","Descriptive","fig_prev_adl_country.pdf"), width = 8, height=10)
fig_prev_adl_country
dev.off()

# saving this data as .csv file but you can change it if you prefer later

write.table(prop_healthy_adl_country, here("Gender_health","Data","SHARE","Prevalence", "prev_share_country_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")



#  Now for Chronic Conditions
# proportions by age category with standard errors.
# here for at least one chronic condition


prop_healthy_chron_country<-svyby(formula = ~chronic_free==1, by = ~age_cat+year+ragender_2+country, 
                          design = df_share_surv, FUN = svymean,vartype=c("ci"),
                          na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_country<-as.data.frame(prop_healthy_chron_country)


colnames(prop_healthy_chron_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                "CI_up_unhealthy")
prop_healthy_chron_country<-prop_healthy_chron_country[, c(1:7, 9, 8,10 )]


# Grouping information and checking
# first age groups
prop_healthy_chron_country<-prop_healthy_chron_country %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_country) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_country,
       aes(age, unhealthy, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_country,aes(age, CI_low_unhealthy, 
                                        group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_country,aes(age, CI_up_unhealthy, 
                                        group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
   facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 



# saving this data as .csv file but you can change it if you prefer later

write.table(prop_healthy_chron_country, here("Gender_health","Data","SHARE","Prevalence", "prev_share_cron_country_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")


# now doing some sensitivity for different conditions

# here for being free from heart conditions, but not any other.

prop_healthy_chron_heart_country<-svyby(formula = ~chronic_free_heart==1, by = ~age_cat+year+ragender_2+country, 
                                design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_heart_country<-as.data.frame(prop_healthy_chron_heart_country)


colnames(prop_healthy_chron_heart_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                        "CI_up_unhealthy")
prop_healthy_chron_heart_country<-prop_healthy_chron_heart_country[, c(1:7, 9, 8,10 )]

# Grouping information and checking
# first age groups
prop_healthy_chron_heart_country<-prop_healthy_chron_heart_country %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_heart_country) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_heart_country,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_heart_country,aes(age, CI_low_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_heart_country,aes(age, CI_up_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
   facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 
## gradient reverses.

# let us see arthrites: being free from arthiritis, but not any other.

prop_healthy_chron_arthre_country<-svyby(formula = ~chronic_free_arthre==1, by = ~age_cat+year+ragender_2+country, 
                                 design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                 na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_arthre_country<-as.data.frame(prop_healthy_chron_arthre_country)


colnames(prop_healthy_chron_arthre_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                       "CI_up_unhealthy")
prop_healthy_chron_arthre_country<-prop_healthy_chron_arthre_country[, c(1:7, 9, 8,10 )]


# Grouping information and checking
# first age groups
prop_healthy_chron_arthre_country<-prop_healthy_chron_arthre_country %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_arthre_country) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_arthre_country,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_arthre_country,aes(age, CI_low_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_arthre_country,aes(age, CI_up_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 

# gradient goes back to female disadvantage

# cancer
# let us see cancer: being free from cancer, but not any other.

prop_healthy_chron_cancer_country<-svyby(formula = ~chronic_free_cancre==1, by = ~age_cat+year+ragender_2+country, 
                                 design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                 na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_cancer_country<-as.data.frame(prop_healthy_chron_cancer_country)


colnames(prop_healthy_chron_cancer_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                       "CI_up_unhealthy")
prop_healthy_chron_cancer_country<-prop_healthy_chron_cancer_country[, c(1:7, 9, 8,10 )]


# Grouping information and checking
# first age groups
prop_healthy_chron_cancer_country<-prop_healthy_chron_cancer_country %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_cancer_country) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_cancer_country,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_cancer_country,aes(age, CI_low_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_cancer_country,aes(age, CI_up_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
   facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 

#  Stroke
# being free from stroke, but not any other.

prop_healthy_chron_stroke_country<-svyby(formula = ~chronic_free_stroke==1, by = ~age_cat+year+ragender_2+country, 
                                 design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                 na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_stroke_country<-as.data.frame(prop_healthy_chron_stroke_country)


colnames(prop_healthy_chron_stroke_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                       "CI_up_unhealthy")
prop_healthy_chron_stroke_country<-prop_healthy_chron_stroke_country[, c(1:7, 9, 8,10 )]


# Grouping information and checking
# first age groups
prop_healthy_chron_stroke_country<-prop_healthy_chron_stroke_country %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_stroke_country) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_stroke_country,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_stroke_country,aes(age, CI_low_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_stroke_country,aes(age, CI_up_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 


#  lung disease
# being free from lung disease, but not any other.

prop_healthy_chron_lunge_country<-svyby(formula = ~chronic_free_lunge==1, by = ~age_cat+year+ragender_2+country, 
                                design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_lunge_country<-as.data.frame(prop_healthy_chron_lunge_country)


colnames(prop_healthy_chron_lunge_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                      "CI_up_unhealthy")
prop_healthy_chron_lunge_country<-prop_healthy_chron_lunge_country[, c(1:7,9,8,10)]


# Grouping information and checking
# first age groups
prop_healthy_chron_lunge_country<-prop_healthy_chron_lunge_country%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_lunge_country) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_lunge_country,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_lunge_country,aes(age, CI_low_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_lunge_country,aes(age, CI_up_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 

#  diabetes
# being free from diabetes, but not any other.

prop_healthy_chron_diabe_country<-svyby(formula = ~chronic_free_diabe==1, by = ~age_cat+year+ragender_2+country, 
                                design = df_share_surv, FUN = svymean,vartype=c("ci"),
                                na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_diabe_country<-as.data.frame(prop_healthy_chron_diabe_country)


colnames(prop_healthy_chron_diabe_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                      "CI_up_unhealthy")
prop_healthy_chron_diabe_country<-prop_healthy_chron_diabe_country[, c(1:7,9,8,10)]


# Grouping information and checking
# first age groups
prop_healthy_chron_diabe_country<-prop_healthy_chron_diabe_country%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_diabe_country) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_diabe_country,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_diabe_country,aes(age, CI_low_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_diabe_country,aes(age, CI_up_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 



# dummy of severity of conditions - 
# >=1 & <3 conditions

prop_healthy_chron_2_country<-svyby(formula = ~chronic_severe==1, by = ~age_cat+year+ragender_2+country, 
                            design = df_share_surv, FUN = svymean,vartype=c("ci"),
                            na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_2_country<-as.data.frame(prop_healthy_chron_2_country)


colnames(prop_healthy_chron_2_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                  "CI_up_unhealthy")
prop_healthy_chron_2_country<-prop_healthy_chron_2_country[, c(1:7,9,8,10)]


# Grouping information and checking
# first age groups
prop_healthy_chron_2_country<-prop_healthy_chron_2_country%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_2_country) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_2_country,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_2_country,aes(age, CI_low_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_2_country,aes(age, CI_up_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 


# having >= 3 & <5

prop_healthy_chron_3_country<-svyby(formula = ~chronic_severe==2, by = ~age_cat+year+ragender_2+country, 
                            design = df_share_surv, FUN = svymean,vartype=c("ci"),
                            na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_3_country<-as.data.frame(prop_healthy_chron_3_country)


colnames(prop_healthy_chron_3_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                  "CI_up_unhealthy")
prop_healthy_chron_3_country<-prop_healthy_chron_3_country[, c(1:7,9,8,10)]


# Grouping information and checking
# first age groups
prop_healthy_chron_3_country<-prop_healthy_chron_3_country%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_3_country) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_3_country,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_3_country,aes(age, CI_low_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_3_country,aes(age, CI_up_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 


# having 	>= 5

prop_healthy_chron_4_country<-svyby(formula = ~chronic_severe==3, by = ~age_cat+year+ragender_2+country, 
                            design = df_share_surv, FUN = svymean,vartype=c("ci"),
                            na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_4_country<-as.data.frame(prop_healthy_chron_4_country)


colnames(prop_healthy_chron_4_country)<-c("age","year","gender","country","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                  "CI_up_unhealthy")
prop_healthy_chron_4_country<-prop_healthy_chron_4_country[, c(1:7,9,8,10)]


# Grouping information and checking
# first age groups
prop_healthy_chron_4_country<-prop_healthy_chron_4_country%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_4_country) <- NULL

# Simple graph to check gender with CI 
# no good to make this distinction by country because the sample size is very small.
ggplot(prop_healthy_chron_4_country,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron_4_country,aes(age, CI_low_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_4_country,aes(age, CI_up_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_wrap(.~country)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988'))


# joining them all

prop_healthy_chron_country$type<-"at least one"
prop_healthy_chron_arthre_country$type<-"arthritis"
prop_healthy_chron_cancer_country$type<-"cancer"
prop_healthy_chron_heart_country$type<-"heart"
prop_healthy_chron_lunge_country$type<-"lung"
prop_healthy_chron_stroke_country$type<-"stroke"
prop_healthy_chron_diabe_country$type<-"diabetes"
prop_healthy_chron_2_country$type<-"one or two any condition"
prop_healthy_adl_country$type<-"adl"


prop_all_chronic_country<- rbind(prop_healthy_chron_country, prop_healthy_adl_country,prop_healthy_chron_arthre_country,
                                 prop_healthy_chron_cancer_country, prop_healthy_chron_diabe_country,
                         prop_healthy_chron_heart_country, prop_healthy_chron_lunge_country,
                         prop_healthy_chron_stroke_country,prop_healthy_chron_2_country)

X11()


fig_prev_all_country<-
  ggplot(prop_all_chronic_country,
                             aes(age, country, group=country, fill=unhealthy))+
  geom_raster(interpolate = F) +
  #  geom_tile (color="grey80") +
  theme_clean(base_size = 14)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=10),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 90))+
  facet_grid(gender~type)+
  scale_fill_distiller(palette = "Spectral", name="%Unhealthy", n.breaks=4)


pdf(here("Gender_health", "Countries","EUROPE","Descriptive","fig_prev_cron_country.pdf"), width = 18, height=8)
fig_prev_all_country
dev.off()

#
ggplot(prop_all_chronic_country, aes(x = age, y=unhealthy,fill = type)) +
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "BrBG") + 
  labs(title = "BrBG")+
  facet_grid(gender~country)


# save this dataset as well.

write.table(prop_all_chronic_country, here("Gender_health","Data","SHARE","Prevalence", "prev_share_cron_type_country_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")





# some models
library(ggeffects)

mod1<-svyglm(as.numeric(adl)~age_cat+ragender_2, design= df_share_surv, family = binomial)
summary(mod1)


mod_1<-ggeffect(mod1, terms = c("age_cat", "ragender_2"))  %>%
  plot() 

mod2<-svyglm(as.numeric(adl)~age_cat+ragender_2+raeducl_2, design= df_share_surv, family = binomial)
summary(mod2)


ggeffect(mod2, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


mod3<-svyglm(as.numeric(chronic_free)~age_cat+ragender_2, design= df_share_surv, family = binomial)

ggeffect(mod3, terms = c("age_cat", "ragender_2"))  %>%
  plot() 
summary(mod3)


mod4<-svyglm(as.numeric(chronic_free_heart)~age_cat+ragender_2, design= df_share_surv, family = binomial)
summary(mod4)


ggeffect(mod4, terms = c("age_cat", "ragender_2"))  %>%
  plot() 


mod5<-svyglm(as.numeric(chronic_free_heart)~age_cat+ragender_2+raeducl_2, design= df_share_surv, family = binomial)
summary(mod5)

ggeffect(mod5, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


mod6<-svyglm(as.numeric(chronic_free_arthre)~age_cat+ragender_2, design= df_share_surv, family = binomial)
summary(mod6)

ggeffect(mod6, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


#mod7<-svy_vglm(as.numeric(chronic_severe)~age_cat+ragender_2, design= df_hrs_surv, 
#               family = multinomial)
#summary(mod7)


mod7<-svyglm(as.numeric(chronic_sum)~age_cat+ragender_2, design= df_share_surv)
summary(mod7)

ggeffect(mod7, terms = c("age_cat", "ragender_2"))  %>%
  plot() 









