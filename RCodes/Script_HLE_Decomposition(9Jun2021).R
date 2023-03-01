


# Rates by city (Fev 2020)
# Updated figures 24/09/2019
# figures Black and White



## cleaning the workspace
rm(list=ls(all=TRUE))

library(tidyr)
library(dplyr)
library(reshape2)
library(DemoDecomp) 
library(ggplot2)
library(gridExtra)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions
#-----------------------------------------------------------------------------------------------
start.age = 60
open.age = 85

Sullivan.fun = function (rates,age=seq(start.age,open.age,5)) {
  # 1) First, we split from our single vector 'rates' the set of age-specific death rates (mx) 
  # and age-specific prevalence of disability (wx)
  lengthvec <- length(rates)
  mx <- rates[1:(lengthvec / 2)]
  wx <- rates[(lengthvec / 2 + 1):lengthvec]
  # 2) Calculating period life table functions
  # ax
  n <- c(diff(age), 10)
  ax <- 0.5 * n
  # probability of dying (qx) and surviving (px)
  qx <- (n * mx)/(1 + (n - ax) * mx)
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  px <- 1 - qx
  # survivors at age x (lx)
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths between ages x and x+n (dx)
  dx <- lx * qx
  # person-years lived between ages x and x+n (Lx)
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  Tx  <- rev(cumsum(rev(Lx)))
  ex = Tx/lx
  # 3) getting the person-years lived with health (without disability) between ages x and x+n
  Lx.health <-  Lx*(1-wx)
  Tx.health  <- rev(cumsum(rev(Lx.health)))
  # 4) healthy life expectancy 
  ex.health <- Tx.health/lx
  return.df <- data.frame(age, n, ax, qx, px, lx, dx, Lx, Tx,ex,Lx.health, Tx.health, ex.health)
  return(ex.health[1])
} 

# Sullivan function to generate the confidence intervals
Sullivan.CI = function (mx,wx,age=seq(start.age,open.age,5)) {
  # 1) First, we split from our single vector 'rates' the set of age-specific death rates (mx) 
  # and age-specific prevalence of disability (wx)
  lengthvec <- length(mx)
  mx <- mx
  wx <- wx
  # 2) Calculating period life table functions
  # ax
  n <- c(diff(age), 10)
  ax <- 0.5 * n
  # probability of dying (qx) and surviving (px)
  qx <- (n * mx)/(1 + (n - ax) * mx)
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  px <- 1 - qx
  # survivors at age x (lx)
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths between ages x and x+n (dx)
  dx <- lx * qx
  # person-years lived between ages x and x+n (Lx)
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  Tx  <- rev(cumsum(rev(Lx)))
  ex = Tx/lx
  # 3) getting the person-years lived with health (without disability) between ages x and x+n
  Lx.health <-  Lx*(1-wx)
  Tx.health  <- rev(cumsum(rev(Lx.health)))
  # 4) healthy life expectancy 
  ex.health <- Tx.health/lx
  return.df <- data.frame(age, n, ax, qx, px, lx, dx, Lx, Tx,ex,wx,Lx.health, Tx.health, ex.health)
  return(return.df)
} 

## function for constructing a lifetable starting from probabilities
lifetable.qx.wx <- function(x, qx, wx){
  m <- length(x)
  # ax
  n <- c(diff(x), 10)
  ax <- 0.5 * n
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]*ax[length(x)]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Lx.health <-  Lx*(1-wx)
  Tx  <- rev(cumsum(rev(Lx)))
  Tx.health  <- rev(cumsum(rev(Lx.health)))
  ex  <- Tx/lx
  ex.health  <- Tx.health/lx
  return.df <- data.frame(x, ax, qx, px, lx, dx, Lx, Tx, ex,wx,Lx.health, Tx.health, ex.health)
  return(return.df)
}

# function to calculate CI
CIex <- function(x, Nx, Dx, Rx,mx,wx, which.x, ns, level){
  ## point-estimated lifetable
  LT <- Sullivan.CI(mx,wx)
  ## number of ages
  m <- nrow(LT)
  ## estimated probs
  qx <- LT$qx
  ## estimated prevs
  wx <- LT$wx
  ## trials for binomial, rounded
  Ntil <- round(Dx/qx)
  Ntil.prev <- round(Rx/wx) # Rx is the number of respondents and wx the prevalence of disability
  ## ax for last age
  last.ax <- LT$ax[m]
  ## simulated death counts
  ## from Binomial distribution
  Y <- suppressWarnings(matrix(rbinom(m*ns,
                                      Ntil,
                                      qx),
                               m,ns))
  ## simulated number of unhealth individuals
  ## from Binomial distribution
  Z <- suppressWarnings(matrix(rbinom(m*ns,
                                      Ntil.prev,
                                      wx),
                               m,ns))
  ## simulated probabilities
  QX <- Y/Ntil
  ## simulated prevalences
  WX <- Z/Ntil.prev
  
  ## which age?
  wh <- which(x==which.x)
  fun.ex <- function(qx){
    return(lifetable.qx.wx(x=x, qx, wx)$ex[wh])
  }
  fun.ex.health <- function(wx){
    return(lifetable.qx.wx(x=x, qx, wx)$ex.health[wh])
  }  
  exsim <- apply(QX, 2, fun.ex)
  exsim.health <- apply(WX, 2, fun.ex.health)
  
  ## confidence interval
  CI <- quantile(exsim,
                 probs = c((1-level)/2,
                           1 - (1-level)/2))
  CI.health <- quantile(exsim.health,
                        probs = c((1-level)/2,
                                  1 - (1-level)/2))
  CI.Wx <- quantile(WX,
                    probs = c((1-level)/2,
                              1 - (1-level)/2))
  ## output
  out <- list(ex=LT$ex[wh],
              meanex=mean(exsim),
              CIex=CI,
              exsim=exsim,
              ex.health=LT$ex.health[wh],
              meanex.health=mean(exsim.health),
              CIex.health=CI.health,
              which.x=which.x,
              WX.mean=mean(WX),
              CI.Wx = CI.Wx,
              Y=Y,
              Z=Z,
              exsim=exsim,
              exsim.health=exsim.health,
              WX=WX)
  return(out)
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SABE data

#Argentina 
#Brazil
#Chile 
#Mexico 
#Uruguay 
#BARBADOS

## Reading and organizing the data

#-----------------------------------------------------------------------
# Mortality

#mx.f = read.table("U:\\ownCloud\\1-Projetos_Marilia\\Nepomuceno&Turra\\Decomposition_sex_differencesHLE_LA\\Data\\LT\\mx.femaleCelade.txt",header=TRUE)
#mx.m = read.table("U:\\ownCloud\\1-Projetos_Marilia\\Nepomuceno&Turra\\Decomposition_sex_differencesHLE_LA\\Data\\LT\\mx.maleCelade.txt",header=TRUE)

#setwd("C:\\Users\\mpidr\\Nextcloud\\")

setwd("/Users/marilianepomuceno/Nextcloud/")

mx.f = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/LT/Cities/mx.femaleCities.txt",header=TRUE)
mx.m = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/LT/Cities/mx.maleCities.txt",header=TRUE)

dx.f = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/LT/Cities/dx.female.txt",header=TRUE)
dx.m = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/LT/Cities/dx.male.txt",header=TRUE)

Lx.f = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/LT/Cities/Lx.female.txt",header=TRUE)
Lx.m = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/LT/Cities/Lx.male.txt",header=TRUE)  

ax.f = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/LT/Cities/ax.female.txt",header=TRUE)
ax.m = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/LT/Cities/ax.male.txt",header=TRUE)  

#-----------------------------------------------------------------------
# SABE
# creating 5-year age group
labs <- c(paste(seq(60, 80, by = 5), seq(60 + 5 - 1, 85 - 1, by = 5),
                sep = "-"), paste(85, "+", sep = ""))
labs

#-----------------
#Argentina 
AR = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/SABE2/AR.csv",header=TRUE, sep= ",")
AR = AR %>% drop_na()%>% filter(AGEYRS>= 60)
AR$AgeGroup <- cut(AR$AGEYRS, breaks = c(seq(60, 85, by = 5), Inf), labels = labs, right = FALSE)
head(AR)
summary(AR)

AR$Disable = NULL
for ( i in 1:dim(AR)[1]){
  if( AR$D11[i]==1 |AR$D13A[i]==1 |AR$D14A[i]==1 |AR$D15A[i]==1 |AR$D17A[i]==1 |AR$D18A[i]==1){
    AR$Disable[i]=1}
    else
      {AR$Disable[i]=2}
}

# sum all ages
AR_allages = AR %>%
  group_by(SEX, Disable) %>%  
  summarise (n = n())

# total respondents by age and sex
AR_tot = AR %>%
  group_by(AgeGroup, SEX) %>%
  summarise (n = n()) 
AR_tot<- dcast(AR_tot, AgeGroup ~ SEX, value.var="n")
colnames(AR_tot) = c('AgeGroup','Female','Male')

AR = AR %>%
  group_by(AgeGroup, SEX, Disable) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

AR = AR %>%  filter(Disable== 1) # disable = 1 = Yes
AR<- dcast(AR, AgeGroup ~ SEX, value.var="freq")
colnames(AR) = c('AgeGroup','Female','Male')


AR_se = matrix(ncol=2,nrow=6)
AR_se[,1] = sqrt((AR$Female*(1-AR$Female))/AR_tot$Female )
AR_se[,2] = sqrt((AR$Male*(1-AR$Male))/AR_tot$Male )


AR_CI.l = matrix(ncol=2,nrow=6)
AR_CI.l[,1] = AR$Female-1.96* AR_se[,1]
AR_CI.l[,2] = AR$Male-1.96* AR_se[,2]

AR_CI.u = matrix(ncol=2,nrow=6)
AR_CI.u[,1] = AR$Female+1.96* AR_se[,1]
AR_CI.u[,2] = AR$Male+1.96* AR_se[,2]

colnames(AR_se)=colnames(AR_CI.l)=colnames(AR_CI.u)=c("Female","Male")

#-----------------
#Brazil 
BR = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/SABE2/BR.csv",header=TRUE, sep= ",")
BR = BR %>% drop_na()%>% filter(AGEYRS>= 60)
BR$AgeGroup <- cut(BR$AGEYRS, breaks = c(seq(60, 85, by = 5), Inf), labels = labs, right = FALSE)
head(BR)
summary(BR)

BR$Disable = NULL
for ( i in 1:dim(BR)[1]){
  if(BR$D11[i]==1 |BR$D13A[i]==1 |BR$D14A[i]==1 |BR$D15A[i]==1 |BR$D17A[i]==1 |BR$D18A[i]==1){
    BR$Disable[i]=1}
  else
  {BR$Disable[i]=2}
}

# sum all ages
BR_allages = BR %>%
  group_by(SEX, Disable) %>%  
  summarise (n = n())

# total respondents by age and sex
BR_tot = BR %>%
  group_by(AgeGroup, SEX) %>%
  summarise (n = n()) 
BR_tot<- dcast(BR_tot, AgeGroup ~ SEX, value.var="n")
colnames(BR_tot) = c('AgeGroup','Female','Male')


BR = BR %>%
  group_by(AgeGroup, SEX, Disable) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

BR = BR %>%  filter(Disable== 1) # disable = 1 = Yes
BR<- dcast(BR, AgeGroup ~ SEX, value.var="freq")
colnames(BR) = c('AgeGroup','Female','Male')

BR_se = matrix(ncol=2,nrow=6)
BR_se[,1] = sqrt((BR$Female*(1-BR$Female))/BR_tot$Female )
BR_se[,2] = sqrt((BR$Male*(1-BR$Male))/BR_tot$Male )


BR_CI.l = matrix(ncol=2,nrow=6)
BR_CI.l[,1] = BR$Female-1.96* BR_se[,1]
BR_CI.l[,2] = BR$Male-1.96* BR_se[,2]

BR_CI.u = matrix(ncol=2,nrow=6)
BR_CI.u[,1] = BR$Female+1.96* BR_se[,1]
BR_CI.u[,2] = BR$Male+1.96* BR_se[,2]

colnames(BR_se)=colnames(BR_CI.l)=colnames(BR_CI.u)=c("Female","Male")


#-----------------
#Chile 
CH = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/SABE2/CH.csv",header=TRUE, sep= ",")
CH = CH %>% drop_na()%>% filter(AGEYRS>= 60)
CH$AgeGroup <- cut(CH$AGEYRS, breaks = c(seq(60, 85, by = 5), Inf), labels = labs, right = FALSE)
head(CH)
summary(CH)

CH$Disable = NULL
for ( i in 1:dim(CH)[1]){
  if( CH$D11[i]==1 |CH$D13A[i]==1 |CH$D14A[i]==1 |CH$D15A[i]==1 |CH$D17A[i]==1 |CH$D18A[i]==1){
    CH$Disable[i]=1}
  else
  {CH$Disable[i]=2}
}

# sum all ages
CH_allages = CH %>%
  group_by(SEX, Disable) %>%  
  summarise (n = n())

# total respondents by age and sex
CH_tot = CH %>%
  group_by(AgeGroup, SEX) %>%
  summarise (n = n()) 
CH_tot<- dcast(CH_tot, AgeGroup ~ SEX, value.var="n")
colnames(CH_tot) = c('AgeGroup','Female','Male')

CH = CH %>%
  group_by(AgeGroup, SEX, Disable) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

CH = CH %>%  filter(Disable== 1) # disable = 1 = Yes
CH<- dcast(CH, AgeGroup ~ SEX, value.var="freq")
colnames(CH) = c('AgeGroup','Female','Male')

CH_se = matrix(ncol=2,nrow=6)
CH_se[,1] = sqrt((CH$Female*(1-CH$Female))/CH_tot$Female )
CH_se[,2] = sqrt((CH$Male*(1-CH$Male))/CH_tot$Male )


CH_CI.l = matrix(ncol=2,nrow=6)
CH_CI.l[,1] = CH$Female-1.96* CH_se[,1]
CH_CI.l[,2] = CH$Male-1.96* CH_se[,2]

CH_CI.u = matrix(ncol=2,nrow=6)
CH_CI.u[,1] = CH$Female+1.96* CH_se[,1]
CH_CI.u[,2] = CH$Male+1.96* CH_se[,2]

colnames(CH_se)=colnames(CH_CI.l)=colnames(CH_CI.u)=c("Female","Male")

#-----------------
#Mexico 
MX = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/SABE2/MX.csv",header=TRUE, sep= ",")
MX = MX %>% drop_na()  %>% filter(AGEYRS>= 60)
MX $AgeGroup <- cut(MX$AGEYRS, breaks = c(seq(60, 85, by = 5), Inf), labels = labs, right = FALSE)
head(MX)
summary(MX)

MX$Disable = NULL
for ( i in 1:dim(MX)[1]){
  if( MX$D11[i]==1 |MX$D13A[i]==1 |MX$D14A[i]==1 |MX$D15A[i]==1 |MX$D17A[i]==1 |MX$D18A[i]==1){
    MX$Disable[i]=1}
  else
  {MX$Disable[i]=2}
}

# sum all ages
MX_allages = MX %>%
  group_by(SEX, Disable) %>%  
  summarise (n = n())

# total respondents by age and sex
MX_tot = MX %>%
  group_by(AgeGroup, SEX) %>%
  summarise (n = n()) 
MX_tot<- dcast(MX_tot, AgeGroup ~ SEX, value.var="n")
colnames(MX_tot) = c('AgeGroup','Female','Male')

MX = MX %>%
  group_by(AgeGroup, SEX, Disable) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

MX = MX %>%  filter(Disable== 1) # disable = 1 = Yes
MX<- dcast(MX, AgeGroup ~ SEX, value.var="freq")
colnames(MX) = c('AgeGroup','Female','Male')


MX_se = matrix(ncol=2,nrow=6)
MX_se[,1] = sqrt((MX$Female*(1-MX$Female))/MX_tot$Female )
MX_se[,2] = sqrt((MX$Male*(1-MX$Male))/MX_tot$Male )


MX_CI.l = matrix(ncol=2,nrow=6)
MX_CI.l[,1] = MX$Female-1.96* MX_se[,1]
MX_CI.l[,2] = MX$Male-1.96* MX_se[,2]

MX_CI.u = matrix(ncol=2,nrow=6)
MX_CI.u[,1] = MX$Female+1.96* MX_se[,1]
MX_CI.u[,2] = MX$Male+1.96* MX_se[,2]

colnames(MX_se)=colnames(MX_CI.l)=colnames(MX_CI.u)=c("Female","Male")


#-----------------
# Uruguay
UR = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/SABE2/UR.csv",header=TRUE, sep= ",")
UR = UR %>% drop_na()%>% filter(AGEYRS>= 60)
UR$AgeGroup <- cut(UR$AGEYRS, breaks = c(seq(60, 85, by = 5), Inf), labels = labs, right = FALSE)
head(UR)
summary(UR)

UR$Disable = NULL
for ( i in 1:dim(UR)[1]){
  if( UR$D11[i]==1 |UR$D13A[i]==1 |UR$D14A[i]==1 |UR$D15A[i]==1 |UR$D17A[i]==1 |UR$D18A[i]==1){
    UR$Disable[i]=1}
  else
  {UR$Disable[i]=2}
}

# sum all ages
UR_allages = UR %>%
  group_by(SEX, Disable) %>%  
  summarise (n = n())

# total respondents by age and sex
UR_tot = UR %>%
  group_by(AgeGroup, SEX) %>%
  summarise (n = n()) 
UR_tot<- dcast(UR_tot, AgeGroup ~ SEX, value.var="n")
colnames(UR_tot) = c('AgeGroup','Female','Male')

UR = UR %>%
  group_by(AgeGroup, SEX, Disable) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

UR = UR %>%  filter(Disable== 1) # disable = 1 = Yes
UR<- dcast(UR, AgeGroup ~ SEX, value.var="freq")
colnames(UR) = c('AgeGroup','Female','Male')


UR_se = matrix(ncol=2,nrow=6)
UR_se[,1] = sqrt((UR$Female*(1-UR$Female))/UR_tot$Female )
UR_se[,2] = sqrt((UR$Male*(1-UR$Male))/UR_tot$Male )


UR_CI.l = matrix(ncol=2,nrow=6)
UR_CI.l[,1] = UR$Female-1.96* UR_se[,1]
UR_CI.l[,2] = UR$Male-1.96* UR_se[,2]

UR_CI.u = matrix(ncol=2,nrow=6)
UR_CI.u[,1] = UR$Female+1.96* UR_se[,1]
UR_CI.u[,2] = UR$Male+1.96* UR_se[,2]

colnames(UR_se)=colnames(UR_CI.l)=colnames(UR_CI.u)=c("Female","Male")

#-----------------
# CUBA
CB = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/SABE2/CB.csv",header=TRUE, sep= ",")
CB = CB %>% drop_na()%>% filter(AGEYRS>= 60)
CB$AgeGroup <- cut(CB$AGEYRS, breaks = c(seq(60, 85, by = 5), Inf), labels = labs, right = FALSE)
head(CB)
summary(CB)

CB$Disable = NULL
for ( i in 1:dim(CB)[1]){
  if( CB$D11[i]==1 |CB$D13A[i]==1 |CB$D14A[i]==1 |CB$D15A[i]==1 |CB$D17A[i]==1 |CB$D18A[i]==1){
    CB$Disable[i]=1}
  else
  {CB$Disable[i]=2}
}

# sum all ages
CB_allages = CB %>%
 group_by(SEX, Disable) %>%  
 summarise (n = n())

# total respondents by age and sex
CB_tot = CB %>%
  group_by(AgeGroup, SEX) %>%
  summarise (n = n()) 
CB_tot<- dcast(CB_tot, AgeGroup ~ SEX, value.var="n")
colnames(CB_tot) = c('AgeGroup','Female','Male')

CB = CB %>%
  group_by(AgeGroup, SEX, Disable) %>% 
  summarise (n = n()) %>%
  mutate(total= sum(n))%>%
  mutate(freq = n / sum(n))

CB = CB %>%  filter(Disable== 1) # disable = 1 = Yes
CB<- dcast(CB, AgeGroup ~ SEX, value.var="freq")
colnames(CB) = c('AgeGroup','Female','Male')

CB_se = matrix(ncol=2,nrow=6)
CB_se[,1] = sqrt((CB$Female*(1-CB$Female))/CB_tot$Female )
CB_se[,2] = sqrt((CB$Male*(1-CB$Male))/CB_tot$Male )


CB_CI.l = matrix(ncol=2,nrow=6)
CB_CI.l[,1] = CB$Female-1.96* CB_se[,1]
CB_CI.l[,2] = CB$Male-1.96* CB_se[,2]

CB_CI.u = matrix(ncol=2,nrow=6)
CB_CI.u[,1] = CB$Female+1.96* CB_se[,1]
CB_CI.u[,2] = CB$Male+1.96* CB_se[,2]

colnames(CB_se)=colnames(CB_CI.l)=colnames(CB_CI.u)=c("Female","Male")


#-----------------
#Barbados 
BA = read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/SABE2/BA.csv",header=TRUE, sep= ",")
BA = BA %>% drop_na()%>% filter(AGEYRS>= 60)
BA$AgeGroup <- cut(BA$AGEYRS, breaks = c(seq(60, 85, by = 5), Inf), labels = labs, right = FALSE)
head(BA)
summary(BA)

BA$Disable = NULL
for ( i in 1:dim(BA)[1]){
  if(BA$D11[i]==1 |BA$D13A[i]==1 |BA$D14A[i]==1 |BA$D15A[i]==1 |BA$D17A[i]==1 |BA$D18A[i]==1){
    BA$Disable[i]=1}
  else
  {BA$Disable[i]=2}
}

# sum all ages
BA_allages = BA %>%
  group_by(SEX, Disable) %>%  
  summarise (n = n())

# total respondents by age and sex
BA_tot = BA%>%
  group_by(AgeGroup, SEX) %>%
  summarise (n = n()) 
BA_tot<- dcast(BA_tot, AgeGroup ~ SEX, value.var="n")
colnames(BA_tot) = c('AgeGroup','Male','Female')

BA = BA %>%
  group_by(AgeGroup, SEX, Disable) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

BA = BA %>%  filter(Disable== 1) # disable = 1 = Yes
BA<- dcast(BA, AgeGroup ~ SEX, value.var="freq")
colnames(BA) = c('AgeGroup','Male','Female')

BA_se = matrix(ncol=2,nrow=6)
BA_se[,1] = sqrt((BA$Female*(1-BA$Female))/BA_tot$Female )
BA_se[,2] = sqrt((BA$Male*(1-BA$Male))/BA_tot$Male )


BA_CI.l = matrix(ncol=2,nrow=6)
BA_CI.l[,1] = BA$Female-1.96* BA_se[,1]
BA_CI.l[,2] = BA$Male-1.96* BA_se[,2]

BA_CI.u = matrix(ncol=2,nrow=6)
BA_CI.u[,1] = BA$Female+1.96* BA_se[,1]
BA_CI.u[,2] = BA$Male+1.96* BA_se[,2]

colnames(BA_se)=colnames(BA_CI.l)=colnames(BA_CI.u)=c("Female","Male")



# data for countries

# Disability prevalences by sex

AR #Argentina 
BR #Brazil
CH #Chile 
MX #Mexico 
UR #Uruguay 
CB #Cuba 
BA #BARBADOS 


# allocating all countries in one dataset

#women
wx.f = cbind(AR$Female,BR$Female,CH$Female,MX$Female,UR$Female,CB$Female,BA$Female)
colnames(wx.f)=c('AR','BR','CH','MX','UR','CB','BA')
rownames(wx.f)=AR$AgeGroup
wx.f

wx.f_tot = cbind(c('Argentina','Brazil','Chile','Mexico','Uruguay','Cuba','Barbados'),
                 c(rep("Female",7)),
                 c(AR_allages[1,3]/(AR_allages[1,3]+AR_allages[2,3]),
                 BR_allages[1,3]/(BR_allages[1,3]+BR_allages[2,3]),
                 CH_allages[1,3]/(CH_allages[1,3]+CH_allages[2,3]),
                 MX_allages[1,3]/(MX_allages[1,3]+MX_allages[2,3]),
                 UR_allages[1,3]/(UR_allages[1,3]+UR_allages[2,3]),
                 CB_allages[1,3]/(CB_allages[1,3]+CB_allages[2,3]),
                 BA_allages[1,3]/(BA_allages[1,3]+BA_allages[2,3])))

colnames(wx.f_tot)=c('country','sex','prev')
wx.f_tot

#men
wx.m = cbind(AR$Male,BR$Male, CH$Male,MX$Male,UR$Male,CB$Male,BA$Male)
colnames(wx.m)=c('AR','BR','CH','MX','UR','CB','BA')
rownames(wx.m)=AR$AgeGroup
wx.m

wx.m_tot = cbind(c('Argentina','Brazil','Chile','Mexico','Uruguay','Cuba','Barbados'),
                 c(rep("Male",7)),
                 c(AR_allages[3,3]/(AR_allages[3,3]+AR_allages[4,3]),
                 BR_allages[3,3]/(BR_allages[3,3]+BR_allages[4,3]),
                 CH_allages[3,3]/(CH_allages[3,3]+CH_allages[4,3]),
                 MX_allages[3,3]/(MX_allages[3,3]+MX_allages[4,3]),
                 UR_allages[3,3]/(UR_allages[3,3]+UR_allages[4,3]),
                 CB_allages[3,3]/(CB_allages[3,3]+CB_allages[4,3]),
                 BA_allages[3,3]/(BA_allages[3,3]+BA_allages[4,3])))

a= wx.m_tot[[21]]
b=wx.f_tot[[21]]

wx.m_tot[[21]] = b
wx.f_tot[[21]] = a

wx.tot = rbind(wx.f_tot,wx.m_tot)
wx.tot



#------------------------------
# organizing dataset for the CI
#------------------------------
# AR
AR_set.f = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.f$AR,
                      dx=dx.f$AR,
                      ax=ax.f$AR,
                      Rx=AR_tot$Female,
                      wx =wx.f[,1])

AR_set.m = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.m$AR,
                      dx=dx.m$AR,
                      ax=ax.m$AR,
                      Rx=AR_tot$Male,
                      wx =wx.m[,1])

# BR
BR_set.f = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.f$BR,
                      dx=dx.f$BR,
                      ax=ax.f$BR,
                      Rx=BR_tot$Female,
                      wx =wx.f[,2])

BR_set.m = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.m$BR,
                      dx=dx.m$BR,
                      ax=ax.m$BR,
                      Rx=BR_tot$Male,
                      wx =wx.m[,2])
# CH
CH_set.f = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.f$CH,
                      dx=dx.f$CH,
                      ax=ax.f$CH,
                      Rx=CH_tot$Female,
                      wx =wx.f[,3])

CH_set.m = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.m$CH,
                      dx=dx.m$CH,
                      ax=ax.m$CH,
                      Rx=CH_tot$Male,
                      wx =wx.m[,3])

# MX
MX_set.f = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.f$MX,
                      dx=dx.f$MX,
                      ax=ax.f$MX,
                      Rx=MX_tot$Female,
                      wx =wx.f[,4])

MX_set.m = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.m$MX,
                      dx=dx.m$MX,
                      ax=ax.m$MX,
                      Rx=MX_tot$Male,
                      wx=wx.m[,4])
# UR
UR_set.f = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.f$UR,
                      dx=dx.f$UR,
                      ax=ax.f$UR,
                      Rx=UR_tot$Female,
                      wx =wx.f[,5])

UR_set.m = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.m$UR,
                      dx=dx.m$UR,
                      ax=ax.m$UR,
                      Rx=UR_tot$Male,
                      wx=wx.m[,5])
# CB
CB_set.f = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.f$CB,
                      dx=dx.f$CB,
                      ax=ax.f$CB,
                      Rx=CB_tot$Female,
                      wx =wx.f[,6])

CB_set.m = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.m$CB,
                      dx=dx.m$CB,
                      ax=ax.m$CB,
                      Rx=CB_tot$Male,
                      wx=wx.m[,6])

# BA
BA_set.f = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.f$BA,
                      dx=dx.f$BA,
                      ax=ax.f$BA,
                      Rx=BA_tot$Female,
                      wx =wx.f[,7])

BA_set.m = data.frame(age=seq(60,85,by=5),
                      Lx=Lx.m$BA,
                      dx=dx.m$BA,
                      ax=ax.m$BA,
                      Rx=BA_tot$Male,
                      wx=wx.m[,7])


#-----------------------------------------------------------------------------------------------
#Preparing the inputs
#The rates argument of the Sullivan.fun function must contain the same values attributed to the pars1 and pars2 arguments of the horiuchi and stepwise_replacement functions. For this example, the set of age-specific mortality rates used as input for rates, pars1 and pars2 arguments come from the Human Mortality Database (HMD, 2018). There is an easy way to read data from the HMD website by using the package HMDHFDplus (Riffe, 2015).

# Combinin age-specific death rates and age-specific prevalence of disability into one vector, as required for the decomposition functions horiuchi and stepwise_replacement.

# AR
mxwx.m.AR <- c(mx.m[,2],wx.m[,1])
mxwx.f.AR <- c(mx.f[,2],wx.f[,1])

mxwx.m.AR.CI.l <- c(mx.m[,2],AR_CI.l[,2])
mxwx.f.AR.CI.l  <- c(mx.f[,2],AR_CI.l[,1])

mxwx.m.AR.CI.u <- c(mx.m[,2],AR_CI.u[,2])
mxwx.f.AR.CI.u  <- c(mx.f[,2],AR_CI.u[,1])

# BR
mxwx.m.BR <- c(mx.m[,3],wx.m[,2])
mxwx.f.BR <- c(mx.f[,3],wx.f[,2])

mxwx.m.BR.CI.l <- c(mx.m[,3],BR_CI.l[,2])
mxwx.f.BR.CI.l  <- c(mx.f[,3],BR_CI.l[,1])

mxwx.m.BR.CI.u <- c(mx.m[,3],BR_CI.u[,2])
mxwx.f.BR.CI.u  <- c(mx.f[,3],BR_CI.u[,1])

# CH
mxwx.m.CH <- c(mx.m[,4],wx.m[,3])
mxwx.f.CH <- c(mx.f[,4],wx.f[,3])

mxwx.m.CH.CI.l <- c(mx.m[,4],CH_CI.l[,2])
mxwx.f.CH.CI.l  <- c(mx.f[,4],CH_CI.l[,1])

mxwx.m.CH.CI.u <- c(mx.m[,4],CH_CI.u[,2])
mxwx.f.CH.CI.u  <- c(mx.f[,4],CH_CI.u[,1])

# MX
mxwx.m.MX <- c(mx.m[,5],wx.m[,4])
mxwx.f.MX <- c(mx.f[,5],wx.f[,4])

mxwx.m.MX.CI.l <- c(mx.m[,5],MX_CI.l[,2])
mxwx.f.MX.CI.l  <- c(mx.f[,5],MX_CI.l[,1])

mxwx.m.MX.CI.u <- c(mx.m[,5],MX_CI.u[,2])
mxwx.f.MX.CI.u  <- c(mx.f[,5],MX_CI.u[,1])

# UR
mxwx.m.UR <- c(mx.m[,6],wx.m[,5])
mxwx.f.UR <- c(mx.f[,6],wx.f[,5])

mxwx.m.UR.CI.l <- c(mx.m[,6],UR_CI.l[,2])
mxwx.f.UR.CI.l  <- c(mx.f[,6],UR_CI.l[,1])

mxwx.m.UR.CI.u <- c(mx.m[,6],UR_CI.u[,2])
mxwx.f.UR.CI.u  <- c(mx.f[,6],UR_CI.u[,1])

# CB
mxwx.m.CB <- c(mx.m[,7],wx.m[,6])
mxwx.f.CB <- c(mx.f[,7],wx.f[,6])

mxwx.m.CB.CI.l <- c(mx.m[,7],CB_CI.l[,2])
mxwx.f.CB.CI.l  <- c(mx.f[,7],CB_CI.l[,1])

mxwx.m.CB.CI.u <- c(mx.m[,7],CB_CI.u[,2])
mxwx.f.CB.CI.u  <- c(mx.f[,7],CB_CI.u[,1])

# BA
mxwx.m.BA <- c(mx.m[,8],wx.m[,7])
mxwx.f.BA <- c(mx.f[,8],wx.f[,7])

mxwx.m.BA.CI.l <- c(mx.m[,8],BA_CI.l[,2])
mxwx.f.BA.CI.l  <- c(mx.f[,8],BA_CI.l[,1])

mxwx.m.BA.CI.u <- c(mx.m[,8],BA_CI.u[,2])
mxwx.f.BA.CI.u  <- c(mx.f[,8],BA_CI.u[,1])

#Calculating the gap in healthy life expectancy

#-------------------------------------------
# AR
# The DFLE male was
# Male
HL.m.AR = Sullivan.fun(rates=mxwx.m.AR)
HL.m.AR



# Female
HL.f.AR = Sullivan.fun(rates=mxwx.f.AR)
HL.f.AR




# The gender gap in DFLE was:
gap.AR = HL.f.AR - HL.m.AR
gap.AR



HE_Decomp_Cont.AR <- horiuchi(func=Sullivan.fun,
                           pars1 = mxwx.m.AR, 
                           pars2 = mxwx.f.AR,
                           N=20)
sum(HE_Decomp_Cont.AR [1:6])
sum(HE_Decomp_Cont.AR [7:12])

HE_Decomp_Cont.AR.CI.l <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.AR.CI.l, 
                              pars2 = mxwx.f.AR.CI.l,
                              N=20)
sum(HE_Decomp_Cont.AR.CI.l [1:6])
sum(HE_Decomp_Cont.AR.CI.l [7:12])

HE_Decomp_Cont.AR.CI.u <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.AR.CI.u, 
                                   pars2 = mxwx.f.AR.CI.u,
                                   N=20)
sum(HE_Decomp_Cont.AR.CI.u [1:6])
sum(HE_Decomp_Cont.AR.CI.u [7:12])

#--------------------------------------------------------
# CI - proportion se

# Male
HL.m.AR.CI.low = Sullivan.fun(rates=mxwx.m.AR.CI.l)
HL.m.AR.CI.low 

HL.m.AR.CI.up= Sullivan.fun(rates=mxwx.m.AR.CI.u)
HL.m.AR.CI.up 

# Female
HL.f.AR.CI.low = Sullivan.fun(rates=mxwx.f.AR.CI.l)
HL.f.AR.CI.low 

HL.f.AR.CI.up= Sullivan.fun(rates=mxwx.f.AR.CI.u)
HL.f.AR.CI.up 

#Gender gap
gap.AR.low = HL.f.AR.CI.low  - HL.m.AR.CI.low 
gap.AR.low

gap.AR.up = HL.f.AR.CI.up  - HL.m.AR.CI.up 
gap.AR.up

#----------------------------------------------------------
# CI
which.x=60; ns=1000; level=0.95
#men
AR.CI.m = CIex(x=AR_set.m$age, 
     Nx=AR_set.m$Lx, 
     Dx=AR_set.m$dx, 
     Rx=AR_set.m$Rx, 
     which.x=which.x, ns=ns, level=level,
     mx=mxwx.m.AR[1:6],
     wx=mxwx.m.AR[7:12])
HL.m.AR.CI  = AR.CI.m$ex.health 
HL.m.AR.CI - HL.m.AR

HL.m.AR.CI.l = AR.CI.m$CIex.health[1]
HL.m.AR.CI.u = AR.CI.m$CIex.health[2]

#women
AR.CI.f  = CIex(x=AR_set.f$age, 
                  Nx=AR_set.f$Lx, 
                  Dx=AR_set.f$dx, 
                  Rx=AR_set.f$Rx, 
                  which.x=which.x, ns=ns, level=level,
                  mx=mxwx.f.AR[1:6],
                  wx=mxwx.f.AR[7:12])
HL.f.AR.CI  = AR.CI.f$ex.health 
HL.f.AR.CI - HL.f.AR

HL.f.AR.CI.l = AR.CI.f$CIex.health[1]
HL.f.AR.CI.u = AR.CI.f$CIex.health[2]

# gaps in total LE
gap.AR.l.tot  = AR.CI.f$CIex[1] - AR.CI.m$CIex[1]
gap.AR.u.tot  = AR.CI.f$CIex[2] - AR.CI.m$CIex[2]
gap.AR.mean.tot  = AR.CI.f$meanex - AR.CI.m$meanex

gap.AR.l.tot
gap.AR.u.tot
gap.AR.mean.tot

#health gaps
gap.AR.l  = HL.f.AR.CI.l-HL.m.AR.CI.l
gap.AR.u  = HL.f.AR.CI.u-HL.m.AR.CI.u
gap.AR.mean  = AR.CI.f$meanex.health - AR.CI.m$meanex.health

gap.AR
gap.AR.mean
gap.AR.l
gap.AR.u

AR_HE_gap=AR.CI.f$exsim.health- AR.CI.m$exsim.health
summary(AR_HE_gap)

AR_LE_gap=AR.CI.f$exsim - AR.CI.m$exsim
summary(AR_LE_gap)

CI_AR_HE_gap=quantile(AR_HE_gap,
         probs = c((1-level)/2,
                   1 - (1-level)/2))

CI_AR_LE_gap=quantile(AR_LE_gap,
                     probs = c((1-level)/2,
                               1 - (1-level)/2))

CI_AR_HE_gap
CI_AR_LE_gap


#-------------------------------------------
# BR
# The DFLE male was
# Male
HL.m.BR = Sullivan.fun(rates=mxwx.m.BR)
HL.m.BR
# Female
HL.f.BR = Sullivan.fun(rates=mxwx.f.BR)
HL.f.BR
# The gender gap in DFLE was:
gap.BR = HL.f.BR-HL.m.BR
gap.BR

HE_Decomp_Cont.BR <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.BR, 
                              pars2 = mxwx.f.BR,
                              N=20)
sum(HE_Decomp_Cont.BR [1:6])
sum(HE_Decomp_Cont.BR [7:12])

HE_Decomp_Cont.BR.CI.l <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.BR.CI.l, 
                                   pars2 = mxwx.f.BR.CI.l,
                                   N=20)
sum(HE_Decomp_Cont.BR.CI.l [1:6])
sum(HE_Decomp_Cont.BR.CI.l [7:12])

HE_Decomp_Cont.BR.CI.u <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.BR.CI.u, 
                                   pars2 = mxwx.f.BR.CI.u,
                                   N=20)
sum(HE_Decomp_Cont.BR.CI.u [1:6])
sum(HE_Decomp_Cont.BR.CI.u [7:12])

#--------------------------------------------------------
# CI - proportion se

# Male
HL.m.BR.CI.low = Sullivan.fun(rates=mxwx.m.BR.CI.l)
HL.m.BR.CI.low 

HL.m.BR.CI.up= Sullivan.fun(rates=mxwx.m.BR.CI.u)
HL.m.BR.CI.up 

# Female
HL.f.BR.CI.low = Sullivan.fun(rates=mxwx.f.BR.CI.l)
HL.f.BR.CI.low 

HL.f.BR.CI.up= Sullivan.fun(rates=mxwx.f.BR.CI.u)
HL.f.BR.CI.up 

#Gender gap
gap.BR.low = HL.f.BR.CI.low  - HL.m.BR.CI.low 
gap.BR.low

gap.BR.up = HL.f.BR.CI.up  - HL.m.BR.CI.up 
gap.BR.up

#----------------------------------------------------------



#---------------------------
# CI

#men
BR.CI.m = CIex(x=BR_set.m$age, 
               Nx=BR_set.m$Lx, 
               Dx=BR_set.m$dx, 
               Rx=BR_set.m$Rx, 
               which.x=which.x, ns=ns, level=level,
               mx=mxwx.m.BR[1:6],
               wx=mxwx.m.BR[7:12])
HL.m.BR.CI  = BR.CI.m$ex.health 
HL.m.BR.CI - HL.m.BR

HL.m.BR.CI.l = BR.CI.m$CIex.health[1]
HL.m.BR.CI.u = BR.CI.m$CIex.health[2]

#women
BR.CI.f  = CIex(x=BR_set.f$age, 
                Nx=BR_set.f$Lx, 
                Dx=BR_set.f$dx, 
                Rx=BR_set.f$Rx, 
                which.x=which.x, ns=ns, level=level,
                mx=mxwx.f.BR[1:6],
                wx=mxwx.f.BR[7:12])
HL.f.BR.CI  = BR.CI.f$ex.health 
HL.f.BR.CI - HL.f.BR

HL.f.BR.CI.l = BR.CI.f$CIex.health[1]
HL.f.BR.CI.u = BR.CI.f$CIex.health[2]

# gaps in total LE
gap.BR.l.tot  = BR.CI.f$CIex[1] - BR.CI.m$CIex[1]
gap.BR.u.tot  = BR.CI.f$CIex[2] - BR.CI.m$CIex[2]
gap.BR.mean.tot  = BR.CI.f$meanex - BR.CI.m$meanex

gap.BR.l.tot
gap.BR.u.tot
gap.BR.mean.tot

#health gaps
gap.BR.l  = HL.f.BR.CI.l-HL.m.BR.CI.l
gap.BR.u  = HL.f.BR.CI.u-HL.m.BR.CI.u
gap.BR.mean  = BR.CI.f$meanex.health - BR.CI.m$meanex.health

gap.BR
gap.BR.mean
gap.BR.l
gap.BR.u

BR_HE_gap=BR.CI.f$exsim.health- BR.CI.m$exsim.health
summary(BR_HE_gap)

BR_LE_gap=BR.CI.f$exsim - BR.CI.m$exsim
summary(BR_LE_gap)

CI_BR_HE_gap=quantile(BR_HE_gap,
                      probs = c((1-level)/2,
                                1 - (1-level)/2))

CI_BR_LE_gap=quantile(BR_LE_gap,
                      probs = c((1-level)/2,
                                1 - (1-level)/2))

CI_BR_HE_gap
CI_BR_LE_gap

#-------------------------------------------
# CH
# The DFLE male was
# Male
HL.m.CH = Sullivan.fun(rates=mxwx.m.CH)
HL.m.CH
# Female
HL.f.CH = Sullivan.fun(rates=mxwx.f.CH)
HL.f.CH
# The gender gap in DFLE was:
gap.CH = HL.f.CH - HL.m.CH
gap.CH

HE_Decomp_Cont.CH <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.CH, 
                              pars2 = mxwx.f.CH,
                              N=20)

sum(HE_Decomp_Cont.CH [1:6])
sum(HE_Decomp_Cont.CH [7:12])

HE_Decomp_Cont.CH.CI.l <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.CH.CI.l, 
                                   pars2 = mxwx.f.CH.CI.l,
                                   N=20)
sum(HE_Decomp_Cont.CH.CI.l [1:6])
sum(HE_Decomp_Cont.CH.CI.l [7:12])

HE_Decomp_Cont.CH.CI.u <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.CH.CI.u, 
                                   pars2 = mxwx.f.CH.CI.u,
                                   N=20)
sum(HE_Decomp_Cont.CH.CI.u [1:6])
sum(HE_Decomp_Cont.CH.CI.u [7:12])
#--------------------------------------------------------
# CI - proportion se

# Male
HL.m.CH.CI.low = Sullivan.fun(rates=mxwx.m.CH.CI.l)
HL.m.CH.CI.low 

HL.m.CH.CI.up= Sullivan.fun(rates=mxwx.m.CH.CI.u)
HL.m.CH.CI.up 

# Female
HL.f.CH.CI.low = Sullivan.fun(rates=mxwx.f.CH.CI.l)
HL.f.CH.CI.low 

HL.f.CH.CI.up= Sullivan.fun(rates=mxwx.f.CH.CI.u)
HL.f.CH.CI.up 

#Gender gap
gap.CH.low = HL.f.CH.CI.low  - HL.m.CH.CI.low 
gap.CH.low

gap.CH.up = HL.f.CH.CI.up  - HL.m.CH.CI.up 
gap.CH.up

#----------------------------------------------------------

#---------------------------
# CI

#men
CH.CI.m = CIex(x=CH_set.m$age, 
               Nx=CH_set.m$Lx, 
               Dx=CH_set.m$dx, 
               Rx=CH_set.m$Rx, 
               which.x=which.x, ns=ns, level=level,
               mx=mxwx.m.CH[1:6],
               wx=mxwx.m.CH[7:12])
HL.m.CH.CI  = CH.CI.m$ex.health 
HL.m.CH.CI - HL.m.CH

HL.m.CH.CI.l = CH.CI.m$CIex.health[1]
HL.m.CH.CI.u = CH.CI.m$CIex.health[2]

#women
CH.CI.f  = CIex(x=CH_set.f$age, 
                Nx=CH_set.f$Lx, 
                Dx=CH_set.f$dx, 
                Rx=CH_set.f$Rx, 
                which.x=which.x, ns=ns, level=level,
                mx=mxwx.f.CH[1:6],
                wx=mxwx.f.CH[7:12])
HL.f.CH.CI  = CH.CI.f$ex.health 
HL.f.CH.CI - HL.f.CH

HL.f.CH.CI.l = CH.CI.f$CIex.health[1]
HL.f.CH.CI.u = CH.CI.f$CIex.health[2]

# gaps in total LE
gap.CH.l.tot  = CH.CI.f$CIex[1] - CH.CI.m$CIex[1]
gap.CH.u.tot  = CH.CI.f$CIex[2] - CH.CI.m$CIex[2]
gap.CH.mean.tot  = CH.CI.f$meanex - CH.CI.m$meanex

gap.CH.l.tot
gap.CH.u.tot
gap.CH.mean.tot

#health gaps
gap.CH.l  = HL.f.CH.CI.l-HL.m.CH.CI.l
gap.CH.u  = HL.f.CH.CI.u-HL.m.CH.CI.u
gap.CH.mean  = CH.CI.f$meanex.health - CH.CI.m$meanex.health

gap.CH
gap.CH.mean
gap.CH.l
gap.CH.u

CH_HE_gap=CH.CI.f$exsim.health- CH.CI.m$exsim.health
summary(CH_HE_gap)

CH_LE_gap=CH.CI.f$exsim - CH.CI.m$exsim
summary(CH_LE_gap)

CI_CH_HE_gap=quantile(CH_HE_gap,
                      probs = c((1-level)/2,
                                1 - (1-level)/2))

CI_CH_LE_gap=quantile(CH_LE_gap,
                      probs = c((1-level)/2,
                                1 - (1-level)/2))

CI_CH_HE_gap
CI_CH_LE_gap

#-------------------------------------------
# MX
# The DFLE male was
# Male
HL.m.MX = Sullivan.fun(rates=mxwx.m.MX)
HL.m.MX
# Female
HL.f.MX = Sullivan.fun(rates=mxwx.f.MX)
HL.f.MX
# The gender gap in DFLE was:
gap.MX = HL.f.MX-HL.m.MX
gap.MX

HE_Decomp_Cont.MX <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.MX, 
                              pars2 = mxwx.f.MX,
                              N=20)

sum(HE_Decomp_Cont.MX [1:6])
sum(HE_Decomp_Cont.MX [7:12])


HE_Decomp_Cont.MX.CI.l <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.MX.CI.l, 
                                   pars2 = mxwx.f.MX.CI.l,
                                   N=20)
sum(HE_Decomp_Cont.MX.CI.l [1:6])
sum(HE_Decomp_Cont.MX.CI.l [7:12])

HE_Decomp_Cont.MX.CI.u <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.MX.CI.u, 
                                   pars2 = mxwx.f.MX.CI.u,
                                   N=20)
sum(HE_Decomp_Cont.MX.CI.u [1:6])
sum(HE_Decomp_Cont.MX.CI.u [7:12])

#--------------------------------------------------------
# CI - proportion se

# Male
HL.m.MX.CI.low = Sullivan.fun(rates=mxwx.m.MX.CI.l)
HL.m.MX.CI.low 

HL.m.MX.CI.up= Sullivan.fun(rates=mxwx.m.MX.CI.u)
HL.m.MX.CI.up 

# Female
HL.f.MX.CI.low = Sullivan.fun(rates=mxwx.f.MX.CI.l)
HL.f.MX.CI.low 

HL.f.MX.CI.up= Sullivan.fun(rates=mxwx.f.MX.CI.u)
HL.f.MX.CI.up 

#Gender gap
gap.MX.low = HL.f.MX.CI.low  - HL.m.MX.CI.low 
gap.MX.low

gap.MX.up = HL.f.MX.CI.up  - HL.m.MX.CI.up 
gap.MX.up

#----------------------------------------------------------

#---------------------------
# CI

#men
MX.CI.m = CIex(x=MX_set.m$age, 
               Nx=MX_set.m$Lx, 
               Dx=MX_set.m$dx, 
               Rx=MX_set.m$Rx, 
               which.x=which.x, ns=ns, level=level,
               mx=mxwx.m.MX[1:6],
               wx=mxwx.m.MX[7:12])
HL.m.MX.CI  = MX.CI.m$ex.health 
HL.m.MX.CI - HL.m.MX

HL.m.MX.CI.l = MX.CI.m$CIex.health[1]
HL.m.MX.CI.u = MX.CI.m$CIex.health[2]

#women
MX.CI.f  = CIex(x=MX_set.f$age, 
                Nx=MX_set.f$Lx, 
                Dx=MX_set.f$dx, 
                Rx=MX_set.f$Rx, 
                which.x=which.x, ns=ns, level=level,
                mx=mxwx.f.MX[1:6],
                wx=mxwx.f.MX[7:12])
HL.f.MX.CI  = MX.CI.f$ex.health 
HL.f.MX.CI - HL.f.MX

HL.f.MX.CI.l = MX.CI.f$CIex.health[1]
HL.f.MX.CI.u = MX.CI.f$CIex.health[2]

# gaps in total LE
gap.MX.l.tot  = MX.CI.f$CIex[1] - MX.CI.m$CIex[1]
gap.MX.u.tot  = MX.CI.f$CIex[2] - MX.CI.m$CIex[2]
gap.MX.mean.tot  = MX.CI.f$meanex - MX.CI.m$meanex

gap.MX.l.tot
gap.MX.u.tot
gap.MX.mean.tot

#health gaps
gap.MX.l  = HL.f.MX.CI.l-HL.m.MX.CI.l
gap.MX.u  = HL.f.MX.CI.u-HL.m.MX.CI.u
gap.MX.mean  = MX.CI.f$meanex.health - MX.CI.m$meanex.health

gap.MX
gap.MX.mean
gap.MX.l
gap.MX.u

MX_HE_gap=MX.CI.f$exsim.health- MX.CI.m$exsim.health
summary(MX_HE_gap)

MX_LE_gap=MX.CI.f$exsim - MX.CI.m$exsim
summary(MX_LE_gap)

CI_MX_HE_gap=quantile(MX_HE_gap,
                      probs = c((1-level)/2,
                                1 - (1-level)/2))

CI_MX_LE_gap=quantile(MX_LE_gap,
                      probs = c((1-level)/2,
                                1 - (1-level)/2))

CI_MX_HE_gap
CI_MX_LE_gap

#-------------------------------------------
# UR
# The DFLE male was
# Male
HL.m.UR = Sullivan.fun(rates=mxwx.m.UR)
HL.m.UR
# Female
HL.f.UR = Sullivan.fun(rates=mxwx.f.UR)
HL.f.UR
# The gender gap in DFLE was:
gap.UR= HL.f.UR-HL.m.UR
gap.UR

HE_Decomp_Cont.UR <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.UR, 
                              pars2 = mxwx.f.UR,
                              N=20)
sum(HE_Decomp_Cont.UR [1:6])
sum(HE_Decomp_Cont.UR [7:12])

HE_Decomp_Cont.UR.CI.l <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.UR.CI.l, 
                                   pars2 = mxwx.f.UR.CI.l,
                                   N=20)
sum(HE_Decomp_Cont.UR.CI.l [1:6])
sum(HE_Decomp_Cont.UR.CI.l [7:12])

HE_Decomp_Cont.UR.CI.u <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.UR.CI.u, 
                                   pars2 = mxwx.f.UR.CI.u,
                                   N=20)
sum(HE_Decomp_Cont.UR.CI.u [1:6])
sum(HE_Decomp_Cont.UR.CI.u [7:12])



#--------------------------------------------------------
# CI - proportion se

# Male
HL.m.UR.CI.low = Sullivan.fun(rates=mxwx.m.UR.CI.l)
HL.m.UR.CI.low 

HL.m.UR.CI.up= Sullivan.fun(rates=mxwx.m.UR.CI.u)
HL.m.UR.CI.up 

# Female
HL.f.UR.CI.low = Sullivan.fun(rates=mxwx.f.UR.CI.l)
HL.f.UR.CI.low 

HL.f.UR.CI.up= Sullivan.fun(rates=mxwx.f.UR.CI.u)
HL.f.UR.CI.up 

#Gender gap
gap.UR.low = HL.f.UR.CI.low  - HL.m.UR.CI.low 
gap.UR.low

gap.UR.up = HL.f.UR.CI.up  - HL.m.UR.CI.up 
gap.UR.up

#----------------------------------------------------------


#---------------------------
# CI

#men
UR.CI.m = CIex(x=UR_set.m$age, 
               Nx=UR_set.m$Lx, 
               Dx=UR_set.m$dx, 
               Rx=UR_set.m$Rx, 
               which.x=which.x, ns=ns, level=level,
               mx=mxwx.m.UR[1:6],
               wx=mxwx.m.UR[7:12])
HL.m.UR.CI  = UR.CI.m$ex.health 
HL.m.UR.CI - HL.m.UR

HL.m.UR.CI.l = UR.CI.m$CIex.health[1]
HL.m.UR.CI.u = UR.CI.m$CIex.health[2]

#women
UR.CI.f  = CIex(x=UR_set.f$age, 
                Nx=UR_set.f$Lx, 
                Dx=UR_set.f$dx, 
                Rx=UR_set.f$Rx, 
                which.x=which.x, ns=ns, level=level,
                mx=mxwx.f.UR[1:6],
                wx=mxwx.f.UR[7:12])
HL.f.UR.CI  = UR.CI.f$ex.health 
HL.f.UR.CI - HL.f.UR

HL.f.UR.CI.l = UR.CI.f$CIex.health[1]
HL.f.UR.CI.u = UR.CI.f$CIex.health[2]

# gaps in total LE
gap.UR.l.tot  = UR.CI.f$CIex[1] - UR.CI.m$CIex[1]
gap.UR.u.tot  = UR.CI.f$CIex[2] - UR.CI.m$CIex[2]
gap.UR.mean.tot  = UR.CI.f$meanex - UR.CI.m$meanex

gap.UR.l.tot
gap.UR.u.tot
gap.UR.mean.tot

#health gaps
gap.UR.l  = HL.f.UR.CI.l-HL.m.UR.CI.l
gap.UR.u  = HL.f.UR.CI.u-HL.m.UR.CI.u
gap.UR.mean  = UR.CI.f$meanex.health-UR.CI.m$meanex.health

gap.UR
gap.UR.mean
gap.UR.l
gap.UR.u

#-------------------------------------------
# CB
# The DFLE male was
# Male
HL.m.CB = Sullivan.fun(rates=mxwx.m.CB)
HL.m.CB
# Female
HL.f.CB = Sullivan.fun(rates=mxwx.f.CB)
HL.f.CB
# The gender gap in DFLE was:
gap.CB= HL.f.CB-HL.m.CB
gap.CB

HE_Decomp_Cont.CB <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.CB, 
                              pars2 = mxwx.f.CB,
                              N=20)
sum(HE_Decomp_Cont.CB [1:6])
sum(HE_Decomp_Cont.CB [7:12])

HE_Decomp_Cont.CB.CI.l <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.CB.CI.l, 
                                   pars2 = mxwx.f.CB.CI.l,
                                   N=20)
sum(HE_Decomp_Cont.CB.CI.l [1:6])
sum(HE_Decomp_Cont.CB.CI.l [7:12])

HE_Decomp_Cont.CB.CI.u <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.CB.CI.u, 
                                   pars2 = mxwx.f.CB.CI.u,
                                   N=20)
sum(HE_Decomp_Cont.CB.CI.u [1:6])
sum(HE_Decomp_Cont.CB.CI.u [7:12])

#--------------------------------------------------------
# CI - proportion se

# Male
HL.m.CB.CI.low = Sullivan.fun(rates=mxwx.m.CB.CI.l)
HL.m.CB.CI.low 

HL.m.CB.CI.up= Sullivan.fun(rates=mxwx.m.CB.CI.u)
HL.m.CB.CI.up 

# Female
HL.f.CB.CI.low = Sullivan.fun(rates=mxwx.f.CB.CI.l)
HL.f.CB.CI.low 

HL.f.CB.CI.up= Sullivan.fun(rates=mxwx.f.CB.CI.u)
HL.f.CB.CI.up 

#Gender gap
gap.CB.low = HL.f.CB.CI.low  - HL.m.CB.CI.low 
gap.CB.low

gap.CB.up = HL.f.CB.CI.up  - HL.m.CB.CI.up 
gap.CB.up

#----------------------------------------------------------


#---------------------------
# CI

#men
CB.CI.m = CIex(x=CB_set.m$age, 
               Nx=CB_set.m$Lx, 
               Dx=CB_set.m$dx, 
               Rx=CB_set.m$Rx, 
               which.x=which.x, ns=ns, level=level,
               mx=mxwx.m.CB[1:6],
               wx=mxwx.m.CB[7:12])
HL.m.CB.CI  = CB.CI.m$ex.health 
HL.m.CB.CI - HL.m.CB

HL.m.CB.CI.l = CB.CI.m$CIex.health[1]
HL.m.CB.CI.u = CB.CI.m$CIex.health[2]

#women
CB.CI.f  = CIex(x=CB_set.f$age, 
                Nx=CB_set.f$Lx, 
                Dx=CB_set.f$dx, 
                Rx=CB_set.f$Rx, 
                which.x=which.x, ns=ns, level=level,
                mx=mxwx.f.CB[1:6],
                wx=mxwx.f.CB[7:12])
HL.f.CB.CI  = CB.CI.f$ex.health 
HL.f.CB.CI - HL.f.CB

HL.f.CB.CI.l = CB.CI.f$CIex.health[1]
HL.f.CB.CI.u = CB.CI.f$CIex.health[2]

# gaps in total LE
gap.CB.l.tot  = CB.CI.f$CIex[1] - CB.CI.m$CIex[1]
gap.CB.u.tot  = CB.CI.f$CIex[2] - CB.CI.m$CIex[2]
gap.CB.mean.tot  = CB.CI.f$meanex - CB.CI.m$meanex

gap.CB.l.tot
gap.CB.u.tot
gap.CB.mean.tot

#health gaps
gap.CB.l  = HL.f.CB.CI.l-HL.m.CB.CI.l
gap.CB.u  = HL.f.CB.CI.u-HL.m.CB.CI.u
gap.CB.mean  = CB.CI.f$meanex.health-CB.CI.m$meanex.health

gap.CB
gap.CB.mean
gap.CB.l
gap.CB.u

#-------------------------------------------
# BA
# The DFLE male was
# Male
HL.m.BA = Sullivan.fun(rates=mxwx.m.BA)
HL.m.BA
# Female
HL.f.BA = Sullivan.fun(rates=mxwx.f.BA)
HL.f.BA
# The gender gap in DFLE was:
gap.BA= HL.f.BA-HL.m.BA
gap.BA

HE_Decomp_Cont.BA <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.BA, 
                              pars2 = mxwx.f.BA,
                              N=20)
sum(HE_Decomp_Cont.BA [1:6])
sum(HE_Decomp_Cont.BA [7:12])


HE_Decomp_Cont.BA.CI.l <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.BA.CI.l, 
                                   pars2 = mxwx.f.BA.CI.l,
                                   N=20)
sum(HE_Decomp_Cont.BA.CI.l [1:6])
sum(HE_Decomp_Cont.BA.CI.l [7:12])

HE_Decomp_Cont.BA.CI.u <- horiuchi(func=Sullivan.fun,
                                   pars1 = mxwx.m.BA.CI.u, 
                                   pars2 = mxwx.f.BA.CI.u,
                                   N=20)
sum(HE_Decomp_Cont.BA.CI.u [1:6])
sum(HE_Decomp_Cont.BA.CI.u [7:12])


#--------------------------------------------------------
# CI - proportion se

# Male
HL.m.BA.CI.low = Sullivan.fun(rates=mxwx.m.BA.CI.l)
HL.m.BA.CI.low 

HL.m.BA.CI.up= Sullivan.fun(rates=mxwx.m.BA.CI.u)
HL.m.BA.CI.up 

# Female
HL.f.BA.CI.low = Sullivan.fun(rates=mxwx.f.BA.CI.l)
HL.f.BA.CI.low 

HL.f.BA.CI.up= Sullivan.fun(rates=mxwx.f.BA.CI.u)
HL.f.BA.CI.up 

#Gender gap
gap.BA.low = HL.f.BA.CI.low  - HL.m.BA.CI.low 
gap.BA.low

gap.BA.up = HL.f.BA.CI.up  - HL.m.BA.CI.up 
gap.BA.up



#---------------------------
# CI

#men
BA.CI.m = CIex(x=BA_set.m$age, 
               Nx=BA_set.m$Lx, 
               Dx=BA_set.m$dx, 
               Rx=BA_set.m$Rx, 
               which.x=which.x, ns=ns, level=level,
               mx=mxwx.m.BA[1:6],
               wx=mxwx.m.BA[7:12])
HL.m.BA.CI  = BA.CI.m$ex.health 
HL.m.BA.CI - HL.m.BA

HL.m.BA.CI.l = BA.CI.m$CIex.health[1]
HL.m.BA.CI.u = BA.CI.m$CIex.health[2]

#women
BA.CI.f  = CIex(x=BA_set.f$age, 
                Nx=BA_set.f$Lx, 
                Dx=BA_set.f$dx, 
                Rx=BA_set.f$Rx, 
                which.x=which.x, ns=ns, level=level,
                mx=mxwx.f.BA[1:6],
                wx=mxwx.f.BA[7:12])
HL.f.BA.CI  = BA.CI.f$ex.health 
HL.f.BA.CI - HL.f.BA

HL.f.BA.CI.l = BA.CI.f$CIex.health[1]
HL.f.BA.CI.u = BA.CI.f$CIex.health[2]

# gaps in total LE
gap.BA.l.tot  = BA.CI.f$CIex[1] - BA.CI.m$CIex[1]
gap.BA.u.tot  = BA.CI.f$CIex[2] - BA.CI.m$CIex[2]
gap.BA.mean.tot  = BA.CI.f$meanex - BA.CI.m$meanex

gap.BA.l.tot
gap.BA.u.tot
gap.BA.mean.tot

#health gaps
gap.BA.l  = HL.f.BA.CI.l-HL.m.BA.CI.l
gap.BA.u  = HL.f.BA.CI.u-HL.m.BA.CI.u
gap.BA.mean  = BA.CI.f$meanex.health - BA.CI.m$meanex.health

gap.BA
gap.BA.mean
gap.BA.l
gap.BA.u


#---------------------------------------------------------------------------
# CI for prev disability
# se = sqrt ((p(1-p))/n)

rownames(wx.tot) <- NULL
wx.tot = as.data.frame(wx.tot)
wx.tot$prev = as.numeric(wx.tot$prev)
wx.tot$country = as.factor(unlist(wx.tot$country))
wx.tot$sex = as.factor(unlist(wx.tot$sex))

wx.tot$n =c(colSums(AR_tot[,2:3])[[1]], 
            colSums(BR_tot[,2:3])[[1]], 
            colSums(CH_tot[,2:3])[[1]], 
            colSums(MX_tot[,2:3])[[1]], 
            colSums(UR_tot[,2:3])[[1]], 
            colSums(CB_tot[,2:3])[[1]], 
            colSums(BA_tot[,2:3])[[1]],
            colSums(AR_tot[,2:3])[[2]],
            colSums(BR_tot[,2:3])[[2]], 
            colSums(CH_tot[,2:3])[[2]], 
            colSums(MX_tot[,2:3])[[2]], 
            colSums(UR_tot[,2:3])[[2]],
            colSums(CB_tot[,2:3])[[2]],
            colSums(BA_tot[,2:3])[[2]]
            )

wx.tot$SE = sqrt((wx.tot$prev*(1-wx.tot$prev))/wx.tot$n)

wx.tot$CI_l = wx.tot$prev-1.96*wx.tot$SE
wx.tot$CI_u = wx.tot$prev+1.96*wx.tot$SE

wx.tot

#-------------------------------------------
# Plotting
Age=seq(start.age,open.age,5)

# AR
# putting into matrix format
HE_cont.AR <- matrix(HE_Decomp_Cont.AR,nrow=(length(HE_Decomp_Cont.AR)/2),
                  ncol=2,byrow=F)
colnames(HE_cont.AR) <- c("Mortality","Disability")

HE_cont_df.AR <- mutate(as.data.frame(HE_cont.AR),Age=c(seq(start.age,open.age,5)))
HE_cont_res.AR <- melt(HE_cont_df.AR,id.vars="Age")
colnames(HE_cont_res.AR) <- c("Age","type","Contribution")

plot1=ggplot(data=HE_cont_res.AR, aes(x=as.factor(Age), y=Contribution, fill=type))+
  ggtitle(bquote(~'Buenos Aires (Argentina)' ))+ xlab(" ") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.position="none")
#ggsave("plot1.pdf", width = 4, height = 4)

# BR
# putting into matrix format
HE_cont.BR <- matrix(HE_Decomp_Cont.BR,nrow=(length(HE_Decomp_Cont.BR)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.BR) <- c("Mortality","Disability")

HE_cont_df.BR <- mutate(as.data.frame(HE_cont.BR),Age=c(seq(start.age,open.age,5)))
HE_cont_res.BR <- melt(HE_cont_df.BR,id.vars="Age")
colnames(HE_cont_res.BR) <- c("Age","type","Contribution")

plot2=ggplot(data=HE_cont_res.BR, aes(x=as.factor(Age), y=Contribution, fill=type))+
  ggtitle(bquote(~'São Paulo (Brazil)' ))+ xlab("Age") + ylab("")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal()+
  theme(legend.position="none")
#ggsave("plot2.pdf", width = 4, height = 4)

# CH
# putting into matrix format
HE_cont.CH <- matrix(HE_Decomp_Cont.CH,nrow=(length(HE_Decomp_Cont.CH)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.CH) <- c("Mortality","Disability")

HE_cont_df.CH <- mutate(as.data.frame(HE_cont.CH),Age=c(seq(start.age,open.age,5)))
HE_cont_res.CH <- melt(HE_cont_df.CH,id.vars="Age")
colnames(HE_cont_res.CH) <- c("Age","type","Contribution")

plot3=ggplot(data=HE_cont_res.CH, aes(x=as.factor(Age), y=Contribution, fill=type))+
  ggtitle(bquote(~'Santiago (Chile)' ))+ xlab("Age") + ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal()+
  theme(legend.position="none")
#ggsave("plot3.pdf", width = 4, height = 4)

# MX
# putting into matrix format
HE_cont.MX <- matrix(HE_Decomp_Cont.MX,nrow=(length(HE_Decomp_Cont.MX)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.MX) <- c("Mortality","Disability")

HE_cont_df.MX <- mutate(as.data.frame(HE_cont.MX),Age=c(seq(start.age,open.age,5)))
HE_cont_res.MX <- melt(HE_cont_df.MX,id.vars="Age")
colnames(HE_cont_res.MX) <- c("Age","type","Contribution")

plot4=ggplot(data=HE_cont_res.MX, aes(x=as.factor(Age), y=Contribution, fill=type))+
  ggtitle(bquote(~'Mexico City (Mexico)' ))+ xlab(" ") + ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal()+
  theme(legend.position="none")
#ggsave("plot4.pdf", width = 4, height = 4)

# UR
# putting into matrix format
HE_cont.UR <- matrix(HE_Decomp_Cont.UR,nrow=(length(HE_Decomp_Cont.UR)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.UR) <- c("Mortality","Disability")

HE_cont_df.UR <- mutate(as.data.frame(HE_cont.UR),Age=c(seq(start.age,open.age,5)))
HE_cont_res.UR <- melt(HE_cont_df.UR,id.vars="Age")
colnames(HE_cont_res.UR) <- c("Age","type","Contribution")

plot5 = ggplot(data=HE_cont_res.UR, aes(x=as.factor(Age), y=Contribution, fill=type))+
  ggtitle(bquote(~'Montevideo (Uruguay)' ))+ xlab(" ") + ylab("")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal()+
  theme(legend.position="none")
#ggsave("plot5.pdf", width = 4, height = 4)

# CB
# putting into matrix format
HE_cont.CB <- matrix(HE_Decomp_Cont.CB,nrow=(length(HE_Decomp_Cont.CB)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.CB) <- c("Mortality","Disability")

HE_cont_df.CB <- mutate(as.data.frame(HE_cont.CB),Age=c(seq(start.age,open.age,5)))
HE_cont_res.CB <- melt(HE_cont_df.CB,id.vars="Age")
colnames(HE_cont_res.CB) <- c("Age","type","Contribution")

plot6 = ggplot(data=HE_cont_res.CB, aes(x=as.factor(Age), y=Contribution, fill=type), size=10)+
  ggtitle(bquote(~'Havana (Cuba)' ))+ xlab(" ") + ylab("Contribution (in years)")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
                              color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal()+
  theme(legend.position="none")

#ggsave("plot6.pdf", width = 4, height = 4)

# BA
# putting into matrix format
HE_cont.BA <- matrix(HE_Decomp_Cont.BA,nrow=(length(HE_Decomp_Cont.BA)/2),
                     ncol=2,byrow=F)
colnames(HE_cont.BA) <- c("Mortality","Disability")

HE_cont_df.BA <- mutate(as.data.frame(HE_cont.BA),Age=c(seq(start.age,open.age,5)))
HE_cont_res.BA <- melt(HE_cont_df.BA,id.vars="Age")
colnames(HE_cont_res.BA) <- c("Age","type","Contribution")

plot7 = ggplot(data=HE_cont_res.BA, aes(x=as.factor(Age), y=Contribution, fill=type), size=10)+
  ggtitle(bquote(~'Bridgetown (Barbados)' ))+ xlab(" ") + ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal()+
  theme(legend.position="none")

#ggsave("plot7.pdf", width = 4, height = 4)


# Legend
plot.leg=ggplot(data=HE_cont_res.AR, aes(x=as.factor(Age), y=Contribution, fill=type))+
  ggtitle(bquote(~'Argentina' ))+ xlab(" ") +ylab("Contribution (in years) ")+
  theme (plot.title = element_text(size = 14))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal() +
  theme(legend.text=element_text(size=14))+
  theme(axis.title =  element_text(size=14),title =  element_text(size=14) )

library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(plot.leg)


#ordem alfabetica
Fig3 = grid.arrange(plot7,plot1,plot6,plot4,plot5,plot3,plot2,legend,nrow = 4, ncol= 2)


#grid.arrange(plot2, plot3,plot1,plot5,plot4,plot6,plot7,legend,nrow = 4, ncol= 2)

#grid.arrange(plot2, plot3,plot1,plot5,plot4,plot6,plot7,legend,nrow = 4, ncol= 2)

setwd("C:/Users/mpidr/Nextcloud/1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Figs")

ggsave(file="Fig3.eps",width = 8, height = 10, units = "in")

#pdf 8 vs 10

#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------

# Plot for Twitter
#Brasil plot2 and Mexico plot4

#Brasil
plot.br=ggplot(data=HE_cont_res.BR, aes(x=as.factor(Age), y=Contribution, fill=type))+
  ggtitle(bquote(~'Brazil (São Paulo)' ))+ xlab("Age") + ylab("")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal()
plot.br

plot.mx=ggplot(data=HE_cont_res.MX, aes(x=as.factor(Age), y=Contribution, fill=type))+
  ggtitle(bquote(~'Mexico (Mexico City)' ))+ xlab(" ") + ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  geom_bar(stat = "identity", position = "stack")+ 
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  ylim(-1.1, 1.1)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal()
plot.mx

library(ggplot2)
library(patchwork)

 
plot.final <- plot.br + plot.mx + plot_layout(guides = "collect")
plot.final 

ggsave ("/Users/marilianepomuceno/Nextcloud/2-Publications&Submissions/ViennaYearBook/Decomposing_HLE_LAC/06-Twitter/Twitter.png", 
        plot = plot.final  , width = 10, height = 7, units = "in") 

#----------------------------------------------------------------------------------------------
# More plots

#-----------
# Plot 1
# e60 mulher vs homem
#setwd("C:\\Users\\mpidr\\Nextcloud\\")

e60= read.table("1-Projetos_Marilia/Nepomuceno&Turra/Decomposition_sex_differencesHLE_LA/Data/LT/Cities/e60_Cities.txt",header=TRUE)

Fig1=ggplot(e60, aes(Women, Men)) +
  #guides(color=guide_legend("Country"),show.legend=FALSE)+
  #theme(legend.position = "none")+
  #scale_size("gap")
  theme(axis.text=element_text(size=20))+
  scale_size_area(name="   Gender gap
in Life Expectancy",breaks=c(1,2,3,4,5),max_size = 7)+
  geom_text(data = e60, aes(label = Country,colour = Country),vjust=c(-2,2.5,-2,-2,1,-2,-2),hjust=c(rep(0.5,4),-0.3,0.5,0.5),cex=3, alpha = 1)+
  geom_point(aes(size = gap, colour = Country), alpha = 0.80) +
  scale_color_manual(values=c("#645EB2" , "#FF0072","#00641F", "red", "darkorange","brown", "#1DAD82","#147A5B"))+
  theme_minimal()  +
  geom_segment(aes(x = 17, y = 17, xend = 24, yend = 24, colour = "segment" ),linetype=2)+
  guides(colour = "none")+
  ylim(17, 24)+
  xlim(17, 24)

Fig1

setwd("C:\\Users\\mpidr\\Nextcloud\\1-Projetos_Marilia\\Nepomuceno&Turra\\Decomposition_sex_differencesHLE_LA\\Figs")

ggsave(file="Fig1.eps",width = 9, height = 8, units = "in")


# pdf 9 x 8

# grey.colors(5, start = 0, end = 0.5, gamma = 2.2)
#+
#theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

#+
#labs(title = "Life expectancy at age 60 (Women vs. Men), by country, 2000",size=0.5) 

#-----------
# Plot 2
wx.f
wx.m

dif.wx = wx.f-wx.m
plot(x=seq(60,85,by=5),y=dif.wx[,1],lty=1, type="l",col='grey40',ylim=c(-0.1,0.4))
lines(x=seq(60,85,by=5),y=dif.wx[,2],col='orange')
lines(x=seq(60,85,by=5),y=dif.wx[,3],col='green')
lines(x=seq(60,85,by=5),y=dif.wx[,4],col='blue')
lines(x=seq(60,85,by=5),y=dif.wx[,5],col='magenta')
lines(x=seq(60,85,by=5),y=dif.wx[,6],col='darkgreen')
abline(h=0, col='red',lty=2)

setwd("C:\\Users\\mpidr\\Nextcloud\\1-Projetos_Marilia\\Nepomuceno&Turra\\Decomposition_sex_differencesHLE_LA\\results\\")
write.csv(dif.wx,"dif.wx.csv")

setwd("C:\\Users\\mpidr\\Nextcloud\\")
Diff.Wx= read.table("1-Projetos_Marilia\\Nepomuceno&Turra\\Decomposition_sex_differencesHLE_LA\\Data\\SABE2\\Diff.Wx.txt",header=TRUE)

ggplot(Diff.Wx, aes(Age,DiffPrev,group=Country))+
  geom_point(aes(colour = Country),alpha = 0.70) +
  geom_line(aes(colour = Country),alpha = 0.70)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  ylab("Differences in the prevalence of disability (Women-Men)") 
  

## Plot bar plot prev. by sex

#rownames(wx.tot) <- NULL
#wx.tot = as.data.frame(wx.tot)
#wx.tot$prev = as.numeric(wx.tot$prev)
#wx.tot$country = as.factor(unlist(wx.tot$country))
#wx.tot$sex = as.factor(unlist(wx.tot$sex))

wx.tot=wx.tot %>% mutate (city = ifelse(country == "Argentina" , "Buenos Aires",
                                  ifelse(country == "Brazil" , "São Paulo",
                                  ifelse(country == "Chile" , "Santiago",
                                  ifelse(country == "Mexico" , "Mexico City",
                                  ifelse(country == "Uruguay" , "Montevideo",
                                  ifelse(country == "Cuba" , "Havana",
                                         "Bridgetown")))))))
wx.tot=wx.tot %>% mutate (sex = ifelse(sex == "Female" , "Women","Men"))

Fig2=ggplot(data=wx.tot, aes(x=city, y=prev,fill=sex), size=10)+
  geom_bar(stat = "identity", position=position_dodge())+
  geom_errorbar(aes(ymin=CI_l, ymax=CI_u),
                width=.2, position=position_dodge(.9))+
  ggtitle(bquote(~'' ))+ xlab(" ") +ylab("Prevalence")+
  theme (plot.title = element_text(size = 12))+
  scale_fill_manual(values=alpha(c("grey10", "grey60"),0.5))+
  labs(fill = " ")+
  ylim(0, .35)+
  theme_minimal()+
  theme(axis.text=element_text(size=11),axis.title=element_text(size=14),
        legend.text = element_text(size=14),plot.title = element_text(size = 20, face = "bold"))

Fig2
setwd("C:\\Users\\mpidr\\Nextcloud\\1-Projetos_Marilia\\Nepomuceno&Turra\\Decomposition_sex_differencesHLE_LA\\Figs")

ggsave(file="Fig2.eps",width = 10, height = 8, units = "in")

#pdf 10 x 8







