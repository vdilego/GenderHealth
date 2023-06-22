# -----------------------------------------------------------------------------------------------------#
# Codes for replicating the paper Going Beyong the Gender Gap
# authors: Vanessa di Lego, Marília Nepomuceno, Cássio M. Turra
# Wittgenstein Centre for Demography and Global Human Capital(IIASA/OeAW/UniWien)
# Vienna Institute of Demography at the Austrian Academy of Sciences
# Max Planck Institute for Demographic Research
# Federal University of Minas Gerais (Cedeplar)
# -----------------------------------------------------------------------------------------------------#

# Calculation and decomposition of DFLE


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
library(colorspace)
library(cowplot)
library(grid)
library(DemoDecomp) 
library(gridExtra)
library(forcats)
library(ggalt)
library(ggtext)
library(extrafont)
library(ggforce)
library(ggfortify)
library(showtext)
library(ggbump)


# Loading useful functions into environment
source(here("Rcodes","0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved
figs.folder <- here("Manuscript","Figures")
dat.folder <- here("Manuscript","Data")
figs.app.folder <- here("Appendix","Figures")

# First setting the folders

dis.folder <- here("Data","All_Prevalence") 

# reading disability data for all countries at the same time
dis<-fread(here(dis.folder,"all_prev_adl.csv")) %>%  
  rename(sex=gender) %>% 
  filter(age>=50 )  %>% 
  mutate(type="ADL")%>% 
  select(-2)

cron<-fread(here(dis.folder,"all_prev_cron.csv")) %>%  
  rename(sex=gender) %>% 
  filter(age>=50 ) %>% 
  mutate(type="Chronic") %>% 
  select(-2)

dis_cron<-full_join(dis,cron)

# Heatmap for age-specific unhealthy prevalences
fig_all_conditions<-ggplot(dis_cron,
                           aes(age, country, group=country, fill=unhealthy))+
  geom_raster(interpolate = F) +
  theme_clean(base_size = 26)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90))+
  facet_grid(sex~type)+
  scale_fill_distiller(palette = "Spectral", name="%Unhealthy", n.breaks=4)

fig_all_conditions

pdf(here( "Countries","EUROPE","Descriptive","fig_prev_cron_country.pdf"), width = 18, height=8)
fig_all_conditions
dev.off()


# Plotting age-specific unhealthy prevalences
# highlighting some countries
fig_selected<- ggplot()+
  geom_line(data=dis_cron,
            aes(age, unhealthy, group=country, color=country), color="grey90", size=1.2)+
  geom_line(data=dis_cron %>% filter(country%in%"Europe"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1.5)+
  geom_point(data=dis_cron %>% filter(country%in%"Europe"),
             aes(age, unhealthy, group=country, color=country), size=2.3)+
  
  geom_line(data=dis_cron %>% filter(country%in%"US"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1.5)+
  geom_point(data=dis_cron %>% filter(country%in%"US"),
             aes(age, unhealthy, group=country, color=country), size=2.3)+
  
  geom_line(data=dis_cron %>% filter(country%in%"China"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1.5)+
  geom_point(data=dis_cron %>% filter(country%in%"China"),
             aes(age, unhealthy, group=country, color=country), size=2.3)+
  
  geom_line(data=dis_cron %>% filter(country%in%"India"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1.5)+
  geom_point(data=dis_cron %>% filter(country%in%"India"),
             aes(age, unhealthy, group=country, color=country), size=2.3)+
  
  geom_line(data=dis_cron %>% filter(country%in%"Korea"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1.5)+
  geom_point(data=dis_cron %>% filter(country%in%"Korea"),
             aes(age, unhealthy, group=country, color=country), size=2.3)+
  
  geom_line(data=dis_cron %>% filter(country%in%"Mexico"),
            aes(age, unhealthy, group=country, color=country, linetype=country), size=1.5)+
  geom_point(data=dis_cron %>% filter(country%in%"Mexico"),
             aes(age, unhealthy, group=country, color=country), size=2.3)+
  theme_clean(base_size =26)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90),
        panel.border = element_blank(),
        plot.background = element_blank())+
  facet_grid(sex~type)+
  #scale_color_brewer(palette = "PuOr")
  scale_color_manual(values=c("#ab864a","#5d7dc3","#0d3173",
                              "#ba1e68","#6ba772","black"))

pdf(here(figs.folder,"fig_3.pdf"), width = 10, height=10)
fig_selected
dev.off()

fig_selected

# Bar plot
bar_allconditions<-ggplot() +
  geom_bar(data= dis_cron %>%
             arrange(unhealthy) %>% 
             group_by(type) %>% 
             mutate(country=fct_reorder(country, unhealthy,.desc = T)),
           aes(x=country, y=unhealthy, fill=factor(sex)),
           stat = "identity", position = "dodge")+
  xlab("Age") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  scale_fill_manual(values=alpha(c("darkred", "blue"),0.5))+
  scale_color_manual(values=alpha(c("darkred", "blue"),0.7))+
  geom_hline(yintercept=0, linetype="dashed",  color = "black", size=0.5)+
  labs(fill = "Component")+
  theme_minimal(base_size = 12) +
  theme(legend.text=element_text(size=9),
        legend.title=element_text(size=10),
        axis.title =  element_text(size=12),title =  element_text(size=12),
        legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        axis.text.x = element_text( vjust = 0.3, hjust = 1))+
  facet_grid(type~age)+
  xlab("Country")+
  coord_flip()


bar_allconditions

pdf(here(figs.app.folder,"fig2.app.pdf"), width = 17, height=10)
bar_allconditions
dev.off()



# making  a dumbbell to check
dis_cron_bell<-dis_cron %>% 
  pivot_wider(id_cols = c(age,country,type), names_from = sex, values_from = unhealthy)


ggplot(dis_cron_bell %>%
         arrange(woman) %>%
         #  filter(!location%in%"Canada") %>% 
         mutate(country=fct_reorder(country, woman,.desc = T)))+ 
  geom_dumbbell(size=1, color="black",
                aes(y = country, x=woman, xend=man, color=type),
                size_x = 3.5, size_xend = 3.5, colour_x = '#B6407D', colour_xend = '#11718A') +
  facet_grid(type~age)+
  theme_clean()+
  theme(axis.text.x = element_text( vjust = 0.3, hjust = 1, angle = 90))



# directories of each survey.
prev.dir.share<-here("Data","SHARE","Prevalence") 
prev.dir.elsa<-here("Data","ELSA","Prevalence")
prev.dir.lasi<-here("Data","LASI","Prevalence")
prev.dir.charls<-here("Data","CHARLS","Prevalence")
prev.dir.hrs<-here("Data","HRS","Prevalence")
prev.dir.klosa<-here("Data","KLOSA","Prevalence")
prev.dir.mhas<-here("Data","MHAS","Prevalence")

# reading data
prev.share<-fread(here(prev.dir.share,"prev_share_cron_type_pooled_agecat.csv")) %>% 
  mutate(country="Europe") %>% 
  select(-year)
prev.share.cntry<-fread(here(prev.dir.share,"prev_share_cron_type_country_agecat.csv"))%>% 
  select(-year)

prev.elsa<-fread(here(prev.dir.elsa,"prev_elsa_cron_type_agecat.csv")) %>% 
  mutate(country="England")%>% 
  select(-year)

prev.lasi<-fread(here(prev.dir.lasi,"prev_lasi_cron_type_agecat.csv")) %>% 
  mutate(country="India")%>% 
  select(-year)

prev.charls<-fread(here(prev.dir.charls,"prev_charls_cron_type_agecat.csv")) %>% 
  mutate(country="China")%>% 
  select(-year)

prev.hrs<-fread(here(prev.dir.hrs,"prev_hrs_cron_type_agecat.csv")) %>% 
  mutate(country="US")%>% 
  select(-year)

prev.klosa<-fread(here(prev.dir.klosa,"prev_klosa_cron_type_agecat.csv")) %>% 
  mutate(country="Korea")%>% 
  select(-year)

prev.mhas<-fread(here(prev.dir.mhas,"prev_mhas_cron_type_agecat.csv")) %>% 
  mutate(country="Mexico")%>% 
  select(-year)


prev.all<-rbind(prev.share.cntry,prev.share, prev.elsa,prev.lasi,prev.charls,prev.hrs,
                prev.klosa,prev.mhas)

# Plotting age-specific unhealthy precalences by gender and health condition

sex.disease<-ggplot()+
  geom_line(data=prev.all,aes(age, unhealthy, group=country,color=gender),size=1, color="grey90")+
  theme_clean(base_size = 14)+
  facet_grid(gender~type)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  geom_line(data=prev.all,aes(age, unhealthy, group=country,color=gender),size=1)+
  scale_color_manual(values = c('#882255','#009988')) 

pdf(here(figs.app.folder,"fig3.app.pdf"), width = 17, height=10)
sex.disease
dev.off()

sex.disease

# Helper function for string wrapping because names are too large in the facets
# Default 20 character target width.
swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)

prev.all$type = swr(prev.all$type)



# Heatmap of age-specific unhealthy prevalences by country and gender
fig_prev_all<-
  ggplot(prev.all,
         aes(age, country, group=country, fill=unhealthy))+
  geom_raster(interpolate = F) +
  theme_clean(base_size = 24)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90))+
  facet_grid(gender~type)+
  scale_fill_distiller(palette = "Spectral", 
                       name="%Unhealthy", n.breaks=4)
fig_prev_all

pdf(here("Manuscript","Figures","fig_prev_all.pdf"), width = 20, height=24)
fig_prev_all
dev.off()

png(here("Manuscript","Figures","fig_prev_all.png"), width = 2000, height=2200)
fig_prev_all
dev.off()


# Joining all decompositions for ages 60+

decomp.dfle<-fread(here("Manuscript","Data", "decomp_60_dfle.csv")) %>% 
  select(1,2,5:7)


decomp.cfle<-fread(here("Manuscript","Data", "decomp_60_dfle_chronic.csv")) %>% 
  select(1,2,5:7) 

colnames(decomp.cfle)<-c("Country","GAP_CFLE","GAP_LE", "Mort.Cron","Chronic")


decomp.all<-full_join(decomp.cfle,decomp.dfle) %>% 
  mutate(GAP.dfle= cut(GAP_DFLE, breaks=c(-0.4, 0, 1,3,5),include.lowest = TRUE),
         GAP.dfle=factor(GAP.dfle, levels=c("[-0.4,0]", "(0,1]", "(1,3]", "(3,5]"), 
                         labels=c("< 0", "0 - 1", "1 - 3", "3 - 5")))

decomp.cron<-full_join(decomp.cfle,decomp.dfle) %>% 
  mutate(GAP.cfle= cut(GAP_CFLE, breaks=c(-2.4, -1,0, 1, 1.7),include.lowest = TRUE),
         GAP.cfle=factor(GAP.cfle, levels=c("[-2.4,-1]", "(-1,0]", "(0,1]","(1,1.7]"), 
                         labels=c("< -1", "-1 - 0", "0 - 1","1-1.7")))



X11()

# select labels: high and low gap
decomp.all.l<-decomp.all %>% 
  arrange(Country,GAP_DFLE) %>% 
  slice(which.min(GAP_DFLE))


decomp.all.u<-decomp.all %>% 
  arrange(Country,GAP_DFLE) %>% 
  slice(which.max(GAP_DFLE)) 

decomp.all.m<-decomp.all %>% 
  arrange(Country,GAP_DFLE) %>% 
  slice(which.min(Mortality))

decomp.all.d<-decomp.all %>% 
  arrange(Country,GAP_DFLE) %>% 
  slice(which.max(Disability))

d.figs<-rbind(decomp.all.l, decomp.all.u, decomp.all.m, decomp.all.d)

#chronic

decomp.cron.l<-decomp.cron %>% 
  arrange(Country,GAP_CFLE) %>% 
  slice(which.min(GAP_CFLE))


decomp.cron.u<-decomp.cron %>% 
  arrange(Country,GAP_CFLE) %>% 
  slice(which.max(GAP_CFLE)) 

decomp.cron.m<-decomp.cron %>% 
  filter(GAP.cfle%in%c("1-1.7")) %>% 
  arrange(Country,GAP_CFLE) %>% 
  slice(which.max(Mortality))

decomp.cron.d<-decomp.cron %>% 
  arrange(Country,GAP_CFLE) %>% 
  slice(which.max(Mort.Cron))

c.figs<-rbind(decomp.cron.l, decomp.cron.u, decomp.cron.m, decomp.cron.d)
c.figs

X11()


# Plot crossing the disability and mortality contribution to the gender gap
fig4<-ggplot(decomp.all, aes(Mortality,Disability, group=GAP.dfle, 
                             color=GAP.dfle,fill=GAP.dfle))+
  geom_point(size=4)+
  geom_mark_ellipse(data=decomp.all %>% 
                      filter(GAP.dfle%in%c("< 0","3 - 5")),
                    aes(fill = GAP.dfle)) +
  geom_segment( data=decomp.all.l, aes(x=0,xend=Mortality,y=Disability,yend=Disability), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.all.u, aes(x=0,xend=Mortality,y=Disability,yend=Disability), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.all.m, aes(x=0,xend=Mortality,y=Disability,yend=Disability), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.all.d, aes(x=0,xend=Mortality,y=Disability,yend=Disability), linetype="dotted", 
                color = "black", size=1)+
  
  
  geom_segment( data=decomp.all.l, aes(x=Mortality,xend=Mortality,y=Disability,yend=-Inf), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.all.u, aes(x=Mortality,xend=Mortality,y=Disability,yend=-Inf), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.all.m, aes(x=Mortality,xend=Mortality,y=Disability,yend=-Inf), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.all.d, aes(x=Mortality,xend=Mortality,y=Disability,yend=-Inf), linetype="dotted", 
                color = "black", size=1)+
  scale_y_reverse()+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 5.7)) +
  scale_y_continuous(limits = c(-3, 1)) +
  scale_color_brewer(palette="RdBu", direction = -1)+
  scale_fill_brewer(palette="RdBu", direction = -1)+
  theme_classic(base_size =24)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=20),
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  
  ggrepel::geom_text_repel(data=decomp.all.l, 
                           aes(Mortality,Disability, label=Country),
                           box.padding = 0.8, show.legend=FALSE,size = 7,
                           nudge_x = 0.8,
                           nudge_y = 0.3)+
  
  
  ggrepel:: geom_text_repel(data=decomp.all.u, 
                            aes(Mortality,Disability, label=Country),
                            box.padding = 0.8, show.legend=FALSE,size = 7,
                            nudge_x = 0.8,
                            nudge_y = 0.3)+
  ggrepel:: geom_text_repel(data=decomp.all.m, 
                            aes(Mortality,Disability, label=Country),
                            box.padding = 0.8, show.legend=FALSE,size = 7,
                            nudge_x = 0.8,
                            nudge_y = 0.3)+
  ggrepel:: geom_text_repel(data=decomp.all.d, 
                            aes(Mortality,Disability, label=Country),
                            box.padding = 0.8, show.legend=FALSE,size = 7,
                            nudge_x = 0.8,
                            nudge_y = 0.3)



pdf(here("Manuscript","Figures","fig4.2.pdf"), width = 10, height=10)
fig4
dev.off()

X11()

# Plot crossing the chronic condition and mortality contribution to the gender gap
fig4.5<-ggplot(decomp.cron, aes(Mort.Cron,Chronic, group=GAP.cfle, 
                                color=GAP.cfle,fill=GAP.cfle))+
  geom_point(size=4)+
  geom_mark_ellipse(data=decomp.cron %>% 
                      filter(GAP.cfle%in%c("< -1","1-1.7")),
                    aes(fill = GAP.cfle)) +
  geom_segment( data=decomp.cron.l, aes(x=0,xend=Mort.Cron,y=Chronic,yend=Chronic), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.cron.u, aes(x=0,xend=Mort.Cron,y=Chronic,yend=Chronic), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.cron.m, aes(x=0,xend=Mort.Cron,y=Chronic,yend=Chronic), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.cron.d, aes(x=0,xend=Mort.Cron,y=Chronic,yend=Chronic), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.cron.l, aes(x=Mort.Cron,xend=Mort.Cron,y=Chronic,yend=-Inf), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.cron.u, aes(x=Mort.Cron,xend=Mort.Cron,y=Chronic,yend=-Inf), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.cron.m, aes(x=Mort.Cron,xend=Mort.Cron,y=Chronic,yend=-Inf), linetype="dotted", 
                color = "black", size=1)+
  
  geom_segment( data=decomp.cron.d, aes(x=Mort.Cron,xend=Mort.Cron,y=Chronic,yend=-Inf), linetype="dotted", 
                color = "black", size=1)+
  scale_y_continuous(limits = c(-4, 1.7)) +
  scale_color_brewer(palette="RdBu", direction = -1)+
  scale_fill_brewer(palette="RdBu", direction = -1)+
  theme_classic(base_size =24)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=20),
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  
  ggrepel::geom_text_repel(data=decomp.cron.l, 
                           aes(Mort.Cron,Chronic, label=Country),
                           box.padding = 0.8, show.legend=FALSE,size = 7,
                           nudge_x = 1,
                           nudge_y = 0.9)+
  
  
  ggrepel:: geom_text_repel(data=decomp.cron.u, 
                            aes(Mort.Cron,Chronic, label=Country),
                            box.padding = 0.8, show.legend=FALSE,size = 7,
                            nudge_x = 0.8,
                            nudge_y = 0.4)+
  ggrepel:: geom_text_repel(data=decomp.cron.m, 
                            aes(Mort.Cron,Chronic, label=Country),
                            box.padding = 0.8, show.legend=FALSE,size = 7,
                            nudge_x = 0.8,
                            nudge_y = 0.3)+
  ggrepel:: geom_text_repel(data=decomp.cron.d, 
                            aes(Mort.Cron,Chronic, label=Country),
                            box.padding = 0.8, show.legend=FALSE,size = 7,
                            nudge_x = 0.8,
                            nudge_y = 0.4)+
  xlab("Mortality")


pdf(here("Manuscript","Figures","fig4.5.pdf"), width = 10, height=10)
fig4.5
dev.off()
X11()


# making  a dumbbell to check

ggplot(decomp.all, aes(GAP.dfle,Country, group=Country,color=GAP.dfle))

ggplot(decomp.all%>%
         arrange(GAP_DFLE) %>%
         #  filter(!location%in%"Canada") %>% 
         mutate(Country=fct_reorder(Country, GAP_DFLE,.desc = F)))+ 
  geom_dumbbell(size=1, color="black",
                aes(y = Country, x=Mortality, xend=Disability, color=GAP.dfle),
                size_x = 3.5, size_xend = 3.5, colour_x = '#B6407D', colour_xend = '#11718A') +
  # facet_grid(type~age)+
  theme_clean()+
  theme(axis.text.x = element_text( vjust = 0.3, hjust = 1, angle = 90))



#adding correlation

decomp.all<-decomp.all %>% 
  mutate(cor.dfle=cor.test(GAP_LE, GAP_DFLE, method = "pearson", conf.level = 0.95)$estimate,
         cor.dfle.p=cor.test(GAP_LE, GAP_DFLE, method = "pearson", conf.level = 0.95)$p.value)

# doing a rank grphs
# first ranking according to different metrics


decomp.rankings.le <- decomp.all %>% 
  #group_by(Country) %>% 
  arrange((GAP_LE), Country) %>% 
  mutate(ranking.le = row_number()) %>% 
  as.data.frame()

decomp.rankings.dfle <- decomp.rankings.le  %>% 
  #group_by(Country) %>% 
  arrange((GAP_DFLE), Country) %>% 
  mutate(ranking.dfle = row_number()) %>% 
  as.data.frame()


decomp.rankings.dis <- decomp.rankings.dfle  %>% 
  #group_by(Country) %>% 
  arrange(desc(Disability), Country) %>% 
  mutate(ranking.disability = row_number()) %>% 
  as.data.frame()

decomp.rankings.cron <- decomp.rankings.dis  %>% 
  #group_by(Country) %>% 
  arrange(desc(Chronic), Country) %>% 
  mutate(ranking.chronic = row_number()) %>% 
  as.data.frame()

decomp.rankings.cron.gap <- decomp.rankings.cron %>% 
  #group_by(Country) %>% 
  arrange(desc(GAP_CFLE), Country) %>% 
  mutate(ranking.cfle = row_number()) %>% 
  as.data.frame()

rank.values<-decomp.rankings.cron.gap  %>% 
  select(1, 12:16) %>% 
  pivot_longer(2:6,names_to ="Type",values_to = "Rank" )

rank.values$tag<-factor(rank.values$Type, 
                        levels = c("ranking.cfle","ranking.chronic","ranking.dfle", 
                                   "ranking.disability", "ranking.le"),
                        labels=c("GAP CFLE","CHRONIC","GAP DFLE","DISABILITY","GAP LE"))


rank.values<-rank.values %>% 
  mutate(tag=fct_relevel(tag,c("GAP DFLE","DISABILITY","GAP CFLE","CHRONIC","GAP LE"))) %>%
  arrange(tag)


# then plot basic bump chart
X11()

r.bump<-ggplot(rank.values,
       aes(x = tag, y = Rank, color = Country, group=Country)) +
  geom_bump(smooth = 15, size = 2, alpha = 0.2, color="grey70")


rank_dis<-r.bump + 
    geom_bump(data= rank.values%>% 
                filter(Country%in%c("Portugal","Korea","Denmark","US", "India")) ,
              aes(x = tag, y = Rank, color = Country, group=Country),
              smooth = 15, size = 2, inherit.aes = F)+
  
  geom_point(data= rank.values,
             aes(x = tag, y = Rank),
             size = 5,  alpha = 0.5,color="grey70") +
  geom_point(data= rank.values %>% 
               filter(Country%in%c("Portugal","Korea","Denmark","US", "India")),
             aes(x = tag, y = Rank),
             size = 5) +
  geom_segment(data =rank.values,
               aes(x = tag , xend = tag , y = Rank, yend = Rank),
               size = 5,  alpha = 0.5,color="grey70",
               lineend = "round")+
  
  geom_segment(data =rank.values %>% 
                 filter(Country%in%c("Portugal","Korea","Denmark","US", "India")),
               aes(x = tag , xend = tag , y = Rank, yend = Rank),
               size = 2.5,alpha = 0.5,
               lineend = "round")+
  
  geom_text(data = rank.values %>% filter(!Country%in%c("Portugal","Korea","Denmark","US", "India")),
            aes(label = Country, x = tag),
            size = 3,  alpha = 0.5,color="grey70",
            nudge_y = .43,
            nudge_x = -.05,
            size = 3.5,
            fontface = 2,
            hjust = 0) +
  
  geom_text(data = rank.values %>% filter(Country%in%c("Portugal","Korea","Denmark","US", "India")),
            aes(label = Country, x = tag),
            nudge_y = .5,
            nudge_x = -.07,
            size = 5,
            fontface = 2,
            hjust = 0) +
  scale_color_manual(values=c("#ab864a","#5d7dc3","#0d3173",
                              "#ba1e68","#6ba772","black"))+
  scale_y_reverse(breaks = 1:nrow(rank.values))+
  labs(  y = "Rank")+
  labs(  x = "")+
  my_theme()+
  theme(legend.position = "none")
  

rank_dis

pdf(here("Manuscript","Figures","fig4.pdf"), width = 15, height=10)
rank_dis
dev.off()





# basic version

#ggplot(data = rank.values, aes(x = tag, y = Rank, group = Country)) +
#  geom_line(aes(color = Country, alpha = 1), size = 2) +
#  geom_point(aes(color = Country, alpha = 1), size = 4) +
#  scale_y_reverse(breaks = 1:nrow(rank.values))##



#rank.values <- rank.values %>%
#  filter(tag%in%c("GAP DFLE","DISABILITY")) %>% 
#  mutate(flag = ifelse(Country %in%c("Portugal","Korea","Denmark","US", "China"), TRUE, FALSE),
#         country_col = if_else(flag == TRUE, Country, "zzz"))#
#
#ggplot(data = rank.values, aes(x = tag, y = Rank, group = Country)) +
#  geom_line(aes(color = country_col, alpha = 1), size = 3) +
#  geom_point(color = "#FFFFFF", size = 4) +
#  geom_point(aes(color = country_col, alpha = 1), size = 4) +
#  geom_point(color = "#FFFFFF", size = 1) +
#  scale_y_reverse(breaks = 1:nrow(rank.values))+
#  # scale_x_continuous(breaks = 1:16, minor_breaks = 1:16, expand = c(.05, .05)) +
#  geom_text(data = rank.values %>% filter(tag == "GAP DFLE"),
#            aes(label = Country, x = 0.8) , fontface = "bold", color = "grey90", size = 6) +
#  geom_text(data = rank.values %>% filter(tag == "DISABILITY"),
#            aes(label = Country, x = 2.2) , fontface = "bold", color = "grey90", size = 6) +
#  
#  geom_text(data = rank.values %>% filter(tag == "GAP DFLE" & Country %in%c("Portugal","Korea","Denmark","US", "China")),
#            aes(label = Country, x = 0.8, color=Country) , fontface = "bold", size = 6) +
#  geom_text(data = rank.values %>% filter(tag == "DISABILITY" &Country %in%c("Portugal","Korea","Denmark","US", "China")),
#            aes(label = Country,  x = 2.2, color=Country) , fontface = "bold", size = 6) +
#  # coord_cartesian(ylim = c(1,show.top.n)) + 
#  theme(legend.position = "none") +
#  labs(  y = "Rank")+
#  my_theme()+
#  
#  scale_color_manual(values = c("#CA0020", "#F4A582", "black", "#92C5DE", "#0571B0","grey90"))


#CA0020
