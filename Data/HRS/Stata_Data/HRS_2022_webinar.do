clear all
pause on
set more off

***define files at their locations***
local rand_hrs "randhrs1992_2018v1.dta"
local harmonized_hrs "H_HRS_c.dta"
local hrs_2014_lb "h14lb_r.dta"

***Load RAND HRS Longitudinal File observations and variables***
use hhid pn hhidpn raehsamp raestrat ragender r12agey_e using "`rand_hrs'"
pause

***Merge in Harmonized HRS variables
merge 1:1 hhidpn using "`harmonized_hrs'", keepusing(inw12sc r12scwtresp r12gcaresck r12lsatsc) nogen
pause

***Merge in HRS 2014 self-completion questionnaire variables
rename hhid HHID
rename pn PN
merge 1:1 HHID PN using "`hrs_2014_lb'", keepusing(OLB024 OLB025)
drop if _merge == 2
drop _merge
pause

*Keep only respondents from Wave 12 self-completion questionnaire
keep if inw12sc == 1
pause

***Apply weights
svyset raehsamp [pw=r12scwtresp], strata(raestrat) 
pause

***Analyze satisfaction with life
svy: mean r12lsatsc
pause

***Analyze satisfaction with life by whether providing care
svy: mean r12lsatsc, over(r12gcaresck)
pause
test c.r12lsatsc@0.r12gcaresck = c.r12lsatsc@1.r12gcaresck
pause

***Analyze satisfaction with life by social control
svy: reg r12lsatsc OLB024
pause

***Analyze satisfaction with life by financial control
svy: reg r12lsatsc OLB025
pause

***Analyze satisfaction with life by whether providing care while controlling for demographic variables
svy: reg r12lsatsc i.r12gcaresck i.ragender r12agey 
pause

***Analyze satisfaction with life by whether providing care while controlling for demographic variables and social control
svy: reg r12lsatsc i.r12gcaresck i.ragender r12agey OLB024
pause

***Analyze satisfaction with life by whether providing care while controlling for demographic variables and social and financial control
svy: reg r12lsatsc i.r12gcaresck i.ragender r12agey OLB024 OLB025

