####---- Description -------------------------------------------------------------------------

## Analysis for baseline characteristics of CPRD females of reproductive age (15-49yo) for Neha's SRHR overview chapter
## Date started: 14/20/2020
## Author: Neha Pathak

## Load packages -------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Hmisc)
library(epiR)
library(data.table)
library(psych)

## Set working directory ------------------------------------------------------------------

setwd("filepath")

## Load final datasets for analysis .Rdata files ------------------------------------------------------

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")

## MAIN ANALYSIS COHORT ------------------------

## Totals -------------------------------------------------------------------------------------------------

## Whole cohort 
total_cohort_n <- count(reproductive_cohort_final)
total_pyears <- sum(reproductive_cohort_final$pyears)
total_wholecohort <- bind_cols(total_cohort_n, total_pyears)
colnames(total_wholecohort) <- c("individuals", "pyears")
total_wholecohort <- total_wholecohort %>% mutate(group = "Whole cohort") 

## By migrant status
total_mvnm_n <- reproductive_cohort_final %>% group_by(migrant_status) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migrant_status)
total_mvnm_pyears <- reproductive_cohort_final %>% group_by(migrant_status) %>%
  tally(pyears) %>%
  rename(pyears = n) %>% 
  rename(group = migrant_status)
total_mvnm <- full_join(total_mvnm_n, total_mvnm_pyears,
                        by = c("group" = "group"))

## By migrant certainty
total_dp_n <- reproductive_cohort_final %>% group_by(migcertainty) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migcertainty)
total_dp_pyears <- reproductive_cohort_final %>% group_by(migcertainty) %>%
  tally(pyears) %>%
  rename(pyears = n) %>% 
  rename(group = migcertainty)
total_dp <- full_join(total_dp_n, total_dp_pyears,
                        by = c("group" = "group"))

## Totals for export
totals <- full_join(total_mvnm, total_dp,
                    by = c("group" = "group", "individuals" = "individuals", "pyears" = "pyears")) %>% 
 full_join(total_wholecohort, by = c("group" = "group", "individuals" = "individuals", "pyears" = "pyears")) %>% 
  mutate(across(is.numeric, ~ round(.,0)))
total_cohort_n<- as.vector(total_cohort_n$n[1])
totals_percent <- totals %>% mutate(percent = (individuals/total_cohort_n)*100) %>% 
  mutate(percent = round(percent, 1))

totals_percent$individuals <- paste(totals_percent$individuals, totals_percent$percent, sep =", ")
totals_percent <- dplyr::select(totals_percent, c(group, individuals, pyears))
write_csv(totals_percent, "filepath")


## Age at cohort entry --------------------------------------------------------------------------------------

## Mean + Median + SD + IQR Age at cohort entry 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

reproductive_cohort_final_age <- reproductive_cohort_final %>%
  mutate(age_cohort_entry = calc_age(dob, cohort_entry))
mean_ages_overall <- describe(reproductive_cohort_final_age$age_cohort_entry, IQR = TRUE)
mean_ages_overall

nm <- reproductive_cohort_final_age %>% filter(migrant_status == "Non-migrant")
nm_age <- describe(nm$age_cohort_entry, IQR = TRUE)
nm_age

m <- reproductive_cohort_final_age %>% filter(migrant_status == "Migrant")
m_age <- describe(m$age_cohort_entry, IQR = TRUE)
m_age

d <- reproductive_cohort_final_age %>% filter(migcertainty == "Definite")
d_age <- describe(d$age_cohort_entry, IQR = TRUE)
d_age

p <- reproductive_cohort_final_age %>% filter(migcertainty == "Probable")
p_age <- describe(p$age_cohort_entry, IQR = TRUE)
p_age

## join export mean + median ages + sd + IQR
mean_ages_overall <- as.data.frame((mean_ages_overall))
mean_ages_overall <- dplyr::select(mean_ages_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_age <- as.data.frame((nm_age))
nm_age <- dplyr::select(nm_age, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_age <- as.data.frame((m_age))
m_age <- dplyr::select(m_age, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_age <- as.data.frame((d_age))
d_age <- dplyr::select(d_age, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_age <- as.data.frame((p_age))
p_age <- dplyr::select(p_age, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_ages <- full_join(mean_ages_overall, nm_age,
                           by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_age, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_age, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_age, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_ages$mean_sd <- paste(all_mean_ages$mean, all_mean_ages$sd, sep =", ")
all_mean_ages$median_iqr <- paste(all_mean_ages$median, all_mean_ages$IQR, sep =", ")
all_mean_ages <- dplyr::select(all_mean_ages, c(group, mean_sd, median_iqr))
write_csv(all_mean_ages, "filepath" )



## Age category - No individuals and p years per group 
reproductive_cohort_final_agecat <- reproductive_cohort_final_age %>%
  mutate(agecat_cohort_entry = age_cohort_entry)
reproductive_cohort_final_agecat <- reproductive_cohort_final_agecat %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 15, 19), 1)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 20, 24), 2)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 25, 29), 3)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 30, 34), 4)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 35, 39), 5)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 40, 44), 6)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 45, 49), 7)) 
reproductive_cohort_final_agecat$agecat_cohort_entry <- factor(reproductive_cohort_final_agecat$agecat_cohort_entry, levels = c(1,2,3,4,5,6,7),
                                    labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

agecat <- reproductive_cohort_final_agecat %>% 
  group_by(agecat_cohort_entry) %>% count() 
agecat <- agecat %>% mutate(percent = (n/sum(agecat$n))*100) %>%
  mutate(group = "Whole Cohort")
agecat_mvnm <- reproductive_cohort_final_agecat %>% 
  group_by(agecat_cohort_entry, migrant_status) %>% count() %>% 
  rename(group = migrant_status)
agecat_m <- agecat_mvnm %>% filter(group == "Migrant")
agecat_m <- agecat_m %>% mutate(percent = (n/sum(agecat_m$n))*100)
agecat_nm <- agecat_mvnm %>% filter(group == "Non-migrant")
agecat_nm <- agecat_nm %>% mutate(percent = (n/sum(agecat_nm$n))*100)
agecat_dp <- reproductive_cohort_final_agecat %>% 
  group_by(agecat_cohort_entry, migcertainty) %>% count() %>% 
  rename(group = migcertainty)
agecat_d <- agecat_dp %>% filter(group == "Definite")
agecat_d <- agecat_d %>% mutate(percent = (n/sum(agecat_d$n))*100)
agecat_p <- agecat_dp %>% filter(group == "Probable")
agecat_p <- agecat_p %>% mutate(percent = (n/sum(agecat_p$n))*100)
age_cat_all <- agecat %>% full_join(agecat_m,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                                       "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(agecat_nm,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                              "group"="group","n"="n", "percent" = "percent")) %>%
  full_join(agecat_d,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                             "group"="group","n"="n", "percent" = "percent")) %>%
  full_join(agecat_p,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                             "group"="group","n"="n", "percent" = "percent")) %>%
  rename(agecat = agecat_cohort_entry) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
age_cat_all <- age_cat_all %>% relocate(group) %>% arrange(group)
age_cat_all$n_percent <- paste(age_cat_all$n, age_cat_all$percent, sep =",")

pyears_agecat <- aggregate(pyears ~ agecat_cohort_entry, reproductive_cohort_final_agecat, sum) %>% 
  mutate(group = "Whole Cohort")
pyears_agecat_mvnm <- aggregate(pyears ~ agecat_cohort_entry + migrant_status, reproductive_cohort_final_agecat, sum) %>% 
  rename(group = migrant_status)
pyears_agecat_dp <- aggregate(pyears ~ agecat_cohort_entry + migcertainty, reproductive_cohort_final_agecat, sum) %>% 
  rename(group = migcertainty)
pyears_agecat_all <- pyears_agecat_mvnm %>% full_join(pyears_agecat_dp,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                                                 "group"="group","pyears"="pyears")) %>% 
  full_join(pyears_agecat,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                     "group"="group","pyears"="pyears")) %>%
  rename(agecat = agecat_cohort_entry) 

pyears_agecat_export <- full_join(pyears_agecat_all, age_cat_all, 
                                  by = c("agecat"="agecat","group"="group")) %>% 
  mutate(across(is.numeric, ~ round(.,0))) %>% 
  dplyr::select(c(group, agecat, n_percent, pyears)) 
  
write_csv(pyears_agecat_export, "filepath")

## Ethnicity ---------------------------------------------------------------------------------

reproductive_cohort_final$ethnicat6 <- reproductive_cohort_final$ethnicat6 %>% fct_explicit_na(na_level = "Not known")
levels(reproductive_cohort_final$ethnicat6)

ethnicat6 <- reproductive_cohort_final %>% 
  group_by(ethnicat6) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
ethnicat6_mvnm <- reproductive_cohort_final %>% 
  group_by(ethnicat6, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(ethnicat6, group, n, percent))
ethnicat6_dp <- reproductive_cohort_final %>% 
  group_by(ethnicat6, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(ethnicat6, group, n, percent))

ethnicat6_all <- ethnicat6_mvnm %>% full_join(ethnicat6_dp,by = c("ethnicat6"="ethnicat6",
                                                           "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(ethnicat6,by = c("ethnicat6"="ethnicat6",
                          "group"="group","n"="n", "percent" = "percent")) %>%
  rename(ethnicat6 = ethnicat6)
ethnicat6_all$n_percent <- paste(ethnicat6_all$n, ethnicat6_all$percent, sep =",")
ethnicat6_all <- ethnicat6_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, ethnicat6, n_percent)

pyears_ethnicat6 <- reproductive_cohort_final %>% group_by(ethnicat6) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_ethnicat6_mvnm <- reproductive_cohort_final %>% group_by(migrant_status, ethnicat6) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_ethnicat6_dp <- reproductive_cohort_final %>% group_by(migcertainty, ethnicat6) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_ethnicat6_all <- pyears_ethnicat6_mvnm %>% full_join(pyears_ethnicat6_dp,by = c("ethnicat6"="ethnicat6",
                                                                               "group"="group","n"="n")) %>% 
  full_join(pyears_ethnicat6,by = c("ethnicat6"="ethnicat6",
                                 "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_ethnicat6_export <- full_join(pyears_ethnicat6_all, ethnicat6_all, 
                                  by = c("ethnicat6"="ethnicat6","group"="group")) %>% 
  relocate(group,ethnicat6,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_ethnicat6_export, "filepath")

## practice region --------------------------------

reproductive_cohort_final$prac_region <- reproductive_cohort_final$prac_region %>% fct_explicit_na(na_level = "Not known")
levels(reproductive_cohort_final$prac_region)

prac_region <- reproductive_cohort_final %>% 
  group_by(prac_region) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
prac_region_mvnm <- reproductive_cohort_final %>% 
  group_by(prac_region, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(prac_region, group, n, percent))
prac_region_dp <- reproductive_cohort_final %>% 
  group_by(prac_region, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(prac_region, group, n, percent))

prac_region_all <- prac_region_mvnm %>% full_join(prac_region_dp,by = c("prac_region"="prac_region",
                                                                  "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(prac_region,by = c("prac_region"="prac_region",
                             "group"="group","n"="n", "percent" = "percent")) %>%
  rename(prac_region = prac_region)
prac_region_all$n_percent <- paste(prac_region_all$n, prac_region_all$percent, sep =",")
prac_region_all <- prac_region_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, prac_region, n_percent)

pyears_prac_region <- reproductive_cohort_final %>% group_by(prac_region) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_prac_region_mvnm <- reproductive_cohort_final %>% group_by(migrant_status, prac_region) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_prac_region_dp <- reproductive_cohort_final %>% group_by(migcertainty, prac_region) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_prac_region_all <- pyears_prac_region_mvnm %>% full_join(pyears_prac_region_dp,by = c("prac_region"="prac_region",
                                                                                       "group"="group","n"="n")) %>% 
  full_join(pyears_prac_region,by = c("prac_region"="prac_region",
                                    "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_prac_region_export <- full_join(pyears_prac_region_all, prac_region_all, 
                                     by = c("prac_region"="prac_region","group"="group")) %>% 
  relocate(group,prac_region,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_prac_region_export, "filepath")

## IMD -------------------------------------

reproductive_cohort_final$imd<- reproductive_cohort_final$imd%>% fct_explicit_na(na_level = "Not known")
levels(reproductive_cohort_final$imd)

imd<- reproductive_cohort_final %>% 
  group_by(imd) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
imd_mvnm <- reproductive_cohort_final %>% 
  group_by(imd, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(imd, group, n, percent))
imd_dp <- reproductive_cohort_final %>% 
  group_by(imd, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(imd, group, n, percent))

imd_all <- imd_mvnm %>% full_join(imd_dp,by = c("imd"="imd",
                               "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(imd,by = c("imd"="imd",
                               "group"="group","n"="n", "percent" = "percent")) %>%
  rename(imd= imd)
imd_all$n_percent <- paste(imd_all$n, imd_all$percent, sep =",")
imd_all <- imd_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, imd, n_percent)

pyears_imd <- reproductive_cohort_final %>% group_by(imd) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_imd_mvnm <- reproductive_cohort_final %>% group_by(migrant_status, imd) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_imd_dp <- reproductive_cohort_final %>% group_by(migcertainty, imd) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_imd_all <- pyears_imd_mvnm %>% full_join(pyears_imd_dp,by = c("imd"="imd",
                                                                                             "group"="group","n"="n")) %>% 
  full_join(pyears_imd,by = c("imd"="imd",
                                      "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_imd_export <- full_join(pyears_imd_all, imd_all, 
                                       by = c("imd"="imd","group"="group")) %>% 
  relocate(group,imd,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_imd_export, "filepath")

## Year of cohort entry -----------------------------------------------------------

reproductive_cohort_final_yearofentry <- reproductive_cohort_final %>% mutate(yearofentry = year(cohort_entry))
mean_yearofentry_overall <- describe(reproductive_cohort_final_yearofentry$yearofentry, IQR = TRUE)
mean_yearofentry_overall

nm <- reproductive_cohort_final_yearofentry %>% filter(migrant_status == "Non-migrant")
nm_yearofentry <- describe(nm$yearofentry, IQR = TRUE)
nm_yearofentry

m <- reproductive_cohort_final_yearofentry %>% filter(migrant_status == "Migrant")
m_yearofentry <- describe(m$yearofentry, IQR = TRUE)
m_yearofentry

d <- reproductive_cohort_final_yearofentry %>% filter(migcertainty == "Definite")
d_yearofentry  <- describe(d$yearofentry, IQR = TRUE)
d_yearofentry 

p <- reproductive_cohort_final_yearofentry %>% filter(migcertainty == "Probable")
p_yearofentry <- describe(p$yearofentry, IQR = TRUE)
p_yearofentry

## join export mean yearsof entry + sds
mean_yearofentry_overall <- as.data.frame((mean_yearofentry_overall))
mean_yearofentry_overall <- dplyr::select(mean_yearofentry_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_yearofentry<- as.data.frame((nm_yearofentry))
nm_yearofentry <- dplyr::select(nm_yearofentry, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_yearofentry <- as.data.frame((m_yearofentry))
m_yearofentry <- dplyr::select(m_yearofentry, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_yearofentry <- as.data.frame((d_yearofentry))
d_yearofentry <- dplyr::select(d_yearofentry, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_yearofentry <- as.data.frame((p_yearofentry))
p_yearofentry <- dplyr::select(p_yearofentry, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_yearofentry <- full_join(mean_yearofentry_overall, nm_yearofentry,
                           by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_yearofentry, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_yearofentry, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_yearofentry, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_yearofentry$mean_sd <- paste(all_mean_yearofentry$mean, all_mean_yearofentry$sd, sep =", ")
all_mean_yearofentry$median_iqr <- paste(all_mean_yearofentry$median, all_mean_yearofentry$IQR, sep =", ")
all_mean_yearofentry <- dplyr::select(all_mean_yearofentry, c(group, mean_sd, median_iqr))
write_csv(all_mean_yearofentry, "filepath")

reproductive_cohort_final_yearofentry$yearofentry <- factor(reproductive_cohort_final_yearofentry$yearofentry,  levels = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018), 
                                                            labels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
yearofentry <- reproductive_cohort_final_yearofentry %>% 
  group_by(yearofentry) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
yearofentry_mvnm <- reproductive_cohort_final_yearofentry %>% 
  group_by(yearofentry, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(yearofentry, group, n, percent))
yearofentry_dp <- reproductive_cohort_final_yearofentry %>% 
  group_by(yearofentry, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(yearofentry, group, n, percent))

yearofentry_all <- yearofentry_mvnm %>% full_join(yearofentry_dp,by = c("yearofentry"="yearofentry",
                       "group"="group","n"="n", "percent" = "percent")) %>%
 full_join(yearofentry, by = c("yearofentry" = "yearofentry", "group" = "group", "n" = "n", "percent" = "percent"))
yearofentry_all$n_percent <- paste(yearofentry_all$n, yearofentry_all$percent, sep =",")
yearofentry_all <- yearofentry_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, yearofentry, n_percent)

pyears_yearofentry <- reproductive_cohort_final_yearofentry %>% group_by(yearofentry) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_yearofentry_mvnm <- reproductive_cohort_final_yearofentry %>% group_by(migrant_status, yearofentry) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_yearofentry_dp <- reproductive_cohort_final_yearofentry %>% group_by(migcertainty, yearofentry) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_yearofentry_all <- pyears_yearofentry_mvnm %>% full_join(pyears_yearofentry_dp,by = c("yearofentry"="yearofentry",
                                                                     "group"="group","n"="n")) %>% 
  full_join(pyears_yearofentry,by = c("yearofentry"="yearofentry",
                              "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_yearofentry_export <- full_join(pyears_yearofentry_all, yearofentry_all, 
                               by = c("group"="group","yearofentry"="yearofentry")) %>% 
  relocate(group,yearofentry,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_yearofentry_export, "filepath")

## Year ----------------------------------------------------------------------------

eventyear<- srh_annual_counts_final_extra %>% 
  group_by(eventyear) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
eventyear_mvnm <- srh_annual_counts_final_extra %>% 
  group_by(eventyear, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(eventyear, group, n, percent))
eventyear_dp <- srh_annual_counts_final_extra %>% 
  group_by(eventyear, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(eventyear, group, n, percent))

eventyear_all <- eventyear_mvnm %>% full_join(eventyear_dp,by = c("eventyear"="eventyear",
                                                "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(eventyear,by = c("eventyear"="eventyear",
                       "group"="group","n"="n", "percent" = "percent")) %>%
  rename(eventyear= eventyear)
eventyear_all$n_percent <- paste(eventyear_all$n, eventyear_all$percent, sep =",")
eventyear_all <- eventyear_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, eventyear, n_percent)

pyears_eventyear <- srh_annual_counts_final_extra %>% group_by(eventyear) %>% tally(pyears_ey) %>% 
  mutate(group = "Whole Cohort")
pyears_eventyear_mvnm <- srh_annual_counts_final_extra %>% group_by(migrant_status, eventyear) %>% tally(pyears_ey) %>% 
  rename(group = migrant_status)
pyears_eventyear_dp <- srh_annual_counts_final_extra %>% group_by(migcertainty, eventyear) %>% tally(pyears_ey) %>% 
  rename(group = migcertainty)
pyears_eventyear_all <- pyears_eventyear_mvnm %>% full_join(pyears_eventyear_dp,by = c("eventyear"="eventyear",
                                                                     "group"="group","n"="n")) %>% 
  full_join(pyears_eventyear,by = c("eventyear"="eventyear",
                              "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_eventyear_export <- full_join(pyears_eventyear_all, eventyear_all, 
                               by = c("eventyear"="eventyear","group"="group")) %>% 
  relocate(group,eventyear,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_eventyear_export, "filepath")

## bar chart of proportions
eventyear_all <- eventyear_mvnm %>% full_join(eventyear_dp,by = c("eventyear"="eventyear",
                                                                  "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(eventyear,by = c("eventyear"="eventyear",
                             "group"="group","n"="n", "percent" = "percent"))
eventyear_all$group <- factor(eventyear_all$group, levels = c("Whole Cohort", "Non-migrant", "Migrant", "Definite", "Probable"))
eventyear_all$group <- recode(eventyear_all$group, "Whole Cohort" = "Whole Cohort", "Non-migrant" = "Non-migrants", "Migrant" = "Migrants", 
                              "Definite" = "Definite Migrants", "Probable" = "Probable Migrants")
eventyear_all_dropdp <- eventyear_all %>% filter(group == "Whole Cohort" | group == "Non-migrants" |group == "Migrants")
eventyear_all_dropdp$group <- droplevels(eventyear_all_dropdp$group)
levels(eventyear_all_dropdp$group)
year_line_point <- ggplot(eventyear_all_dropdp, aes(group = group, x = eventyear, y = percent, colour = group)) + 
  geom_line() + 
  geom_point() +
  ggtitle(str_wrap("Percentage of cohort contributing per year of study period stratified by migration status", 60)) +
  labs(y="Percentage of cohort", x = "Year of study",  color = "Key")
print(year_line_point)
ggsave("filepath", width = 7, height = 5, dpi = 300)

## Person years --------------------------------

mean_pyears_overall <- describe(reproductive_cohort_final$pyears, IQR = TRUE)
mean_pyears_overall

nm <- reproductive_cohort_final %>% filter(migrant_status == "Non-migrant")
nm_pyears <- describe(nm$pyears, IQR = TRUE)
nm_pyears

m <- reproductive_cohort_final %>% filter(migrant_status == "Migrant")
m_pyears <- describe(m$pyears, IQR = TRUE)
m_pyears

d <- reproductive_cohort_final %>% filter(migcertainty == "Definite")
d_pyears <- describe(d$pyears, IQR = TRUE)
d_pyears

p <- reproductive_cohort_final %>% filter(migcertainty == "Probable")
p_pyears <- describe(p$pyears, IQR = TRUE)
p_pyears

## join export mean + median ages + sd + IQR
mean_pyears_overall <- as.data.frame((mean_pyears_overall))
mean_pyears_overall <- dplyr::select(mean_pyears_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_pyears <- as.data.frame((nm_pyears))
nm_pyears <- dplyr::select(nm_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_pyears <- as.data.frame((m_pyears))
m_pyears <- dplyr::select(m_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_pyears <- as.data.frame((d_pyears))
d_pyears <- dplyr::select(d_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_pyears <- as.data.frame((p_pyears))
p_pyears <- dplyr::select(p_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_pyears <- full_join(mean_pyears_overall, nm_pyears,
                           by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_pyears, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_pyears, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_pyears, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_pyears$mean_sd <- paste(all_mean_pyears$mean, all_mean_pyears$sd, sep =", ")
all_mean_pyears$median_iqr <- paste(all_mean_pyears$median, all_mean_pyears$IQR, sep =", ")
all_mean_pyears <- dplyr::select(all_mean_pyears, c(group, mean_sd, median_iqr))
write_csv(all_mean_pyears, "filepath")

## summary outcome data ---- 

#cons
a <- srh_annual_counts_final %>% group_by(patid) %>% tally(consultations_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "All-cause")
b <-  srh_annual_counts_final %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(consultations_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "All-cause")
c <-  srh_annual_counts_final %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(consultations_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "All-cause")
#abortion
d <- srh_annual_counts_final %>% group_by(patid) %>% tally(abortion_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "Abortion")
e <-  srh_annual_counts_final %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(abortion_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "Abortion")
f <-  srh_annual_counts_final %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(abortion_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "Abortion")
#ec
g <- srh_annual_counts_final %>% group_by(patid) %>% tally(ec_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "Emergency contraception")
h <-  srh_annual_counts_final %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(ec_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "Emergency contraception")
i <-  srh_annual_counts_final %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(ec_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "Emergency contraception")
#dva
j <- srh_annual_counts_final %>% group_by(patid) %>% tally(dva_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "DVA")
k <-  srh_annual_counts_final %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(dva_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "DVA")
l <-  srh_annual_counts_final %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(dva_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "DVA")
#chlamydia_testing
m <- srh_annual_counts_final %>% group_by(patid) %>% tally(chlamydia_test_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "chlamydia_testing")
n <-  srh_annual_counts_final %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(chlamydia_test_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "chlamydia_testing")
o <-  srh_annual_counts_final %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(chlamydia_test_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "chlamydia_testing")
#infertiity_management
p <- srh_annual_counts_final %>% group_by(patid) %>% tally(infertility_management_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "infertility_management")
q <-  srh_annual_counts_final %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(infertility_management_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "infertility_management")
r <-  srh_annual_counts_final %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(infertility_management_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "infertility_management")
#cervical_screening
s <- srh_annual_counts_final %>% group_by(patid) %>% tally(cervical_screening_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "cervical_screening")
t <-  srh_annual_counts_final %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(cervical_screening_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "cervical_screening")
u <-  srh_annual_counts_final %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(cervical_screening_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "cervical_screening")
#join 
summary_outcomes_all <- full_join(a,b, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(c, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(d, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(e, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(f, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(g, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(h, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(i, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(j, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(k, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(l, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(m, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(n, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(o, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(p, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(q, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(r, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(s, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(t, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) %>%
  full_join(u, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max")) 

write_csv(summary_outcomes_all, "filepath")

## yearly plot of all-cause cons no.events


annual_cons <- srh_annual_counts_final %>% group_by(eventyear) %>% tally(consultations_n) 
options(scipen=10000000)
annual_cons_plot <- ggplot(annual_cons) +
  geom_point(aes(x=eventyear, y=n), color = "mediumblue") +
  geom_line(aes(x=eventyear, y=n), color = "mediumblue" ) +
  ylim(0,3500000) +
  ggtitle("Annual all-cause consultations events") +
  labs(y="Number of events", x = "Year")
print(annual_cons_plot)
ggsave("filepath")

## yearly plot of SRH outcomes no events 
abortion_cons_n_annual <- srh_annual_counts_final %>% group_by(eventyear) %>% tally(abortion_n) %>% mutate(outcome = "Abortion")
ec_cons_n_annual <- srh_annual_counts_final %>% group_by(eventyear) %>% tally(ec_n) %>% mutate(outcome = "Emergency contraception")
dva_cons_n_annual <- srh_annual_counts_final %>% group_by(eventyear) %>% tally(dva_n) %>% mutate(outcome = "Domestic violence and abuse")
chlamydia_test_cons_n_annual <- srh_annual_counts_final %>% group_by(eventyear) %>% tally(chlamydia_test_n) %>% mutate(outcome = "Chlamydia testing")
infertility_management_cons_n_annual <- srh_annual_counts_final %>% group_by(eventyear) %>% tally(infertility_management_n) %>% mutate(outcome = "Infertility management")
cervical_screening_cons_n_annual <- srh_annual_counts_final %>% group_by(eventyear) %>% tally(cervical_screening_n) %>% mutate(outcome = "Cervical screening")

srh_outcomes_annual <- full_join(abortion_cons_n_annual,ec_cons_n_annual, by = c("outcome"= "outcome", "eventyear" = "eventyear", "n" = "n")) %>% 
  full_join(dva_cons_n_annual, by = c("outcome"= "outcome", "eventyear" = "eventyear", "n" = "n")) %>% 
  full_join(chlamydia_test_cons_n_annual , by = c("outcome"= "outcome", "eventyear" = "eventyear", "n" = "n")) %>% 
  full_join(infertility_management_cons_n_annual,by = c("outcome"= "outcome", "eventyear" = "eventyear", "n" = "n")) %>% 
  full_join(cervical_screening_cons_n_annual, by = c("outcome"= "outcome", "eventyear" = "eventyear", "n" = "n"))
srh_outcomes_annual$outcome <- factor(srh_outcomes_annual$outcome, c("Abortion", "Emergency contraception", "Domestic violence and abuse", "Chlamydia testing" , "Infertility management", "Cervical screening"))

options(scipen=10000000)
srh_outcomes_annual_plot <- ggplot(srh_outcomes_annual) +
  geom_point(aes(x=eventyear, y=n, group = outcome, color = outcome)) +
  geom_line(aes(x=eventyear, y=n, group = outcome, color = outcome) ) +
  ggtitle("Annual all-cause consultations events") +
  labs(y="Number of events", x = "Year", color = "Key")
print(srh_outcomes_annual_plot)
ggsave("filepath")

## EXACT MATCHED COHORT 4:1 --------------------------

# Load final datasets for analysis .Rdata files 

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")

## Totals -------------------------------------------------------------------------------------------------

## Whole cohort 
total_cohort_n <- count(exact_match_reproductive_cohort_final_4to1)
total_pyears <- sum(exact_match_reproductive_cohort_final_4to1$pyears)
total_wholecohort <- bind_cols(total_cohort_n, total_pyears)
colnames(total_wholecohort) <- c("individuals", "pyears")
total_wholecohort <- total_wholecohort %>% mutate(group = "Whole cohort") 

## By migrant status
total_mvnm_n <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migrant_status) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migrant_status)
total_mvnm_pyears <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migrant_status) %>%
  tally(pyears) %>%
  rename(pyears = n) %>% 
  rename(group = migrant_status)
total_mvnm <- full_join(total_mvnm_n, total_mvnm_pyears,
                        by = c("group" = "group"))

## By migrant certainty
total_dp_n <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migcertainty) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migcertainty)
total_dp_pyears <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migcertainty) %>%
  tally(pyears) %>%
  rename(pyears = n) %>% 
  rename(group = migcertainty)
total_dp <- full_join(total_dp_n, total_dp_pyears,
                      by = c("group" = "group"))

## Totals for export
totals <- full_join(total_mvnm, total_dp,
                    by = c("group" = "group", "individuals" = "individuals", "pyears" = "pyears")) %>% 
  full_join(total_wholecohort, by = c("group" = "group", "individuals" = "individuals", "pyears" = "pyears")) %>% 
  mutate(across(is.numeric, ~ round(.,0)))
total_cohort_n<- as.vector(total_cohort_n$n[1])
totals_percent <- totals %>% mutate(percent = (individuals/total_cohort_n)*100) %>% 
  mutate(percent = round(percent, 1))

totals_percent$individuals <- paste(totals_percent$individuals, totals_percent$percent, sep =", ")
totals_percent <- dplyr::select(totals_percent, c(group, individuals, pyears))
write_csv(totals_percent, "filepath")

## Age --------------------------------------------------------------------------------------

## Mean + Median + SD + IQR Age at cohort entry 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

exact_match_reproductive_cohort_final_4to1_age <- exact_match_reproductive_cohort_final_4to1 %>%
  mutate(age_cohort_entry = calc_age(dob, cohort_entry))
mean_ages_overall <- describe(exact_match_reproductive_cohort_final_4to1_age$age_cohort_entry, IQR = TRUE)
mean_ages_overall

nm <- exact_match_reproductive_cohort_final_4to1_age %>% filter(migrant_status == "Non-migrant")
nm_age <- describe(nm$age_cohort_entry, IQR = TRUE)
nm_age

m <- exact_match_reproductive_cohort_final_4to1_age %>% filter(migrant_status == "Migrant")
m_age <- describe(m$age_cohort_entry, IQR = TRUE)
m_age

d <- exact_match_reproductive_cohort_final_4to1_age %>% filter(migcertainty == "Definite")
d_age <- describe(d$age_cohort_entry, IQR = TRUE)
d_age

p <- exact_match_reproductive_cohort_final_4to1_age %>% filter(migcertainty == "Probable")
p_age <- describe(p$age_cohort_entry, IQR = TRUE)
p_age

## join export mean + median ages + sd + IQR
mean_ages_overall <- as.data.frame((mean_ages_overall))
mean_ages_overall <- dplyr::select(mean_ages_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_age <- as.data.frame((nm_age))
nm_age <- dplyr::select(nm_age, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_age <- as.data.frame((m_age))
m_age <- dplyr::select(m_age, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_age <- as.data.frame((d_age))
d_age <- dplyr::select(d_age, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_age <- as.data.frame((p_age))
p_age <- dplyr::select(p_age, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_ages <- full_join(mean_ages_overall, nm_age,
                           by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_age, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_age, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_age, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_ages$mean_sd <- paste(all_mean_ages$mean, all_mean_ages$sd, sep =", ")
all_mean_ages$median_iqr <- paste(all_mean_ages$median, all_mean_ages$IQR, sep =", ")
all_mean_ages <- dplyr::select(all_mean_ages, c(group, mean_sd, median_iqr))
write_csv(all_mean_ages, "filepath")

## Age category - No individuals and p years per group 
exact_match_reproductive_cohort_final_4to1_agecat <- exact_match_reproductive_cohort_final_4to1_age %>%
  mutate(agecat_cohort_entry = age_cohort_entry)
exact_match_reproductive_cohort_final_4to1_agecat <- exact_match_reproductive_cohort_final_4to1_agecat %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 15, 19), 1)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 20, 24), 2)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 25, 29), 3)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 30, 34), 4)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 35, 39), 5)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 40, 44), 6)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 45, 49), 7)) 
exact_match_reproductive_cohort_final_4to1_agecat$agecat_cohort_entry <- factor(exact_match_reproductive_cohort_final_4to1_agecat$agecat_cohort_entry, levels = c(1,2,3,4,5,6,7),
                                                               labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

agecat <- exact_match_reproductive_cohort_final_4to1_agecat %>% 
  group_by(agecat_cohort_entry) %>% count() 
agecat <- agecat %>% mutate(percent = (n/sum(agecat$n))*100) %>%
  mutate(group = "Whole Cohort")
agecat_mvnm <- exact_match_reproductive_cohort_final_4to1_agecat %>% 
  group_by(agecat_cohort_entry, migrant_status) %>% count() %>% 
  rename(group = migrant_status)
agecat_m <- agecat_mvnm %>% filter(group == "Migrant")
agecat_m <- agecat_m %>% mutate(percent = (n/sum(agecat_m$n))*100)
agecat_nm <- agecat_mvnm %>% filter(group == "Non-migrant")
agecat_nm <- agecat_nm %>% mutate(percent = (n/sum(agecat_nm$n))*100)
agecat_dp <- exact_match_reproductive_cohort_final_4to1_agecat %>% 
  group_by(agecat_cohort_entry, migcertainty) %>% count() %>% 
  rename(group = migcertainty)
agecat_d <- agecat_dp %>% filter(group == "Definite")
agecat_d <- agecat_d %>% mutate(percent = (n/sum(agecat_d$n))*100)
agecat_p <- agecat_dp %>% filter(group == "Probable")
agecat_p <- agecat_p %>% mutate(percent = (n/sum(agecat_p$n))*100)
age_cat_all <- agecat %>% full_join(agecat_m,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                                    "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(agecat_nm,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                             "group"="group","n"="n", "percent" = "percent")) %>%
  full_join(agecat_d,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                            "group"="group","n"="n", "percent" = "percent")) %>%
  full_join(agecat_p,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                            "group"="group","n"="n", "percent" = "percent")) %>%
  rename(agecat = agecat_cohort_entry) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
age_cat_all <- age_cat_all %>% relocate(group) %>% arrange(group)
age_cat_all$n_percent <- paste(age_cat_all$n, age_cat_all$percent, sep =",")

pyears_agecat <- aggregate(pyears ~ agecat_cohort_entry, exact_match_reproductive_cohort_final_4to1_agecat, sum) %>% 
  mutate(group = "Whole Cohort")
pyears_agecat_mvnm <- aggregate(pyears ~ agecat_cohort_entry + migrant_status, exact_match_reproductive_cohort_final_4to1_agecat, sum) %>% 
  rename(group = migrant_status)
pyears_agecat_dp <- aggregate(pyears ~ agecat_cohort_entry + migcertainty, exact_match_reproductive_cohort_final_4to1_agecat, sum) %>% 
  rename(group = migcertainty)
pyears_agecat_all <- pyears_agecat_mvnm %>% full_join(pyears_agecat_dp,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                                                              "group"="group","pyears"="pyears")) %>% 
  full_join(pyears_agecat,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                 "group"="group","pyears"="pyears")) %>%
  rename(agecat = agecat_cohort_entry) 

pyears_agecat_export <- full_join(pyears_agecat_all, age_cat_all, 
                                  by = c("agecat"="agecat","group"="group")) %>% 
  mutate(across(is.numeric, ~ round(.,0))) %>% 
  dplyr::select(c(group, agecat, n_percent, pyears)) 

write_csv(pyears_agecat_export, "filepath")


## Ethnicity ---------------------------------------------------------------------------------


exact_match_reproductive_cohort_final_4to1$ethnicat6 <- exact_match_reproductive_cohort_final_4to1$ethnicat6 %>% fct_explicit_na(na_level = "Not known")
levels(exact_match_reproductive_cohort_final_4to1$ethnicat6)

ethnicat6 <- exact_match_reproductive_cohort_final_4to1 %>% 
  group_by(ethnicat6) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
ethnicat6_mvnm <- exact_match_reproductive_cohort_final_4to1 %>% 
  group_by(ethnicat6, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(ethnicat6, group, n, percent))
ethnicat6_dp <- exact_match_reproductive_cohort_final_4to1 %>% 
  group_by(ethnicat6, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(ethnicat6, group, n, percent))


ethnicat6_all <- ethnicat6_mvnm %>% full_join(ethnicat6_dp,by = c("ethnicat6"="ethnicat6",
                                                                  "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(ethnicat6,by = c("ethnicat6"="ethnicat6",
                             "group"="group","n"="n", "percent" = "percent")) %>%
  rename(ethnicat6 = ethnicat6)
ethnicat6_all$n_percent <- paste(ethnicat6_all$n, ethnicat6_all$percent, sep =",")
ethnicat6_all <- ethnicat6_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, ethnicat6, n_percent)

pyears_ethnicat6 <- exact_match_reproductive_cohort_final_4to1 %>% group_by(ethnicat6) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_ethnicat6_mvnm <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migrant_status, ethnicat6) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_ethnicat6_dp <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migcertainty, ethnicat6) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_ethnicat6_all <- pyears_ethnicat6_mvnm %>% full_join(pyears_ethnicat6_dp,by = c("ethnicat6"="ethnicat6",
                                                                                       "group"="group","n"="n")) %>% 
  full_join(pyears_ethnicat6,by = c("ethnicat6"="ethnicat6",
                                    "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_ethnicat6_export <- full_join(pyears_ethnicat6_all, ethnicat6_all, 
                                     by = c("ethnicat6"="ethnicat6","group"="group")) %>% 
  relocate(group,ethnicat6,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_ethnicat6_export, "filepath")


## practice region --------------------------------

exact_match_reproductive_cohort_final_4to1$prac_region <- exact_match_reproductive_cohort_final_4to1$prac_region %>% fct_explicit_na(na_level = "Not known")
levels(exact_match_reproductive_cohort_final_4to1$prac_region)

prac_region <- exact_match_reproductive_cohort_final_4to1 %>% 
  group_by(prac_region) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
prac_region_mvnm <- exact_match_reproductive_cohort_final_4to1 %>% 
  group_by(prac_region, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(prac_region, group, n, percent))
prac_region_dp <- exact_match_reproductive_cohort_final_4to1 %>% 
  group_by(prac_region, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(prac_region, group, n, percent))


prac_region_all <- prac_region_mvnm %>% full_join(prac_region_dp,by = c("prac_region"="prac_region",
                                                                        "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(prac_region,by = c("prac_region"="prac_region",
                               "group"="group","n"="n", "percent" = "percent")) %>%
  rename(prac_region = prac_region)
prac_region_all$n_percent <- paste(prac_region_all$n, prac_region_all$percent, sep =",")
prac_region_all <- prac_region_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, prac_region, n_percent)



pyears_prac_region <- exact_match_reproductive_cohort_final_4to1 %>% group_by(prac_region) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_prac_region_mvnm <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migrant_status, prac_region) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_prac_region_dp <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migcertainty, prac_region) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_prac_region_all <- pyears_prac_region_mvnm %>% full_join(pyears_prac_region_dp,by = c("prac_region"="prac_region",
                                                                                             "group"="group","n"="n")) %>% 
  full_join(pyears_prac_region,by = c("prac_region"="prac_region",
                                      "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_prac_region_export <- full_join(pyears_prac_region_all, prac_region_all, 
                                       by = c("prac_region"="prac_region","group"="group")) %>% 
  relocate(group,prac_region,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_prac_region_export, "filepath")

## IMD -------------------------------------

exact_match_reproductive_cohort_final_4to1$imd <- exact_match_reproductive_cohort_final_4to1$imd %>% fct_explicit_na(na_level = "Not known")
levels(exact_match_reproductive_cohort_final_4to1$imd)

imd<- exact_match_reproductive_cohort_final_4to1 %>% 
  group_by(imd) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
imd_mvnm <- exact_match_reproductive_cohort_final_4to1 %>% 
  group_by(imd, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(imd, group, n, percent))
imd_dp <- exact_match_reproductive_cohort_final_4to1 %>% 
  group_by(imd, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(imd, group, n, percent))

imd_all <- imd_mvnm %>% full_join(imd_dp,by = c("imd"="imd",
                                                "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(imd,by = c("imd"="imd",
                       "group"="group","n"="n", "percent" = "percent")) %>%
  rename(imd= imd)
imd_all$n_percent <- paste(imd_all$n, imd_all$percent, sep =",")
imd_all <- imd_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, imd, n_percent)

pyears_imd <- exact_match_reproductive_cohort_final_4to1 %>% group_by(imd) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_imd_mvnm <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migrant_status, imd) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_imd_dp <- exact_match_reproductive_cohort_final_4to1 %>% group_by(migcertainty, imd) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_imd_all <- pyears_imd_mvnm %>% full_join(pyears_imd_dp,by = c("imd"="imd",
                                                                     "group"="group","n"="n")) %>% 
  full_join(pyears_imd,by = c("imd"="imd",
                              "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_imd_export <- full_join(pyears_imd_all, imd_all, 
                               by = c("imd"="imd","group"="group")) %>% 
  relocate(group,imd,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_imd_export, "filepath")

## Year of cohort entry -----------------------------------------------------------

exact_match_reproductive_cohort_final_4to1_yearofentry <- exact_match_reproductive_cohort_final_4to1 %>% mutate(yearofentry = year(cohort_entry))
mean_yearofentry_overall <- describe(exact_match_reproductive_cohort_final_4to1_yearofentry$yearofentry, IQR = TRUE)
mean_yearofentry_overall

nm <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% filter(migrant_status == "Non-migrant")
nm_yearofentry <- describe(nm$yearofentry, IQR = TRUE)
nm_yearofentry

m <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% filter(migrant_status == "Migrant")
m_yearofentry <- describe(m$yearofentry, IQR = TRUE)
m_yearofentry

d <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% filter(migcertainty == "Definite")
d_yearofentry  <- describe(d$yearofentry, IQR = TRUE)
d_yearofentry 

p <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% filter(migcertainty == "Probable")
p_yearofentry <- describe(p$yearofentry, IQR = TRUE)
p_yearofentry

## join export mean yearsof entry + sds
mean_yearofentry_overall <- as.data.frame((mean_yearofentry_overall))
mean_yearofentry_overall <- dplyr::select(mean_yearofentry_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_yearofentry<- as.data.frame((nm_yearofentry))
nm_yearofentry <- dplyr::select(nm_yearofentry, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_yearofentry <- as.data.frame((m_yearofentry))
m_yearofentry <- dplyr::select(m_yearofentry, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_yearofentry <- as.data.frame((d_yearofentry))
d_yearofentry <- dplyr::select(d_yearofentry, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_yearofentry <- as.data.frame((p_yearofentry))
p_yearofentry <- dplyr::select(p_yearofentry, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_yearofentry <- full_join(mean_yearofentry_overall, nm_yearofentry,
                                  by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_yearofentry, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_yearofentry, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_yearofentry, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_yearofentry$mean_sd <- paste(all_mean_yearofentry$mean, all_mean_yearofentry$sd, sep =", ")
all_mean_yearofentry$median_iqr <- paste(all_mean_yearofentry$median, all_mean_yearofentry$IQR, sep =", ")
all_mean_yearofentry <- dplyr::select(all_mean_yearofentry, c(group, mean_sd, median_iqr))

write_csv(all_mean_yearofentry, "filepath")

exact_match_reproductive_cohort_final_4to1_yearofentry$yearofentry <- factor(exact_match_reproductive_cohort_final_4to1_yearofentry$yearofentry,  levels = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018), 
                                                            labels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
yearofentry <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% 
  group_by(yearofentry) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
yearofentry_mvnm <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% 
  group_by(yearofentry, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(yearofentry, group, n, percent))
yearofentry_dp <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% 
  group_by(yearofentry, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(yearofentry, group, n, percent))

yearofentry_all <- yearofentry_mvnm %>% full_join(yearofentry_dp,by = c("yearofentry"="yearofentry",
                                                                        "group"="group","n"="n", "percent" = "percent")) %>%
  full_join(yearofentry, by = c("yearofentry" = "yearofentry", "group" = "group", "n" = "n", "percent" = "percent"))
yearofentry_all$n_percent <- paste(yearofentry_all$n, yearofentry_all$percent, sep =",")
yearofentry_all <- yearofentry_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, yearofentry, n_percent)

pyears_yearofentry <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% group_by(yearofentry) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_yearofentry_mvnm <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% group_by(migrant_status, yearofentry) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_yearofentry_dp <- exact_match_reproductive_cohort_final_4to1_yearofentry %>% group_by(migcertainty, yearofentry) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_yearofentry_all <- pyears_yearofentry_mvnm %>% full_join(pyears_yearofentry_dp,by = c("yearofentry"="yearofentry",
                                                                                             "group"="group","n"="n")) %>% 
  full_join(pyears_yearofentry,by = c("yearofentry"="yearofentry",
                                      "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_yearofentry_export <- full_join(pyears_yearofentry_all, yearofentry_all, 
                                       by = c("group"="group","yearofentry"="yearofentry")) %>% 
  relocate(group,yearofentry,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_yearofentry_export, "filepath")

## Year ----------------------------------------------------------------------------

eventyear<- exact_match_srh_annual_counts_final_4to1 %>% 
  group_by(eventyear) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
eventyear_mvnm <- exact_match_srh_annual_counts_final_4to1 %>% 
  group_by(eventyear, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(eventyear, group, n, percent))
eventyear_dp <- exact_match_srh_annual_counts_final_4to1 %>% 
  group_by(eventyear, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(eventyear, group, n, percent))

eventyear_all <- eventyear_mvnm %>% full_join(eventyear_dp,by = c("eventyear"="eventyear",
                                                                  "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(eventyear,by = c("eventyear"="eventyear",
                             "group"="group","n"="n", "percent" = "percent")) %>%
  rename(eventyear= eventyear)
eventyear_all$n_percent <- paste(eventyear_all$n, eventyear_all$percent, sep =",")
eventyear_all <- eventyear_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, eventyear, n_percent)

pyears_eventyear <- exact_match_srh_annual_counts_final_4to1 %>% group_by(eventyear) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_eventyear_mvnm <- exact_match_srh_annual_counts_final_4to1 %>% group_by(migrant_status, eventyear) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_eventyear_dp <- exact_match_srh_annual_counts_final_4to1 %>% group_by(migcertainty, eventyear) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_eventyear_all <- pyears_eventyear_mvnm %>% full_join(pyears_eventyear_dp,by = c("eventyear"="eventyear",
                                                                                       "group"="group","n"="n")) %>% 
  full_join(pyears_eventyear,by = c("eventyear"="eventyear",
                                    "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_eventyear_export <- full_join(pyears_eventyear_all, eventyear_all, 
                                     by = c("eventyear"="eventyear","group"="group")) %>% 
  relocate(group,eventyear,n_percent,pyears)  %>% 
  arrange(group)

write_csv(pyears_eventyear_export, "filepath")

## bar chart of proportions

eventyear_all <- eventyear_mvnm %>% full_join(eventyear_dp,by = c("eventyear"="eventyear",
                                                                  "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(eventyear,by = c("eventyear"="eventyear",
                             "group"="group","n"="n", "percent" = "percent"))
eventyear_all$group <- factor(eventyear_all$group, levels = c("Whole Cohort", "Non-migrant", "Migrant", "Definite", "Probable"))
eventyear_all$group <- recode(eventyear_all$group, "Whole Cohort" = "Whole Cohort", "Non-migrant" = "Non-migrants", "Migrant" = "Migrants", 
                              "Definite" = "Definite Migrants", "Probable" = "Probable Migrants")
eventyear_all_dropdp <- eventyear_all %>% filter(group == "Whole Cohort" | group == "Non-migrants" |group == "Migrants")
eventyear_all_dropdp$group <- droplevels(eventyear_all_dropdp$group)
levels(eventyear_all_dropdp$group)
year_line_point <- ggplot(eventyear_all_dropdp, aes(group = group, x = eventyear, y = percent, colour = group)) + 
  geom_line() + 
  geom_point() +
  ggtitle(str_wrap("Percentage of cohort contributing per year of study period stratified by migration status", 60)) +
  labs(y="Percentage of cohort", x = "Year of study",  color = "Key")
print(year_line_point)
ggsave("filepath", width = 7, height = 5, dpi = 300)

## Person years --------------------------------

mean_pyears_overall <- describe(exact_match_reproductive_cohort_final_4to1$pyears, IQR = TRUE)
mean_pyears_overall

nm <- exact_match_reproductive_cohort_final_4to1 %>% filter(migrant_status == "Non-migrant")
nm_pyears <- describe(nm$pyears, IQR = TRUE)
nm_pyears

m <- exact_match_reproductive_cohort_final_4to1 %>% filter(migrant_status == "Migrant")
m_pyears <- describe(m$pyears, IQR = TRUE)
m_pyears

d <- exact_match_reproductive_cohort_final_4to1 %>% filter(migcertainty == "Definite")
d_pyears <- describe(d$pyears, IQR = TRUE)
d_pyears

p <- exact_match_reproductive_cohort_final_4to1 %>% filter(migcertainty == "Probable")
p_pyears <- describe(p$pyears, IQR = TRUE)
p_pyears

## join export mean + median ages + sd + IQR
mean_pyears_overall <- as.data.frame((mean_pyears_overall))
mean_pyears_overall <- dplyr::select(mean_pyears_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_pyears <- as.data.frame((nm_pyears))
nm_pyears <- dplyr::select(nm_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_pyears <- as.data.frame((m_pyears))
m_pyears <- dplyr::select(m_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_pyears <- as.data.frame((d_pyears))
d_pyears <- dplyr::select(d_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_pyears <- as.data.frame((p_pyears))
p_pyears <- dplyr::select(p_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_pyears <- full_join(mean_pyears_overall, nm_pyears,
                             by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_pyears, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_pyears, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_pyears, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_pyears$mean_sd <- paste(all_mean_pyears$mean, all_mean_pyears$sd, sep =", ")
all_mean_pyears$median_iqr <- paste(all_mean_pyears$median, all_mean_pyears$IQR, sep =", ")
all_mean_pyears <- dplyr::select(all_mean_pyears, c(group, mean_sd, median_iqr))
write_csv(all_mean_pyears, "filepath")
