####---- Description -------------------------------------------------------------------------

## SDI Analysis for Neha's contraception prescribing chapter in females of reproductive age (15-49yo) 
## Date started: 30/09/2020
## Author: Neha Pathak

## Load packages -------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(psych)
library(MASS)
library(epitools)
library(forestplot)
library(Hmisc)
library(epiR)
library(data.table)

## Set working directory ------------------------------------------------------------------

setwd("filepath")

## Import data -----------------------------------------------------------------------

load(file = "filepath") 
load(file = "filepath") 
load(file = "filepath") 

sdipresc_annual_counts_final$prac_region <- relevel(sdipresc_annual_counts_final$prac_region, "London")
sdipresc_annual_counts_final_extra$prac_region <- relevel(sdipresc_annual_counts_final_extra$prac_region, "London")


## BASELINE CHARACTERISTICS - MAIN ANALYSIS COHORT ----------------------------------------------------------------------------------


## change cohort final to be based on preg max assumption
# drop records with 0 or less pdays_ey_pregmax in counts
sdipresc_annual_counts_final_extra_preg_max <- sdipresc_annual_counts_final_extra %>% filter(pdays_ey_pregmax > 0) 
# recalculate pdays total based on counts data and join
sdipresc_annual_counts_final_extra_preg_max_pdays <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(patid) %>% tally(pdays_ey_pregmax) %>% rename(sdi_pdays_max = n )
sdipresc_annual_counts_final_extra_preg_max_pyears <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(patid) %>% tally(pyears_ey_pregmax) %>% rename(sdi_pyears_max = n )
sdipresc_annual_counts_final_extra_preg_max_days_pyears <- full_join(sdipresc_annual_counts_final_extra_preg_max_pdays, sdipresc_annual_counts_final_extra_preg_max_pyears, by = c("patid" = "patid"))
# select patients in cohort file based on preg max assumption and add on recalculated pdays/pyears
sdi_cohort_final_pregmax <- sdi_cohort_final %>% filter(patid %in% sdipresc_annual_counts_final_extra_preg_max$patid) %>% dplyr::select(-sdi_pdays_max, -sdi_pdays_min, -sdi_pyears_max, -sdi_pyears_min)
sdi_cohort_final_pregmax <- left_join(sdi_cohort_final_pregmax, sdipresc_annual_counts_final_extra_preg_max_days_pyears, by = c("patid" = "patid"))

## Totals ---

## Whole cohort 
total_cohort_n <- count(sdi_cohort_final_pregmax)
total_sdi_pyears_max <- sum(sdi_cohort_final_pregmax$sdi_pyears_max)
total_wholecohort <- bind_cols(total_cohort_n, total_sdi_pyears_max)
colnames(total_wholecohort) <- c("individuals", "sdi_pyears_max")
total_wholecohort <- total_wholecohort %>% mutate(group = "Whole cohort") 

## By migrant status
total_mvnm_n <- sdi_cohort_final_pregmax %>% group_by(migrant_status) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migrant_status)
total_mvnm_sdi_pyears_max <- sdi_cohort_final_pregmax %>% group_by(migrant_status) %>%
  tally(sdi_pyears_max) %>%
  rename(sdi_pyears_max = n) %>% 
  rename(group = migrant_status)
total_mvnm <- full_join(total_mvnm_n, total_mvnm_sdi_pyears_max,
                        by = c("group" = "group"))

## By migrant certainty
total_dp_n <- sdi_cohort_final_pregmax %>% group_by(migcertainty) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migcertainty)
total_dp_sdi_pyears_max <- sdi_cohort_final_pregmax %>% group_by(migcertainty) %>%
  tally(sdi_pyears_max) %>%
  rename(sdi_pyears_max = n) %>% 
  rename(group = migcertainty)
total_dp <- full_join(total_dp_n, total_dp_sdi_pyears_max,
                      by = c("group" = "group"))

## Totals for export
totals <- full_join(total_mvnm, total_dp,
                    by = c("group" = "group", "individuals" = "individuals", "sdi_pyears_max" = "sdi_pyears_max")) %>% 
  full_join(total_wholecohort, by = c("group" = "group", "individuals" = "individuals", "sdi_pyears_max" = "sdi_pyears_max")) %>% 
  mutate(across(is.numeric, ~ round(.,0)))
total_cohort_n<- as.vector(total_cohort_n$n[1])
totals_percent <- totals %>% mutate(percent = (individuals/total_cohort_n)*100) %>% 
  mutate(percent = round(percent, 1))

totals_percent$individuals <- paste(totals_percent$individuals, totals_percent$percent, sep =", ")
totals_percent <- dplyr::select(totals_percent, c(group, individuals, sdi_pyears_max))
write_csv(totals_percent, "filepath")


## Age ---

## Mean + Median + SD + IQR Age at cohort entry 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

sdi_cohort_final_pregmax_age <- sdi_cohort_final_pregmax %>%
  mutate(age_cohort_entry = calc_age(dob, sdi_cohort_entry))
mean_ages_overall <- psych::describe(sdi_cohort_final_pregmax_age$age_cohort_entry, IQR = TRUE)
mean_ages_overall

nm <- sdi_cohort_final_pregmax_age %>% filter(migrant_status == "Non-migrant")
nm_age <- psych::describe(nm$age_cohort_entry, IQR = TRUE)
nm_age

m <- sdi_cohort_final_pregmax_age %>% filter(migrant_status == "Migrant")
m_age <- psych::describe(m$age_cohort_entry, IQR = TRUE)
m_age

d <- sdi_cohort_final_pregmax_age %>% filter(migcertainty == "Definite")
d_age <- psych::describe(d$age_cohort_entry, IQR = TRUE)
d_age

p <- sdi_cohort_final_pregmax_age %>% filter(migcertainty == "Probable")
p_age <- psych::describe(p$age_cohort_entry, IQR = TRUE)
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
sdi_cohort_final_pregmax_agecat <- sdi_cohort_final_pregmax_age %>%
  mutate(agecat_cohort_entry = age_cohort_entry)
sdi_cohort_final_pregmax_agecat <- sdi_cohort_final_pregmax_agecat %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 15, 19), 1)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 20, 24), 2)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 25, 29), 3)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 30, 34), 4)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 35, 39), 5)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 40, 44), 6)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 45, 49), 7)) 
sdi_cohort_final_pregmax_agecat$agecat_cohort_entry <- factor(sdi_cohort_final_pregmax_agecat$agecat_cohort_entry, levels = c(1,2,3,4,5,6,7),
                                                              labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

agecat <- sdi_cohort_final_pregmax_agecat %>% 
  group_by(agecat_cohort_entry) %>% count() 
agecat <- agecat %>% mutate(percent = (n/sum(agecat$n))*100) %>%
  mutate(group = "Whole Cohort")
agecat_mvnm <- sdi_cohort_final_pregmax_agecat %>% 
  group_by(agecat_cohort_entry, migrant_status) %>% count() %>% 
  rename(group = migrant_status)
agecat_m <- agecat_mvnm %>% filter(group == "Migrant")
agecat_m <- agecat_m %>% mutate(percent = (n/sum(agecat_m$n))*100)
agecat_nm <- agecat_mvnm %>% filter(group == "Non-migrant")
agecat_nm <- agecat_nm %>% mutate(percent = (n/sum(agecat_nm$n))*100)
agecat_dp <- sdi_cohort_final_pregmax_agecat %>% 
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

sdi_pyears_max_agecat <- aggregate(sdi_pyears_max ~ agecat_cohort_entry, sdi_cohort_final_pregmax_agecat, sum) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_agecat_mvnm <- aggregate(sdi_pyears_max ~ agecat_cohort_entry + migrant_status, sdi_cohort_final_pregmax_agecat, sum) %>% 
  rename(group = migrant_status)
sdi_pyears_max_agecat_dp <- aggregate(sdi_pyears_max ~ agecat_cohort_entry + migcertainty, sdi_cohort_final_pregmax_agecat, sum) %>% 
  rename(group = migcertainty)
sdi_pyears_max_agecat_all <- sdi_pyears_max_agecat_mvnm %>% full_join(sdi_pyears_max_agecat_dp,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                                                                                      "group"="group","sdi_pyears_max"="sdi_pyears_max")) %>% 
  full_join(sdi_pyears_max_agecat,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                         "group"="group","sdi_pyears_max"="sdi_pyears_max")) %>%
  rename(agecat = agecat_cohort_entry) 

sdi_pyears_max_agecat_export <- full_join(sdi_pyears_max_agecat_all, age_cat_all, 
                                          by = c("agecat"="agecat","group"="group")) %>% 
  mutate(across(is.numeric, ~ round(.,0))) %>% 
  dplyr::select(c(group, agecat, n_percent, sdi_pyears_max)) 

write_csv(sdi_pyears_max_agecat_export, "filepath")


## Ethnicity ---


sdi_cohort_final_pregmax$ethnicat6 <- sdi_cohort_final_pregmax$ethnicat6 %>% fct_explicit_na(na_level = "Not known")
levels(sdi_cohort_final_pregmax$ethnicat6)

ethnicat6 <- sdi_cohort_final_pregmax %>% 
  group_by(ethnicat6) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
ethnicat6_mvnm <- sdi_cohort_final_pregmax %>% 
  group_by(ethnicat6, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(ethnicat6, group, n, percent))
ethnicat6_dp <- sdi_cohort_final_pregmax %>% 
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



sdi_pyears_max_ethnicat6 <- sdi_cohort_final_pregmax %>% group_by(ethnicat6) %>% tally(sdi_pyears_max) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_ethnicat6_mvnm <- sdi_cohort_final_pregmax %>% group_by(migrant_status, ethnicat6) %>% tally(sdi_pyears_max) %>% 
  rename(group = migrant_status)
sdi_pyears_max_ethnicat6_dp <- sdi_cohort_final_pregmax %>% group_by(migcertainty, ethnicat6) %>% tally(sdi_pyears_max) %>% 
  rename(group = migcertainty)
sdi_pyears_max_ethnicat6_all <- sdi_pyears_max_ethnicat6_mvnm %>% full_join(sdi_pyears_max_ethnicat6_dp,by = c("ethnicat6"="ethnicat6",
                                                                                                               "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_max_ethnicat6,by = c("ethnicat6"="ethnicat6",
                                            "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_max_ethnicat6_export <- full_join(sdi_pyears_max_ethnicat6_all, ethnicat6_all, 
                                             by = c("ethnicat6"="ethnicat6","group"="group")) %>% 
  relocate(group,ethnicat6,n_percent,sdi_pyears_max)  %>% 
  arrange(group)

write_csv(sdi_pyears_max_ethnicat6_export, "filepath")


## practice region ---

sdi_cohort_final_pregmax$prac_region <- sdi_cohort_final_pregmax$prac_region %>% fct_explicit_na(na_level = "Not known")
levels(sdi_cohort_final_pregmax$prac_region)

prac_region <- sdi_cohort_final_pregmax %>% 
  group_by(prac_region) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
prac_region_mvnm <- sdi_cohort_final_pregmax %>% 
  group_by(prac_region, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(prac_region, group, n, percent))
prac_region_dp <- sdi_cohort_final_pregmax %>% 
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



sdi_pyears_max_prac_region <- sdi_cohort_final_pregmax %>% group_by(prac_region) %>% tally(sdi_pyears_max) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_prac_region_mvnm <- sdi_cohort_final_pregmax %>% group_by(migrant_status, prac_region) %>% tally(sdi_pyears_max) %>% 
  rename(group = migrant_status)
sdi_pyears_max_prac_region_dp <- sdi_cohort_final_pregmax %>% group_by(migcertainty, prac_region) %>% tally(sdi_pyears_max) %>% 
  rename(group = migcertainty)
sdi_pyears_max_prac_region_all <- sdi_pyears_max_prac_region_mvnm %>% full_join(sdi_pyears_max_prac_region_dp,by = c("prac_region"="prac_region",
                                                                                                                     "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_max_prac_region,by = c("prac_region"="prac_region",
                                              "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_max_prac_region_export <- full_join(sdi_pyears_max_prac_region_all, prac_region_all, 
                                               by = c("prac_region"="prac_region","group"="group")) %>% 
  relocate(group,prac_region,n_percent,sdi_pyears_max)  %>% 
  arrange(group)

write_csv(sdi_pyears_max_prac_region_export, "filepath")


## IMD ---

sdi_cohort_final_pregmax$imd<- sdi_cohort_final_pregmax$imd%>% fct_explicit_na(na_level = "Not known")
levels(sdi_cohort_final_pregmax$imd)

imd<- sdi_cohort_final_pregmax %>% 
  group_by(imd) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
imd_mvnm <- sdi_cohort_final_pregmax %>% 
  group_by(imd, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(imd, group, n, percent))
imd_dp <- sdi_cohort_final_pregmax %>% 
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

sdi_pyears_max_imd <- sdi_cohort_final_pregmax %>% group_by(imd) %>% tally(sdi_pyears_max) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_imd_mvnm <- sdi_cohort_final_pregmax %>% group_by(migrant_status, imd) %>% tally(sdi_pyears_max) %>% 
  rename(group = migrant_status)
sdi_pyears_max_imd_dp <- sdi_cohort_final_pregmax %>% group_by(migcertainty, imd) %>% tally(sdi_pyears_max) %>% 
  rename(group = migcertainty)
sdi_pyears_max_imd_all <- sdi_pyears_max_imd_mvnm %>% full_join(sdi_pyears_max_imd_dp,by = c("imd"="imd",
                                                                                             "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_max_imd,by = c("imd"="imd",
                                      "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_max_imd_export <- full_join(sdi_pyears_max_imd_all, imd_all, 
                                       by = c("imd"="imd","group"="group")) %>% 
  relocate(group,imd,n_percent,sdi_pyears_max)  %>% 
  arrange(group)

write_csv(sdi_pyears_max_imd_export, "filepath")



## Year of cohort entry ---

sdi_cohort_final_pregmax_yearofentry <- sdi_cohort_final_pregmax %>% mutate(yearofentry = year(sdi_cohort_entry))
mean_yearofentry_overall <- psych::describe(sdi_cohort_final_pregmax_yearofentry$yearofentry, IQR = TRUE)
mean_yearofentry_overall

nm <- sdi_cohort_final_pregmax_yearofentry %>% filter(migrant_status == "Non-migrant")
nm_yearofentry <- psych::describe(nm$yearofentry, IQR = TRUE)
nm_yearofentry

m <- sdi_cohort_final_pregmax_yearofentry %>% filter(migrant_status == "Migrant")
m_yearofentry <- psych::describe(m$yearofentry, IQR = TRUE)
m_yearofentry

d <- sdi_cohort_final_pregmax_yearofentry %>% filter(migcertainty == "Definite")
d_yearofentry  <- psych::describe(d$yearofentry, IQR = TRUE)
d_yearofentry 

p <- sdi_cohort_final_pregmax_yearofentry %>% filter(migcertainty == "Probable")
p_yearofentry <- psych::describe(p$yearofentry, IQR = TRUE)
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
write_csv(all_mean_yearofentry, "filepath" )


sdi_cohort_final_pregmax_yearofentry$yearofentry <- factor(sdi_cohort_final_pregmax_yearofentry$yearofentry,  levels = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018), 
                                                           labels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
yearofentry <- sdi_cohort_final_pregmax_yearofentry %>% 
  group_by(yearofentry) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
yearofentry_mvnm <- sdi_cohort_final_pregmax_yearofentry %>% 
  group_by(yearofentry, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(yearofentry, group, n, percent))
yearofentry_dp <- sdi_cohort_final_pregmax_yearofentry %>% 
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

sdi_pyears_max_yearofentry <- sdi_cohort_final_pregmax_yearofentry %>% group_by(yearofentry) %>% tally(sdi_pyears_max) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_yearofentry_mvnm <- sdi_cohort_final_pregmax_yearofentry %>% group_by(migrant_status, yearofentry) %>% tally(sdi_pyears_max) %>% 
  rename(group = migrant_status)
sdi_pyears_max_yearofentry_dp <- sdi_cohort_final_pregmax_yearofentry %>% group_by(migcertainty, yearofentry) %>% tally(sdi_pyears_max) %>% 
  rename(group = migcertainty)
sdi_pyears_max_yearofentry_all <- sdi_pyears_max_yearofentry_mvnm %>% full_join(sdi_pyears_max_yearofentry_dp,by = c("yearofentry"="yearofentry",
                                                                                                                     "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_max_yearofentry,by = c("yearofentry"="yearofentry",
                                              "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_max_yearofentry_export <- full_join(sdi_pyears_max_yearofentry_all, yearofentry_all, 
                                               by = c("group"="group","yearofentry"="yearofentry")) %>% 
  relocate(group,yearofentry,n_percent,sdi_pyears_max)  %>% 
  arrange(group)

write_csv(sdi_pyears_max_yearofentry_export, "filepath")


## Year ---

eventyear<- sdipresc_annual_counts_final_extra_preg_max %>% 
  group_by(eventyear) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
eventyear_mvnm <- sdipresc_annual_counts_final_extra_preg_max  %>% 
  group_by(eventyear, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(eventyear, group, n, percent))
eventyear_dp <- sdipresc_annual_counts_final_extra_preg_max  %>% 
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

pyears_eventyear <- sdipresc_annual_counts_final_extra_preg_max  %>% group_by(eventyear) %>% tally(pyears_ey_pregmax) %>% 
  mutate(group = "Whole Cohort")
pyears_eventyear_mvnm <- sdipresc_annual_counts_final_extra_preg_max  %>% group_by(migrant_status, eventyear) %>% tally(pyears_ey_pregmax) %>% 
  rename(group = migrant_status)
pyears_eventyear_dp <- sdipresc_annual_counts_final_extra_preg_max  %>% group_by(migcertainty, eventyear) %>% tally(pyears_ey_pregmax) %>% 
  rename(group = migcertainty)
pyears_eventyear_all <- pyears_eventyear_mvnm %>% full_join(pyears_eventyear_dp,by = c("eventyear"="eventyear",
                                                                                       "group"="group","n"="n")) %>% 
  full_join(pyears_eventyear,by = c("eventyear"="eventyear",
                                    "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_eventyear_export <- full_join(pyears_eventyear_all, eventyear_all, 
                                     by = c("eventyear"="eventyear","group"="group")) %>% 
  relocate(group,eventyear,n_percent,sdi_pyears_max)  %>% 
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



## Person years ---

mean_sdi_pyears_max_overall <- psych::describe(sdi_cohort_final_pregmax$sdi_pyears_max, IQR = TRUE)
mean_sdi_pyears_max_overall

nm <- sdi_cohort_final_pregmax %>% filter(migrant_status == "Non-migrant")
nm_sdi_pyears_max <- psych::describe(nm$sdi_pyears_max, IQR = TRUE)
nm_sdi_pyears_max

m <- sdi_cohort_final_pregmax %>% filter(migrant_status == "Migrant")
m_sdi_pyears_max <- psych::describe(m$sdi_pyears_max, IQR = TRUE)
m_sdi_pyears_max

d <- sdi_cohort_final_pregmax %>% filter(migcertainty == "Definite")
d_sdi_pyears_max <- psych::describe(d$sdi_pyears_max, IQR = TRUE)
d_sdi_pyears_max

p <- sdi_cohort_final_pregmax %>% filter(migcertainty == "Probable")
p_sdi_pyears_max <- psych::describe(p$sdi_pyears_max, IQR = TRUE)
p_sdi_pyears_max

## join export mean + median ages + sd + IQR
mean_sdi_pyears_max_overall <- as.data.frame((mean_sdi_pyears_max_overall))
mean_sdi_pyears_max_overall <- dplyr::select(mean_sdi_pyears_max_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_sdi_pyears_max <- as.data.frame((nm_sdi_pyears_max))
nm_sdi_pyears_max <- dplyr::select(nm_sdi_pyears_max, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_sdi_pyears_max <- as.data.frame((m_sdi_pyears_max))
m_sdi_pyears_max <- dplyr::select(m_sdi_pyears_max, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_sdi_pyears_max <- as.data.frame((d_sdi_pyears_max))
d_sdi_pyears_max <- dplyr::select(d_sdi_pyears_max, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_sdi_pyears_max <- as.data.frame((p_sdi_pyears_max))
p_sdi_pyears_max <- dplyr::select(p_sdi_pyears_max, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_sdi_pyears_max <- full_join(mean_sdi_pyears_max_overall, nm_sdi_pyears_max,
                                     by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_sdi_pyears_max, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_sdi_pyears_max, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_sdi_pyears_max, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_sdi_pyears_max$mean_sd <- paste(all_mean_sdi_pyears_max$mean, all_mean_sdi_pyears_max$sd, sep =", ")
all_mean_sdi_pyears_max$median_iqr <- paste(all_mean_sdi_pyears_max$median, all_mean_sdi_pyears_max$IQR, sep =", ")
all_mean_sdi_pyears_max <- dplyr::select(all_mean_sdi_pyears_max, c(group, mean_sd, median_iqr))
write_csv(all_mean_sdi_pyears_max, "filepath")



## BASELINE CHARACTERISTICS - EXACT MATCHED COHORT 4:1 WITH 365D WASHOUT--------------------------


# Load final datasets for analysis .Rdata files 

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")


## change cohort final to be based on preg max assumption
# drop records with 0 or less pdays_ey_pregmax in counts
exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max <- exact_match_sdipresc_annual_counts_final_extra_4to1 %>% filter(pdays_ey_pregmax > 0) 
# recalculate pdays total based on counts data and join
exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max_pdays <- exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max %>% group_by(patid) %>% tally(pdays_ey_pregmax) %>% rename(sdi_pdays_max = n )
exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max_pyears <- exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max %>% group_by(patid) %>% tally(pyears_ey_pregmax) %>% rename(sdi_pyears_max = n )
exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max_days_pyears <- full_join(exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max_pdays, exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max_pyears, by = c("patid" = "patid"))
# select patients in cohort file based on preg max assumption and add on recalculated pdays/pyears
exact_match_sdi_cohort_final_4to1_pregmax <- exact_match_sdi_cohort_final_4to1 %>% filter(patid %in% exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max$patid) %>% dplyr::select(-sdi_pdays_max, -sdi_pdays_min, -sdi_pyears_max, -sdi_pyears_min)
exact_match_sdi_cohort_final_4to1_pregmax <- left_join(exact_match_sdi_cohort_final_4to1_pregmax, exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max_days_pyears, by = c("patid" = "patid"))

## Totals ---

## Whole cohort 
total_cohort_n <- count(exact_match_sdi_cohort_final_4to1_pregmax)
total_sdi_pyears_max <- sum(exact_match_sdi_cohort_final_4to1_pregmax$sdi_pyears_max)
total_wholecohort <- bind_cols(total_cohort_n, total_sdi_pyears_max)
colnames(total_wholecohort) <- c("individuals", "sdi_pyears_max")
total_wholecohort <- total_wholecohort %>% mutate(group = "Whole cohort") 

## By migrant status
total_mvnm_n <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migrant_status) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migrant_status)
total_mvnm_sdi_pyears_max <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migrant_status) %>%
  tally(sdi_pyears_max) %>%
  rename(sdi_pyears_max = n) %>% 
  rename(group = migrant_status)
total_mvnm <- full_join(total_mvnm_n, total_mvnm_sdi_pyears_max,
                        by = c("group" = "group"))

## By migrant certainty
total_dp_n <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migcertainty) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migcertainty)
total_dp_sdi_pyears_max <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migcertainty) %>%
  tally(sdi_pyears_max) %>%
  rename(sdi_pyears_max = n) %>% 
  rename(group = migcertainty)
total_dp <- full_join(total_dp_n, total_dp_sdi_pyears_max,
                      by = c("group" = "group"))

## Totals for export
totals <- full_join(total_mvnm, total_dp,
                    by = c("group" = "group", "individuals" = "individuals", "sdi_pyears_max" = "sdi_pyears_max")) %>% 
  full_join(total_wholecohort, by = c("group" = "group", "individuals" = "individuals", "sdi_pyears_max" = "sdi_pyears_max")) %>% 
  mutate(across(is.numeric, ~ round(.,0)))
total_cohort_n<- as.vector(total_cohort_n$n[1])
totals_percent <- totals %>% mutate(percent = (individuals/total_cohort_n)*100) %>% 
  mutate(percent = round(percent, 1))

totals_percent$individuals <- paste(totals_percent$individuals, totals_percent$percent, sep =", ")
totals_percent <- dplyr::select(totals_percent, c(group, individuals, sdi_pyears_max))
write_csv(totals_percent, "filepath" )


## Age ---

## Mean + Median + SD + IQR Age at cohort entry 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

exact_match_sdi_cohort_final_4to1_pregmax_age <- exact_match_sdi_cohort_final_4to1_pregmax %>%
  mutate(age_cohort_entry = calc_age(dob, sdi_cohort_entry))
mean_ages_overall <- psych::describe(exact_match_sdi_cohort_final_4to1_pregmax_age$age_cohort_entry, IQR = TRUE)
mean_ages_overall

nm <- exact_match_sdi_cohort_final_4to1_pregmax_age %>% filter(migrant_status == "Non-migrant")
nm_age <- psych::describe(nm$age_cohort_entry, IQR = TRUE)
nm_age

m <- exact_match_sdi_cohort_final_4to1_pregmax_age %>% filter(migrant_status == "Migrant")
m_age <- psych::describe(m$age_cohort_entry, IQR = TRUE)
m_age

d <- exact_match_sdi_cohort_final_4to1_pregmax_age %>% filter(migcertainty == "Definite")
d_age <- psych::describe(d$age_cohort_entry, IQR = TRUE)
d_age

p <- exact_match_sdi_cohort_final_4to1_pregmax_age %>% filter(migcertainty == "Probable")
p_age <- psych::describe(p$age_cohort_entry, IQR = TRUE)
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
exact_match_sdi_cohort_final_4to1_pregmax_agecat <- exact_match_sdi_cohort_final_4to1_pregmax_age %>%
  mutate(agecat_cohort_entry = age_cohort_entry)
exact_match_sdi_cohort_final_4to1_pregmax_agecat <- exact_match_sdi_cohort_final_4to1_pregmax_agecat %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 15, 19), 1)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 20, 24), 2)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 25, 29), 3)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 30, 34), 4)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 35, 39), 5)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 40, 44), 6)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 45, 49), 7)) 
exact_match_sdi_cohort_final_4to1_pregmax_agecat$agecat_cohort_entry <- factor(exact_match_sdi_cohort_final_4to1_pregmax_agecat$agecat_cohort_entry, levels = c(1,2,3,4,5,6,7),
                                                                               labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

agecat <- exact_match_sdi_cohort_final_4to1_pregmax_agecat %>% 
  group_by(agecat_cohort_entry) %>% count() 
agecat <- agecat %>% mutate(percent = (n/sum(agecat$n))*100) %>%
  mutate(group = "Whole Cohort")
agecat_mvnm <- exact_match_sdi_cohort_final_4to1_pregmax_agecat %>% 
  group_by(agecat_cohort_entry, migrant_status) %>% count() %>% 
  rename(group = migrant_status)
agecat_m <- agecat_mvnm %>% filter(group == "Migrant")
agecat_m <- agecat_m %>% mutate(percent = (n/sum(agecat_m$n))*100)
agecat_nm <- agecat_mvnm %>% filter(group == "Non-migrant")
agecat_nm <- agecat_nm %>% mutate(percent = (n/sum(agecat_nm$n))*100)
agecat_dp <- exact_match_sdi_cohort_final_4to1_pregmax_agecat %>% 
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

sdi_pyears_max_agecat <- aggregate(sdi_pyears_max ~ agecat_cohort_entry, exact_match_sdi_cohort_final_4to1_pregmax_agecat, sum) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_agecat_mvnm <- aggregate(sdi_pyears_max ~ agecat_cohort_entry + migrant_status, exact_match_sdi_cohort_final_4to1_pregmax_agecat, sum) %>% 
  rename(group = migrant_status)
sdi_pyears_max_agecat_dp <- aggregate(sdi_pyears_max ~ agecat_cohort_entry + migcertainty, exact_match_sdi_cohort_final_4to1_pregmax_agecat, sum) %>% 
  rename(group = migcertainty)
sdi_pyears_max_agecat_all <- sdi_pyears_max_agecat_mvnm %>% full_join(sdi_pyears_max_agecat_dp,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                                                                                      "group"="group","sdi_pyears_max"="sdi_pyears_max")) %>% 
  full_join(sdi_pyears_max_agecat,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                         "group"="group","sdi_pyears_max"="sdi_pyears_max")) %>%
  rename(agecat = agecat_cohort_entry) 

sdi_pyears_max_agecat_export <- full_join(sdi_pyears_max_agecat_all, age_cat_all, 
                                          by = c("agecat"="agecat","group"="group")) %>% 
  mutate(across(is.numeric, ~ round(.,0))) %>% 
  dplyr::select(c(group, agecat, n_percent, sdi_pyears_max)) 

write_csv(sdi_pyears_max_agecat_export, "filepath")


## Ethnicity ---


exact_match_sdi_cohort_final_4to1_pregmax$ethnicat6 <- exact_match_sdi_cohort_final_4to1_pregmax$ethnicat6 %>% fct_explicit_na(na_level = "Not known")
levels(exact_match_sdi_cohort_final_4to1_pregmax$ethnicat6)

ethnicat6 <- exact_match_sdi_cohort_final_4to1_pregmax %>% 
  group_by(ethnicat6) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
ethnicat6_mvnm <- exact_match_sdi_cohort_final_4to1_pregmax %>% 
  group_by(ethnicat6, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(ethnicat6, group, n, percent))
ethnicat6_dp <- exact_match_sdi_cohort_final_4to1_pregmax %>% 
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



sdi_pyears_max_ethnicat6 <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(ethnicat6) %>% tally(sdi_pyears_max) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_ethnicat6_mvnm <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migrant_status, ethnicat6) %>% tally(sdi_pyears_max) %>% 
  rename(group = migrant_status)
sdi_pyears_max_ethnicat6_dp <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migcertainty, ethnicat6) %>% tally(sdi_pyears_max) %>% 
  rename(group = migcertainty)
sdi_pyears_max_ethnicat6_all <- sdi_pyears_max_ethnicat6_mvnm %>% full_join(sdi_pyears_max_ethnicat6_dp,by = c("ethnicat6"="ethnicat6",
                                                                                                               "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_max_ethnicat6,by = c("ethnicat6"="ethnicat6",
                                            "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_max_ethnicat6_export <- full_join(sdi_pyears_max_ethnicat6_all, ethnicat6_all, 
                                             by = c("ethnicat6"="ethnicat6","group"="group")) %>% 
  relocate(group,ethnicat6,n_percent,sdi_pyears_max)  %>% 
  arrange(group)

write_csv(sdi_pyears_max_ethnicat6_export, "filepath")


## practice region ---

exact_match_sdi_cohort_final_4to1_pregmax$prac_region <- exact_match_sdi_cohort_final_4to1_pregmax$prac_region %>% fct_explicit_na(na_level = "Not known")
levels(exact_match_sdi_cohort_final_4to1_pregmax$prac_region)

prac_region <- exact_match_sdi_cohort_final_4to1_pregmax %>% 
  group_by(prac_region) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
prac_region_mvnm <- exact_match_sdi_cohort_final_4to1_pregmax %>% 
  group_by(prac_region, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(prac_region, group, n, percent))
prac_region_dp <- exact_match_sdi_cohort_final_4to1_pregmax %>% 
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



sdi_pyears_max_prac_region <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(prac_region) %>% tally(sdi_pyears_max) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_prac_region_mvnm <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migrant_status, prac_region) %>% tally(sdi_pyears_max) %>% 
  rename(group = migrant_status)
sdi_pyears_max_prac_region_dp <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migcertainty, prac_region) %>% tally(sdi_pyears_max) %>% 
  rename(group = migcertainty)
sdi_pyears_max_prac_region_all <- sdi_pyears_max_prac_region_mvnm %>% full_join(sdi_pyears_max_prac_region_dp,by = c("prac_region"="prac_region",
                                                                                                                     "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_max_prac_region,by = c("prac_region"="prac_region",
                                              "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_max_prac_region_export <- full_join(sdi_pyears_max_prac_region_all, prac_region_all, 
                                               by = c("prac_region"="prac_region","group"="group")) %>% 
  relocate(group,prac_region,n_percent,sdi_pyears_max)  %>% 
  arrange(group)

write_csv(sdi_pyears_max_prac_region_export, "filepath")


## IMD ---

exact_match_sdi_cohort_final_4to1_pregmax$imd<- exact_match_sdi_cohort_final_4to1_pregmax$imd%>% fct_explicit_na(na_level = "Not known")
levels(exact_match_sdi_cohort_final_4to1_pregmax$imd)

imd<- exact_match_sdi_cohort_final_4to1_pregmax %>% 
  group_by(imd) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
imd_mvnm <- exact_match_sdi_cohort_final_4to1_pregmax %>% 
  group_by(imd, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(imd, group, n, percent))
imd_dp <- exact_match_sdi_cohort_final_4to1_pregmax %>% 
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

sdi_pyears_max_imd <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(imd) %>% tally(sdi_pyears_max) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_imd_mvnm <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migrant_status, imd) %>% tally(sdi_pyears_max) %>% 
  rename(group = migrant_status)
sdi_pyears_max_imd_dp <- exact_match_sdi_cohort_final_4to1_pregmax %>% group_by(migcertainty, imd) %>% tally(sdi_pyears_max) %>% 
  rename(group = migcertainty)
sdi_pyears_max_imd_all <- sdi_pyears_max_imd_mvnm %>% full_join(sdi_pyears_max_imd_dp,by = c("imd"="imd",
                                                                                             "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_max_imd,by = c("imd"="imd",
                                      "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_max_imd_export <- full_join(sdi_pyears_max_imd_all, imd_all, 
                                       by = c("imd"="imd","group"="group")) %>% 
  relocate(group,imd,n_percent,sdi_pyears_max)  %>% 
  arrange(group)

write_csv(sdi_pyears_max_imd_export, "filepath")

## Year of cohort entry ---

exact_match_sdi_cohort_final_4to1_pregmax_yearofentry <- exact_match_sdi_cohort_final_4to1_pregmax %>% mutate(yearofentry = year(sdi_cohort_entry))
mean_yearofentry_overall <- psych::describe(exact_match_sdi_cohort_final_4to1_pregmax_yearofentry$yearofentry, IQR = TRUE)
mean_yearofentry_overall

nm <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% filter(migrant_status == "Non-migrant")
nm_yearofentry <- psych::describe(nm$yearofentry, IQR = TRUE)
nm_yearofentry

m <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% filter(migrant_status == "Migrant")
m_yearofentry <- psych::describe(m$yearofentry, IQR = TRUE)
m_yearofentry

d <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% filter(migcertainty == "Definite")
d_yearofentry  <- psych::describe(d$yearofentry, IQR = TRUE)
d_yearofentry 

p <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% filter(migcertainty == "Probable")
p_yearofentry <- psych::describe(p$yearofentry, IQR = TRUE)
p_yearofentry

## join export mean yearsof entry + sds
mean_yearofentry_overall <- as.data.frame(mean_yearofentry_overall)
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
write_csv(all_mean_yearofentry, "filepath" )


exact_match_sdi_cohort_final_4to1_pregmax_yearofentry$yearofentry <- factor(exact_match_sdi_cohort_final_4to1_pregmax_yearofentry$yearofentry,  levels = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018), 
                                                                            labels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
yearofentry <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% 
  group_by(yearofentry) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
yearofentry_mvnm <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% 
  group_by(yearofentry, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(yearofentry, group, n, percent))
yearofentry_dp <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% 
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

sdi_pyears_max_yearofentry <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% group_by(yearofentry) %>% tally(sdi_pyears_max) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_max_yearofentry_mvnm <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% group_by(migrant_status, yearofentry) %>% tally(sdi_pyears_max) %>% 
  rename(group = migrant_status)
sdi_pyears_max_yearofentry_dp <- exact_match_sdi_cohort_final_4to1_pregmax_yearofentry %>% group_by(migcertainty, yearofentry) %>% tally(sdi_pyears_max) %>% 
  rename(group = migcertainty)
sdi_pyears_max_yearofentry_all <- sdi_pyears_max_yearofentry_mvnm %>% full_join(sdi_pyears_max_yearofentry_dp,by = c("yearofentry"="yearofentry",
                                                                                                                     "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_max_yearofentry,by = c("yearofentry"="yearofentry",
                                              "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_max_yearofentry_export <- full_join(sdi_pyears_max_yearofentry_all, yearofentry_all, 
                                               by = c("group"="group","yearofentry"="yearofentry")) %>% 
  relocate(group,yearofentry,n_percent,sdi_pyears_max)  %>% 
  arrange(group)

write_csv(sdi_pyears_max_yearofentry_export, "filepath")


## Year ---

eventyear<- exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max %>% 
  group_by(eventyear) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
eventyear_mvnm <- exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max  %>% 
  group_by(eventyear, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(eventyear, group, n, percent))
eventyear_dp <- exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max  %>% 
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

pyears_eventyear <- exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max  %>% group_by(eventyear) %>% tally(pyears_ey_pregmax) %>% 
  mutate(group = "Whole Cohort")
pyears_eventyear_mvnm <- exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max  %>% group_by(migrant_status, eventyear) %>% tally(pyears_ey_pregmax) %>% 
  rename(group = migrant_status)
pyears_eventyear_dp <- exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max  %>% group_by(migcertainty, eventyear) %>% tally(pyears_ey_pregmax) %>% 
  rename(group = migcertainty)
pyears_eventyear_all <- pyears_eventyear_mvnm %>% full_join(pyears_eventyear_dp,by = c("eventyear"="eventyear",
                                                                                       "group"="group","n"="n")) %>% 
  full_join(pyears_eventyear,by = c("eventyear"="eventyear",
                                    "group"="group","n"="n")) %>%
  rename(sdi_pyears_max = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_eventyear_export <- full_join(pyears_eventyear_all, eventyear_all, 
                                     by = c("eventyear"="eventyear","group"="group")) %>% 
  relocate(group,eventyear,n_percent,sdi_pyears_max)  %>% 
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

## Person years ---

mean_sdi_pyears_max_overall <- psych::describe(exact_match_sdi_cohort_final_4to1_pregmax$sdi_pyears_max, IQR = TRUE)
mean_sdi_pyears_max_overall

nm <- exact_match_sdi_cohort_final_4to1_pregmax %>% filter(migrant_status == "Non-migrant")
nm_sdi_pyears_max <- psych::describe(nm$sdi_pyears_max, IQR = TRUE)
nm_sdi_pyears_max

m <- exact_match_sdi_cohort_final_4to1_pregmax %>% filter(migrant_status == "Migrant")
m_sdi_pyears_max <- psych::describe(m$sdi_pyears_max, IQR = TRUE)
m_sdi_pyears_max

d <- exact_match_sdi_cohort_final_4to1_pregmax %>% filter(migcertainty == "Definite")
d_sdi_pyears_max <- psych::describe(d$sdi_pyears_max, IQR = TRUE)
d_sdi_pyears_max

p <- exact_match_sdi_cohort_final_4to1_pregmax %>% filter(migcertainty == "Probable")
p_sdi_pyears_max <- psych::describe(p$sdi_pyears_max, IQR = TRUE)
p_sdi_pyears_max

## join export mean + median ages + sd + IQR
mean_sdi_pyears_max_overall <- as.data.frame((mean_sdi_pyears_max_overall))
mean_sdi_pyears_max_overall <- dplyr::select(mean_sdi_pyears_max_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_sdi_pyears_max <- as.data.frame((nm_sdi_pyears_max))
nm_sdi_pyears_max <- dplyr::select(nm_sdi_pyears_max, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_sdi_pyears_max <- as.data.frame((m_sdi_pyears_max))
m_sdi_pyears_max <- dplyr::select(m_sdi_pyears_max, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_sdi_pyears_max <- as.data.frame((d_sdi_pyears_max))
d_sdi_pyears_max <- dplyr::select(d_sdi_pyears_max, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_sdi_pyears_max <- as.data.frame((p_sdi_pyears_max))
p_sdi_pyears_max <- dplyr::select(p_sdi_pyears_max, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_sdi_pyears_max <- full_join(mean_sdi_pyears_max_overall, nm_sdi_pyears_max,
                                     by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_sdi_pyears_max, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_sdi_pyears_max, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_sdi_pyears_max, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_sdi_pyears_max$mean_sd <- paste(all_mean_sdi_pyears_max$mean, all_mean_sdi_pyears_max$sd, sep =", ")
all_mean_sdi_pyears_max$median_iqr <- paste(all_mean_sdi_pyears_max$median, all_mean_sdi_pyears_max$IQR, sep =", ")
all_mean_sdi_pyears_max <- dplyr::select(all_mean_sdi_pyears_max, c(group, mean_sd, median_iqr))
write_csv(all_mean_sdi_pyears_max, "filepath")





## BASELINE CHARACTERISTICS - PREG MIN ASSUMPTION ----------------------------------------------------------------------------------


## change cohort final to be based on preg min assumption
# drop records with 0 or less pdays_ey_pregmin in counts
sdipresc_annual_counts_final_extra_preg_min <- sdipresc_annual_counts_final_extra %>% filter(pdays_ey_pregmin > 0) 
# recalculate pdays total based on counts data and join
sdipresc_annual_counts_final_extra_preg_min_pdays <- sdipresc_annual_counts_final_extra_preg_min %>% group_by(patid) %>% tally(pdays_ey_pregmin) %>% rename(sdi_pdays_min = n )
sdipresc_annual_counts_final_extra_preg_min_pyears <- sdipresc_annual_counts_final_extra_preg_min %>% group_by(patid) %>% tally(pyears_ey_pregmin) %>% rename(sdi_pyears_min = n )
sdipresc_annual_counts_final_extra_preg_min_days_pyears <- full_join(sdipresc_annual_counts_final_extra_preg_min_pdays, sdipresc_annual_counts_final_extra_preg_min_pyears, by = c("patid" = "patid"))
# select patients in cohort file based on preg min assumption and add on recalculated pdays/pyears
sdi_cohort_final_pregmin <- sdi_cohort_final %>% filter(patid %in% sdipresc_annual_counts_final_extra_preg_min$patid) %>% dplyr::select(-sdi_pdays_min, -sdi_pdays_min, -sdi_pyears_min, -sdi_pyears_min)
sdi_cohort_final_pregmin <- left_join(sdi_cohort_final_pregmin, sdipresc_annual_counts_final_extra_preg_min_days_pyears, by = c("patid" = "patid"))

## Totals ---

## Whole cohort 
total_cohort_n <- count(sdi_cohort_final_pregmin)
total_sdi_pyears_min <- sum(sdi_cohort_final_pregmin$sdi_pyears_min)
total_wholecohort <- bind_cols(total_cohort_n, total_sdi_pyears_min)
colnames(total_wholecohort) <- c("individuals", "sdi_pyears_min")
total_wholecohort <- total_wholecohort %>% mutate(group = "Whole cohort") 

## By migrant status
total_mvnm_n <- sdi_cohort_final_pregmin %>% group_by(migrant_status) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migrant_status)
total_mvnm_sdi_pyears_min <- sdi_cohort_final_pregmin %>% group_by(migrant_status) %>%
  tally(sdi_pyears_min) %>%
  rename(sdi_pyears_min = n) %>% 
  rename(group = migrant_status)
total_mvnm <- full_join(total_mvnm_n, total_mvnm_sdi_pyears_min,
                        by = c("group" = "group"))

## By migrant certainty
total_dp_n <- sdi_cohort_final_pregmin %>% group_by(migcertainty) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migcertainty)
total_dp_sdi_pyears_min <- sdi_cohort_final_pregmin %>% group_by(migcertainty) %>%
  tally(sdi_pyears_min) %>%
  rename(sdi_pyears_min = n) %>% 
  rename(group = migcertainty)
total_dp <- full_join(total_dp_n, total_dp_sdi_pyears_min,
                      by = c("group" = "group"))

## Totals for export
totals <- full_join(total_mvnm, total_dp,
                    by = c("group" = "group", "individuals" = "individuals", "sdi_pyears_min" = "sdi_pyears_min")) %>% 
  full_join(total_wholecohort, by = c("group" = "group", "individuals" = "individuals", "sdi_pyears_min" = "sdi_pyears_min")) %>% 
  mutate(across(is.numeric, ~ round(.,0)))
total_cohort_n<- as.vector(total_cohort_n$n[1])
totals_percent <- totals %>% mutate(percent = (individuals/total_cohort_n)*100) %>% 
  mutate(percent = round(percent, 1))

totals_percent$individuals <- paste(totals_percent$individuals, totals_percent$percent, sep =", ")
totals_percent <- dplyr::select(totals_percent, c(group, individuals, sdi_pyears_min))
write_csv(totals_percent, "filepath")


## Age ---

## Mean + Median + SD + IQR Age at cohort entry 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

sdi_cohort_final_pregmin_age <- sdi_cohort_final_pregmin %>%
  mutate(age_cohort_entry = calc_age(dob, sdi_cohort_entry))
mean_ages_overall <- psych::describe(sdi_cohort_final_pregmin_age$age_cohort_entry, IQR = TRUE)
mean_ages_overall

nm <- sdi_cohort_final_pregmin_age %>% filter(migrant_status == "Non-migrant")
nm_age <- psych::describe(nm$age_cohort_entry, IQR = TRUE)
nm_age

m <- sdi_cohort_final_pregmin_age %>% filter(migrant_status == "Migrant")
m_age <- psych::describe(m$age_cohort_entry, IQR = TRUE)
m_age

d <- sdi_cohort_final_pregmin_age %>% filter(migcertainty == "Definite")
d_age <- psych::describe(d$age_cohort_entry, IQR = TRUE)
d_age

p <- sdi_cohort_final_pregmin_age %>% filter(migcertainty == "Probable")
p_age <- psych::describe(p$age_cohort_entry, IQR = TRUE)
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
sdi_cohort_final_pregmin_agecat <- sdi_cohort_final_pregmin_age %>%
  mutate(agecat_cohort_entry = age_cohort_entry)
sdi_cohort_final_pregmin_agecat <- sdi_cohort_final_pregmin_agecat %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 15, 19), 1)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 20, 24), 2)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 25, 29), 3)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 30, 34), 4)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 35, 39), 5)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 40, 44), 6)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 45, 49), 7)) 
sdi_cohort_final_pregmin_agecat$agecat_cohort_entry <- factor(sdi_cohort_final_pregmin_agecat$agecat_cohort_entry, levels = c(1,2,3,4,5,6,7),
                                                              labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

agecat <- sdi_cohort_final_pregmin_agecat %>% 
  group_by(agecat_cohort_entry) %>% count() 
agecat <- agecat %>% mutate(percent = (n/sum(agecat$n))*100) %>%
  mutate(group = "Whole Cohort")
agecat_mvnm <- sdi_cohort_final_pregmin_agecat %>% 
  group_by(agecat_cohort_entry, migrant_status) %>% count() %>% 
  rename(group = migrant_status)
agecat_m <- agecat_mvnm %>% filter(group == "Migrant")
agecat_m <- agecat_m %>% mutate(percent = (n/sum(agecat_m$n))*100)
agecat_nm <- agecat_mvnm %>% filter(group == "Non-migrant")
agecat_nm <- agecat_nm %>% mutate(percent = (n/sum(agecat_nm$n))*100)
agecat_dp <- sdi_cohort_final_pregmin_agecat %>% 
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

sdi_pyears_min_agecat <- aggregate(sdi_pyears_min ~ agecat_cohort_entry, sdi_cohort_final_pregmin_agecat, sum) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_min_agecat_mvnm <- aggregate(sdi_pyears_min ~ agecat_cohort_entry + migrant_status, sdi_cohort_final_pregmin_agecat, sum) %>% 
  rename(group = migrant_status)
sdi_pyears_min_agecat_dp <- aggregate(sdi_pyears_min ~ agecat_cohort_entry + migcertainty, sdi_cohort_final_pregmin_agecat, sum) %>% 
  rename(group = migcertainty)
sdi_pyears_min_agecat_all <- sdi_pyears_min_agecat_mvnm %>% full_join(sdi_pyears_min_agecat_dp,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                                                                                      "group"="group","sdi_pyears_min"="sdi_pyears_min")) %>% 
  full_join(sdi_pyears_min_agecat,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                         "group"="group","sdi_pyears_min"="sdi_pyears_min")) %>%
  rename(agecat = agecat_cohort_entry) 

sdi_pyears_min_agecat_export <- full_join(sdi_pyears_min_agecat_all, age_cat_all, 
                                          by = c("agecat"="agecat","group"="group")) %>% 
  mutate(across(is.numeric, ~ round(.,0))) %>% 
  dplyr::select(c(group, agecat, n_percent, sdi_pyears_min)) 

write_csv(sdi_pyears_min_agecat_export, "filepath")


## Ethnicity ---


sdi_cohort_final_pregmin$ethnicat6 <- sdi_cohort_final_pregmin$ethnicat6 %>% fct_explicit_na(na_level = "Not known")
levels(sdi_cohort_final_pregmin$ethnicat6)

ethnicat6 <- sdi_cohort_final_pregmin %>% 
  group_by(ethnicat6) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
ethnicat6_mvnm <- sdi_cohort_final_pregmin %>% 
  group_by(ethnicat6, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(ethnicat6, group, n, percent))
ethnicat6_dp <- sdi_cohort_final_pregmin %>% 
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



sdi_pyears_min_ethnicat6 <- sdi_cohort_final_pregmin %>% group_by(ethnicat6) %>% tally(sdi_pyears_min) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_min_ethnicat6_mvnm <- sdi_cohort_final_pregmin %>% group_by(migrant_status, ethnicat6) %>% tally(sdi_pyears_min) %>% 
  rename(group = migrant_status)
sdi_pyears_min_ethnicat6_dp <- sdi_cohort_final_pregmin %>% group_by(migcertainty, ethnicat6) %>% tally(sdi_pyears_min) %>% 
  rename(group = migcertainty)
sdi_pyears_min_ethnicat6_all <- sdi_pyears_min_ethnicat6_mvnm %>% full_join(sdi_pyears_min_ethnicat6_dp,by = c("ethnicat6"="ethnicat6",
                                                                                                               "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_min_ethnicat6,by = c("ethnicat6"="ethnicat6",
                                            "group"="group","n"="n")) %>%
  rename(sdi_pyears_min = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_min_ethnicat6_export <- full_join(sdi_pyears_min_ethnicat6_all, ethnicat6_all, 
                                             by = c("ethnicat6"="ethnicat6","group"="group")) %>% 
  relocate(group,ethnicat6,n_percent,sdi_pyears_min)  %>% 
  arrange(group)

write_csv(sdi_pyears_min_ethnicat6_export, "filepath")


## practice region ---

sdi_cohort_final_pregmin$prac_region <- sdi_cohort_final_pregmin$prac_region %>% fct_explicit_na(na_level = "Not known")
levels(sdi_cohort_final_pregmin$prac_region)

prac_region <- sdi_cohort_final_pregmin %>% 
  group_by(prac_region) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
prac_region_mvnm <- sdi_cohort_final_pregmin %>% 
  group_by(prac_region, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(prac_region, group, n, percent))
prac_region_dp <- sdi_cohort_final_pregmin %>% 
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



sdi_pyears_min_prac_region <- sdi_cohort_final_pregmin %>% group_by(prac_region) %>% tally(sdi_pyears_min) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_min_prac_region_mvnm <- sdi_cohort_final_pregmin %>% group_by(migrant_status, prac_region) %>% tally(sdi_pyears_min) %>% 
  rename(group = migrant_status)
sdi_pyears_min_prac_region_dp <- sdi_cohort_final_pregmin %>% group_by(migcertainty, prac_region) %>% tally(sdi_pyears_min) %>% 
  rename(group = migcertainty)
sdi_pyears_min_prac_region_all <- sdi_pyears_min_prac_region_mvnm %>% full_join(sdi_pyears_min_prac_region_dp,by = c("prac_region"="prac_region",
                                                                                                                     "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_min_prac_region,by = c("prac_region"="prac_region",
                                              "group"="group","n"="n")) %>%
  rename(sdi_pyears_min = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_min_prac_region_export <- full_join(sdi_pyears_min_prac_region_all, prac_region_all, 
                                               by = c("prac_region"="prac_region","group"="group")) %>% 
  relocate(group,prac_region,n_percent,sdi_pyears_min)  %>% 
  arrange(group)

write_csv(sdi_pyears_min_prac_region_export, "filepath")


## IMD ---

sdi_cohort_final_pregmin$imd<- sdi_cohort_final_pregmin$imd%>% fct_explicit_na(na_level = "Not known")
levels(sdi_cohort_final_pregmin$imd)

imd<- sdi_cohort_final_pregmin %>% 
  group_by(imd) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
imd_mvnm <- sdi_cohort_final_pregmin %>% 
  group_by(imd, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(imd, group, n, percent))
imd_dp <- sdi_cohort_final_pregmin %>% 
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

sdi_pyears_min_imd <- sdi_cohort_final_pregmin %>% group_by(imd) %>% tally(sdi_pyears_min) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_min_imd_mvnm <- sdi_cohort_final_pregmin %>% group_by(migrant_status, imd) %>% tally(sdi_pyears_min) %>% 
  rename(group = migrant_status)
sdi_pyears_min_imd_dp <- sdi_cohort_final_pregmin %>% group_by(migcertainty, imd) %>% tally(sdi_pyears_min) %>% 
  rename(group = migcertainty)
sdi_pyears_min_imd_all <- sdi_pyears_min_imd_mvnm %>% full_join(sdi_pyears_min_imd_dp,by = c("imd"="imd",
                                                                                             "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_min_imd,by = c("imd"="imd",
                                      "group"="group","n"="n")) %>%
  rename(sdi_pyears_min = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_min_imd_export <- full_join(sdi_pyears_min_imd_all, imd_all, 
                                       by = c("imd"="imd","group"="group")) %>% 
  relocate(group,imd,n_percent,sdi_pyears_min)  %>% 
  arrange(group)

write_csv(sdi_pyears_min_imd_export, "filepath")



## Year of cohort entry ---

sdi_cohort_final_pregmin_yearofentry <- sdi_cohort_final_pregmin %>% mutate(yearofentry = year(sdi_cohort_entry))
mean_yearofentry_overall <- psych::describe(sdi_cohort_final_pregmin_yearofentry$yearofentry, IQR = TRUE)
mean_yearofentry_overall

nm <- sdi_cohort_final_pregmin_yearofentry %>% filter(migrant_status == "Non-migrant")
nm_yearofentry <- psych::describe(nm$yearofentry, IQR = TRUE)
nm_yearofentry

m <- sdi_cohort_final_pregmin_yearofentry %>% filter(migrant_status == "Migrant")
m_yearofentry <- psych::describe(m$yearofentry, IQR = TRUE)
m_yearofentry

d <- sdi_cohort_final_pregmin_yearofentry %>% filter(migcertainty == "Definite")
d_yearofentry  <- psych::describe(d$yearofentry, IQR = TRUE)
d_yearofentry 

p <- sdi_cohort_final_pregmin_yearofentry %>% filter(migcertainty == "Probable")
p_yearofentry <- psych::describe(p$yearofentry, IQR = TRUE)
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


sdi_cohort_final_pregmin_yearofentry$yearofentry <- factor(sdi_cohort_final_pregmin_yearofentry$yearofentry,  levels = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018), 
                                                           labels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
yearofentry <- sdi_cohort_final_pregmin_yearofentry %>% 
  group_by(yearofentry) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
yearofentry_mvnm <- sdi_cohort_final_pregmin_yearofentry %>% 
  group_by(yearofentry, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(yearofentry, group, n, percent))
yearofentry_dp <- sdi_cohort_final_pregmin_yearofentry %>% 
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

sdi_pyears_min_yearofentry <- sdi_cohort_final_pregmin_yearofentry %>% group_by(yearofentry) %>% tally(sdi_pyears_min) %>% 
  mutate(group = "Whole Cohort")
sdi_pyears_min_yearofentry_mvnm <- sdi_cohort_final_pregmin_yearofentry %>% group_by(migrant_status, yearofentry) %>% tally(sdi_pyears_min) %>% 
  rename(group = migrant_status)
sdi_pyears_min_yearofentry_dp <- sdi_cohort_final_pregmin_yearofentry %>% group_by(migcertainty, yearofentry) %>% tally(sdi_pyears_min) %>% 
  rename(group = migcertainty)
sdi_pyears_min_yearofentry_all <- sdi_pyears_min_yearofentry_mvnm %>% full_join(sdi_pyears_min_yearofentry_dp,by = c("yearofentry"="yearofentry",
                                                                                                                     "group"="group","n"="n")) %>% 
  full_join(sdi_pyears_min_yearofentry,by = c("yearofentry"="yearofentry",
                                              "group"="group","n"="n")) %>%
  rename(sdi_pyears_min = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

sdi_pyears_min_yearofentry_export <- full_join(sdi_pyears_min_yearofentry_all, yearofentry_all, 
                                               by = c("group"="group","yearofentry"="yearofentry")) %>% 
  relocate(group,yearofentry,n_percent,sdi_pyears_min)  %>% 
  arrange(group)

write_csv(sdi_pyears_min_yearofentry_export, "filepath")


## Year ---

eventyear<- sdipresc_annual_counts_final_extra_preg_min %>% 
  group_by(eventyear) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
eventyear_mvnm <- sdipresc_annual_counts_final_extra_preg_min  %>% 
  group_by(eventyear, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(eventyear, group, n, percent))
eventyear_dp <- sdipresc_annual_counts_final_extra_preg_min  %>% 
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

pyears_eventyear <- sdipresc_annual_counts_final_extra_preg_min  %>% group_by(eventyear) %>% tally(pyears_ey_pregmin) %>% 
  mutate(group = "Whole Cohort")
pyears_eventyear_mvnm <- sdipresc_annual_counts_final_extra_preg_min  %>% group_by(migrant_status, eventyear) %>% tally(pyears_ey_pregmin) %>% 
  rename(group = migrant_status)
pyears_eventyear_dp <- sdipresc_annual_counts_final_extra_preg_min  %>% group_by(migcertainty, eventyear) %>% tally(pyears_ey_pregmin) %>% 
  rename(group = migcertainty)
pyears_eventyear_all <- pyears_eventyear_mvnm %>% full_join(pyears_eventyear_dp,by = c("eventyear"="eventyear",
                                                                                       "group"="group","n"="n")) %>% 
  full_join(pyears_eventyear,by = c("eventyear"="eventyear",
                                    "group"="group","n"="n")) %>%
  rename(sdi_pyears_min = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_eventyear_export <- full_join(pyears_eventyear_all, eventyear_all, 
                                     by = c("eventyear"="eventyear","group"="group")) %>% 
  relocate(group,eventyear,n_percent,sdi_pyears_min)  %>% 
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



## Person years ---

mean_sdi_pyears_min_overall <- psych::describe(sdi_cohort_final_pregmin$sdi_pyears_min, IQR = TRUE)
mean_sdi_pyears_min_overall

nm <- sdi_cohort_final_pregmin %>% filter(migrant_status == "Non-migrant")
nm_sdi_pyears_min <- psych::describe(nm$sdi_pyears_min, IQR = TRUE)
nm_sdi_pyears_min

m <- sdi_cohort_final_pregmin %>% filter(migrant_status == "Migrant")
m_sdi_pyears_min <- psych::describe(m$sdi_pyears_min, IQR = TRUE)
m_sdi_pyears_min

d <- sdi_cohort_final_pregmin %>% filter(migcertainty == "Definite")
d_sdi_pyears_min <- psych::describe(d$sdi_pyears_min, IQR = TRUE)
d_sdi_pyears_min

p <- sdi_cohort_final_pregmin %>% filter(migcertainty == "Probable")
p_sdi_pyears_min <- psych::describe(p$sdi_pyears_min, IQR = TRUE)
p_sdi_pyears_min

## join export mean + median ages + sd + IQR
mean_sdi_pyears_min_overall <- as.data.frame((mean_sdi_pyears_min_overall))
mean_sdi_pyears_min_overall <- dplyr::select(mean_sdi_pyears_min_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_sdi_pyears_min <- as.data.frame((nm_sdi_pyears_min))
nm_sdi_pyears_min <- dplyr::select(nm_sdi_pyears_min, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_sdi_pyears_min <- as.data.frame((m_sdi_pyears_min))
m_sdi_pyears_min <- dplyr::select(m_sdi_pyears_min, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_sdi_pyears_min <- as.data.frame((d_sdi_pyears_min))
d_sdi_pyears_min <- dplyr::select(d_sdi_pyears_min, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_sdi_pyears_min <- as.data.frame((p_sdi_pyears_min))
p_sdi_pyears_min <- dplyr::select(p_sdi_pyears_min, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_sdi_pyears_min <- full_join(mean_sdi_pyears_min_overall, nm_sdi_pyears_min,
                                     by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_sdi_pyears_min, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_sdi_pyears_min, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_sdi_pyears_min, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_sdi_pyears_min$mean_sd <- paste(all_mean_sdi_pyears_min$mean, all_mean_sdi_pyears_min$sd, sep =", ")
all_mean_sdi_pyears_min$median_iqr <- paste(all_mean_sdi_pyears_min$median, all_mean_sdi_pyears_min$IQR, sep =", ")
all_mean_sdi_pyears_min <- dplyr::select(all_mean_sdi_pyears_min, c(group, mean_sd, median_iqr))
write_csv(all_mean_sdi_pyears_min, "filepath")


## SUMMARY OUTCOMES --------------------

## change cohort final to be based on preg max assumption
# drop records with 0 or less pdays_ey_pregmax in counts
sdipresc_annual_counts_final_extra_preg_max <- sdipresc_annual_counts_final_extra %>% filter(pdays_ey_pregmax > 0) 
# recalculate pdays total based on counts data and join
sdipresc_annual_counts_final_extra_preg_max_pdays <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(patid) %>% tally(pdays_ey_pregmax) %>% rename(sdi_pdays_max = n )
sdipresc_annual_counts_final_extra_preg_max_pyears <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(patid) %>% tally(pyears_ey_pregmax) %>% rename(sdi_pyears_max = n )
sdipresc_annual_counts_final_extra_preg_max_days_pyears <- full_join(sdipresc_annual_counts_final_extra_preg_max_pdays, sdipresc_annual_counts_final_extra_preg_max_pyears, by = c("patid" = "patid"))
# select patients in cohort file based on preg max assumption and add on recalculated pdays/pyears
sdi_cohort_final_pregmax <- sdi_cohort_final %>% filter(patid %in% sdipresc_annual_counts_final_extra_preg_max$patid) %>% dplyr::select(-sdi_pdays_max, -sdi_pdays_min, -sdi_pyears_max, -sdi_pyears_min)
sdi_cohort_final_pregmax <- left_join(sdi_cohort_final_pregmax, sdipresc_annual_counts_final_extra_preg_max_days_pyears, by = c("patid" = "patid"))


## Overall - whole cohort
sdipresc_count_overall_whole_cohort <- sdipresc_annual_counts_final_extra_preg_max %>% tally(sdipresc_n_pregmax) %>% rename(n_events = n)
sdipresc_summary_overall_whole_cohort <- sdipresc_annual_counts_final_extra_preg_max %>% 
  summarise(mean = mean(sdipresc_n_pregmax), sd = sd(sdipresc_n_pregmax), median = median(sdipresc_n_pregmax), iqr = IQR(sdipresc_n_pregmax), 
            min = min(sdipresc_n_pregmax), max = max(sdipresc_n_pregmax))
sdipresc_count_summary_overall_whole_cohort <- merge(sdipresc_count_overall_whole_cohort, sdipresc_summary_overall_whole_cohort)
sdipresc_count_summary_overall_whole_cohort

## Overall by migrant status
sdipresc_count_overall_migrant_status <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(migrant_status) %>%
  tally(sdipresc_n_pregmax) %>% rename(n_events = n)
sdipresc_summary_overall_migrant_status <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(migrant_status) %>%
  summarise(mean = mean(sdipresc_n_pregmax), sd = sd(sdipresc_n_pregmax), median = median(sdipresc_n_pregmax), iqr = IQR(sdipresc_n_pregmax), 
            min = min(sdipresc_n_pregmax), max = max(sdipresc_n_pregmax))
sdipresc_count_summary_overall_migrant_status <- left_join(sdipresc_count_overall_migrant_status, sdipresc_summary_overall_migrant_status,
                                                             by = c("migrant_status"="migrant_status" ))

## Overall by migcertainty
sdipresc_count_overall_migcertainty <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(migcertainty) %>%
  tally(sdipresc_n_pregmax) %>% rename(n_events = n)
sdipresc_summary_overall_migcertainty <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(migcertainty) %>%
  summarise(mean = mean(sdipresc_n_pregmax), sd = sd(sdipresc_n_pregmax), median = median(sdipresc_n_pregmax), iqr = IQR(sdipresc_n_pregmax), 
            min = min(sdipresc_n_pregmax), max = max(sdipresc_n_pregmax))
sdipresc_count_summary_overall_migcertainty <- left_join(sdipresc_count_overall_migcertainty, sdipresc_summary_overall_migcertainty,
                                                           by = c("migcertainty"="migcertainty" ))

## Join for overall whole cohort + by migrant status
sdipresc_count_summary_overall_final <- full_join(sdipresc_count_summary_overall_whole_cohort, sdipresc_count_summary_overall_migrant_status, 
                                                    by = c("n_events"="n_events", "mean" = "mean",
                                                           "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max")) %>% 
  full_join(sdipresc_count_summary_overall_migcertainty, by = c("n_events"="n_events", "mean" = "mean",
                                                                  "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max",
                                                                  "migrant_status" = "migcertainty")) 
sdipresc_count_summary_overall_final$migrant_status <- fct_explicit_na(sdipresc_count_summary_overall_final$migrant_status, na_level = "whole_cohort")
sdipresc_count_summary_overall_final <- sdipresc_count_summary_overall_final %>% relocate(migrant_status)
sdipresc_count_summary_overall_final

write_csv(sdipresc_count_summary_overall_final, "filepath" )

## Annual - whole cohort 
sdipresc_count_annual_whole_cohort <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(eventyear) %>%
  tally(sdipresc_n_pregmax) %>% rename(n_events = n)
sdipresc_summary_annual_whole_cohort <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(eventyear) %>%
  summarise(mean = mean(sdipresc_n_pregmax), sd = sd(sdipresc_n_pregmax), median = median(sdipresc_n_pregmax), iqr = IQR(sdipresc_n_pregmax), 
            min = min(sdipresc_n_pregmax), max = max(sdipresc_n_pregmax))
sdipresc_count_summary_annual_whole_cohort <- left_join(sdipresc_count_annual_whole_cohort, sdipresc_summary_annual_whole_cohort,
                                                          by = c("eventyear"="eventyear"))


## Annual - by migrant status
sdipresc_count_annual_migrant_status <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(migrant_status, eventyear) %>%
  tally(sdipresc_n_pregmax) %>% rename(n_events = n)
sdipresc_summary_annual_migrant_status <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(migrant_status, eventyear) %>%
  summarise(mean = mean(sdipresc_n_pregmax), sd = sd(sdipresc_n_pregmax), median = median(sdipresc_n_pregmax), iqr = IQR(sdipresc_n_pregmax), 
            min = min(sdipresc_n_pregmax), max = max(sdipresc_n_pregmax))
sdipresc_count_summary_annual_migrant_status <- left_join(sdipresc_count_annual_migrant_status, sdipresc_summary_annual_migrant_status,
                                                            by = c("migrant_status"="migrant_status" , "eventyear"="eventyear"))

## Annual - by migcertainty 
sdipresc_count_annual_migcertainty <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(migcertainty, eventyear) %>%
  tally(sdipresc_n_pregmax) %>% rename(n_events = n)
sdipresc_summary_annual_migcertainty <- sdipresc_annual_counts_final_extra_preg_max %>% group_by(migcertainty, eventyear) %>%
  summarise(mean = mean(sdipresc_n_pregmax), sd = sd(sdipresc_n_pregmax), median = median(sdipresc_n_pregmax), iqr = IQR(sdipresc_n_pregmax), 
            min = min(sdipresc_n_pregmax), max = max(sdipresc_n_pregmax))
sdipresc_count_summary_annual_migcertainty <- left_join(sdipresc_count_annual_migcertainty, sdipresc_summary_annual_migcertainty,
                                                          by = c("migcertainty"="migcertainty" , "eventyear"="eventyear"))



## Join to give annual for whole cohort + migrant status
sdipresc_count_summary_annual_final <- full_join(sdipresc_count_summary_annual_whole_cohort, sdipresc_count_summary_annual_migrant_status, 
                                                   by = c("eventyear" = "eventyear", "n_events"="n_events", "mean" = "mean",
                                                          "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max")) 
sdipresc_count_summary_annual_final <-   full_join(sdipresc_count_summary_annual_final, sdipresc_count_summary_annual_migcertainty,
                                                     by = c("eventyear" = "eventyear", "n_events"="n_events", "mean" = "mean",
                                                            "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max",
                                                            "migrant_status" = "migcertainty"))

sdipresc_count_summary_annual_final$migrant_status <- fct_explicit_na(sdipresc_count_summary_annual_final$migrant_status, na_level = "whole_cohort") 
sdipresc_count_summary_annual_final <- sdipresc_count_summary_annual_final %>% 
  relocate(migrant_status)


## Export sdipresc summary measures
write_csv(sdipresc_count_summary_annual_final,"filepath" )



## MAIN ANALYSIS -----------------------------
# based on preg max assumption 
# some years have pdays/years_pregmax <0 - this means that those years should be dropped for these analyses as based on max assumption the person was pregnant for the whole period they were in the cohort that year
# for the pyears/pdays of baseline characteristics, the dropping of these will automatically occur 

# drop event years with pdays less than or equal to 0 - 4,892,032 x 28
sdipresc_annual_counts_final_extra_preg_max <- sdipresc_annual_counts_final_extra %>% filter(pdays_ey_pregmax > 0)

pyears_overall <- sum(sdipresc_annual_counts_final_extra_preg_max$pyears_ey_pregmax)
sdipresccount <- sum(sdipresc_annual_counts_final_extra_preg_max$sdipresc_n_pregmax)
rate_output <- pois.exact(sdipresccount, pyears_overall, conf.level=0.95)
rate_output <- rate_output %>% mutate(rate100000 = rate*100000) %>% mutate(lower100000 = lower*100000) %>% mutate(upper100000 = upper*100000)
rate_output

## IR & univariable IRR - migrant status ----------------------------------------------------------------------


# IR by migrant status - whole cohort
pyears_mvnm_overall <- aggregate(pyears_ey_pregmax~ migrant_status, sdipresc_annual_counts_final_extra_preg_max, sum) 
pyears_mvnm_overall <- pyears_mvnm_overall %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_mvnm <- aggregate(sdipresc_n_pregmax ~ migrant_status, sdipresc_annual_counts_final_extra_preg_max, sum) 
IR_mvnm <- inner_join(sdipresccount_mvnm,pyears_mvnm_overall, by= c("migrant_status" = "migrant_status"))
IR_mvnm_output <- pois.exact(IR_mvnm$sdipresc_n_pregmax, IR_mvnm$pyears_ey_pregmax, conf.level=0.95)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(lower100000=lower*100000)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(upper100000=upper*100000)
migrant_status <- IR_mvnm[,1]
IR_mvnm_output <- cbind(IR_mvnm_output, migrant_status)
IR_mvnm_output <- IR_mvnm_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_output <- IR_mvnm_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_output$ci <- paste(IR_mvnm_output$lower100000, IR_mvnm_output$upper100000, sep ="-")
IR_mvnm_output$ir_ci <- paste(IR_mvnm_output$incidence_rate100000, IR_mvnm_output$ci, sep =",")
IR_mvnm_output_table <-  dplyr::select(IR_mvnm_output, migrant_status, events, person_years, ir_ci)


##  generate IRR by negative binomial regression

x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + offset(log(pyears_ey_pregmax)), data = sdipresc_annual_counts_final_extra_preg_max)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm <- cbind(names,estimate,confint.default,p)
glm_mig <- as_tibble(glm_IRR_mvnm)
glm_mig$names <- factor(glm_mig$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant"))
glm_mig$names <- recode(glm_mig$names, "(Intercept)" = "Non-migrant" ,"as.factor(migrant_status)Migrant" = "Migrant")
glm_mig$estimate <- as.numeric(glm_mig$estimate)
glm_mig <- glm_mig %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mig$upper <- as.numeric(glm_mig$upper)
glm_mig$lower <- as.numeric(glm_mig$lower)
glm_mig$p <- as.numeric(glm_mig$p)
glm_mig <- glm_mig %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mig$ci <- paste(glm_mig$lower, glm_mig$upper, sep ="-")
glm_mig$estimate[1] <- 1.00
glm_mig$lower[1] <- 1.00
glm_mig$upper[1] <- 1.00
glm_mig$ci[1] <- "1.00-1.00"
glm_mig$irr_ci <- paste(glm_mig$estimate, glm_mig$ci, sep =",")
glm_mig_table <- dplyr::select(glm_mig,names, irr_ci ) 



## join glm + IR + univariable_mig 
univariable_mig <- full_join(IR_mvnm_output_table, glm_mig_table, by = c("migrant_status" = "names"))
write_csv(univariable_mig, "filepath")

## IR & univariable IRR - agecategory ------------------------------------------------------------------

## Set up data for stratifying by time variant vairable (agecat)
a<- sdipresc_annual_counts_final_extra_preg_max %>% group_by(patid, eventyear_agecat) %>% 
  tally(sdipresc_n_pregmax)
b<- sdipresc_annual_counts_final_extra_preg_max %>% group_by(patid, eventyear_agecat) %>% 
  tally(pyears_ey_pregmax)
c<- full_join(a,b, by = c("patid"="patid", "eventyear_agecat" = "eventyear_agecat"))
d<- left_join(c, sdi_cohort_final  , by = c("patid" = "patid")) %>% 
  rename(sdipresc_n_pregmax = n.x) %>% 
  rename(pyears_agecat = n.y)


# IR by age cat - whole cohort
pyears_agecat_overall <- aggregate(pyears_ey_pregmax ~ eventyear_agecat, sdipresc_annual_counts_final_extra_preg_max, sum) 
pyears_agecat_overall <- pyears_agecat_overall %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_agecat <- aggregate(sdipresc_n_pregmax ~ eventyear_agecat, sdipresc_annual_counts_final_extra_preg_max, sum) 
IR_agecat <- inner_join(sdipresccount_agecat,pyears_agecat_overall, by= c("eventyear_agecat" = "eventyear_agecat"))
IR_agecat_output <- pois.exact(IR_agecat$sdipresc_n_pregmax, IR_agecat$pyears_ey_pregmax, conf.level=0.95)
IR_agecat_output <- IR_agecat_output %>%
  mutate(incidence_rate100000=rate*100000) %>%
  mutate(lower100000=lower*100000) %>%
  mutate(upper100000=upper*100000)
eventyear_agecat <- IR_agecat[,1]
IR_agecat_output <- cbind(IR_agecat_output, eventyear_agecat)
IR_agecat_output <- IR_agecat_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate)  
IR_agecat_output <- IR_agecat_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_agecat_output$ci <- paste(IR_agecat_output$lower100000, IR_agecat_output$upper100000, sep ="-")
IR_agecat_output$ir_ci <- paste(IR_agecat_output$incidence_rate100000, IR_agecat_output$ci, sep =",")
IR_agecat_output_table <-  dplyr::select(IR_agecat_output, eventyear_agecat, events, person_years, ir_ci)

## Generate IRR via negative binomial regression univariabe for  year 
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(eventyear_agecat) + offset(log(pyears_agecat)), data = d)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_agecat <- cbind(names,estimate,confint.default,p)
glm_agecat <- as_tibble(glm_IRR_agecat)
glm_agecat$names <- factor(glm_agecat$names, c("(Intercept)" , 
                                               "as.factor(eventyear_agecat)20-24",
                                               "as.factor(eventyear_agecat)25-29",
                                               "as.factor(eventyear_agecat)30-34",
                                               "as.factor(eventyear_agecat)35-39",
                                               "as.factor(eventyear_agecat)40-44",
                                               "as.factor(eventyear_agecat)45-49"))
glm_agecat$names <- recode(glm_agecat$names, "(Intercept)" = "15-19" ,
                           "as.factor(eventyear_agecat)20-24" = "20-24",
                           "as.factor(eventyear_agecat)25-29" = "25-29",
                           "as.factor(eventyear_agecat)30-34" = "30-34",
                           "as.factor(eventyear_agecat)35-39" = "35-39",
                           "as.factor(eventyear_agecat)40-44" = "40-44",
                           "as.factor(eventyear_agecat)45-49" = "45-49")
glm_agecat$estimate <- as.numeric(glm_agecat$estimate)
glm_agecat <- glm_agecat %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_agecat$lower <- as.numeric(glm_agecat$lower)
glm_agecat$upper <- as.numeric(glm_agecat$upper)
glm_agecat$p <- as.numeric(glm_agecat$p)
glm_agecat <- glm_agecat %>% mutate(across(is.numeric, ~ round(.,2)))
glm_agecat$ci <- paste(glm_agecat$lower, glm_agecat$upper, sep ="-")
glm_agecat$estimate[1] <- 1.00
glm_agecat$lower[1] <- 1.00
glm_agecat$upper[1] <- 1.00
glm_agecat$ci[1] <- "1.00-1.00"
glm_agecat$irr_ci <- paste(glm_agecat$estimate, glm_agecat$ci, sep =",")
glm_agecat_table <- dplyr::select(glm_agecat,names, irr_ci ) 

## join glm + IR + export
univariable_agecat <- full_join(IR_agecat_output_table, glm_agecat_table, by = c("eventyear_agecat" = "names"))
write_csv(univariable_agecat, "filepath")

## IR & univariable IRR - prac region ------------------------------------------------------------------


# IR by imd  - whole cohort
pyears_prac_region_overall <- aggregate(pyears_ey_pregmax ~ prac_region, sdipresc_annual_counts_final_extra_preg_max, sum) 
pyears_prac_region_overall <- pyears_prac_region_overall %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_prac_region <- aggregate(sdipresc_n_pregmax ~ prac_region, sdipresc_annual_counts_final_extra_preg_max, sum) 
IR_prac_region <- inner_join(sdipresccount_prac_region,pyears_prac_region_overall, by= c("prac_region" = "prac_region"))
IR_prac_region_output <- pois.exact(IR_prac_region$sdipresc_n_pregmax, IR_prac_region$pyears_ey_pregmax, conf.level=0.95)
IR_prac_region_output <- IR_prac_region_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_prac_region_output <- IR_prac_region_output %>%
  mutate(lower100000=lower*100000)
IR_prac_region_output <- IR_prac_region_output %>%
  mutate(upper100000=upper*100000)
prac_region <- IR_prac_region[,1]
IR_prac_region_output <- cbind(IR_prac_region_output, prac_region)
IR_prac_region_output <- IR_prac_region_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_prac_region_output <- IR_prac_region_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_prac_region_output$ci <- paste(IR_prac_region_output$lower100000, IR_prac_region_output$upper100000, sep ="-")
IR_prac_region_output$ir_ci <- paste(IR_prac_region_output$incidence_rate100000, IR_prac_region_output$ci, sep =",")
IR_prac_region_output_table <-  dplyr::select(IR_prac_region_output, prac_region, events, person_years, ir_ci)

## Generate IRR via negative binomial regression univariabe for prac region
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(prac_region) + offset(log(pyears_ey_pregmax)), data = sdipresc_annual_counts_final_extra_preg_max)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_prac_region <- cbind(names,estimate,confint.default,p)
glm_prac_region <- as_tibble(glm_IRR_prac_region)
glm_prac_region$names <- factor(glm_prac_region$names, c("(Intercept)" , 
                                                         "as.factor(prac_region)North East",
                                                         "as.factor(prac_region)North West",
                                                         "as.factor(prac_region)Yorkshire & The Humber",
                                                         "as.factor(prac_region)East Midlands",
                                                         "as.factor(prac_region)West Midlands",
                                                         "as.factor(prac_region)East of England", 
                                                         "as.factor(prac_region)South West",
                                                         "as.factor(prac_region)South Central",
                                                         "as.factor(prac_region)South East Coast"))
glm_prac_region$names <- recode(glm_prac_region$names,"(Intercept)" = "London",
                                "as.factor(prac_region)North East" = "North East",
                                "as.factor(prac_region)North West" = "North West",
                                "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                                "as.factor(prac_region)East Midlands" = "East Midlands",
                                "as.factor(prac_region)West Midlands" = "West Midlands",
                                "as.factor(prac_region)East of England" = "East of England", 
                                "as.factor(prac_region)South West" = "South West",
                                "as.factor(prac_region)South Central" = "South Central",
                                "as.factor(prac_region)South East Coast" = "South East Coast")
glm_prac_region$estimate <- as.numeric(glm_prac_region$estimate)
glm_prac_region <- glm_prac_region %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_prac_region$lower <- as.numeric(glm_prac_region$lower)
glm_prac_region$upper <- as.numeric(glm_prac_region$upper)
glm_prac_region$p <- as.numeric(glm_prac_region$p)
glm_prac_region <- glm_prac_region %>% mutate(across(is.numeric, ~ round(.,2)))
glm_prac_region$ci <- paste(glm_prac_region$lower, glm_prac_region$upper, sep ="-")
glm_prac_region$estimate[1] <- 1.00
glm_prac_region$lower[1] <- 1.00
glm_prac_region$upper[1] <- 1.00
glm_prac_region$ci[1] <- "1.00-1.00"
glm_prac_region$irr_ci <- paste(glm_prac_region$estimate, glm_prac_region$ci, sep =",")
glm_prac_region_table <- dplyr::select(glm_prac_region,names, irr_ci ) 


## join glm + IR
univariable_prac_region <- full_join(IR_prac_region_output_table, glm_prac_region_table, by = c("prac_region" = "names"))
write_csv(univariable_prac_region, "filepath")


## IR & univariable IRR - imd ------------------------------------------------------------------


# IR by imd  - whole cohort
pyears_imd_overall <- aggregate(pyears_ey_pregmax ~ imd, sdipresc_annual_counts_final_extra_preg_max, sum) 
pyears_imd_overall <- pyears_imd_overall %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_imd <- aggregate(sdipresc_n_pregmax ~ imd, sdipresc_annual_counts_final_extra_preg_max, sum) 
IR_imd <- inner_join(sdipresccount_imd,pyears_imd_overall, by= c("imd" = "imd"))
IR_imd_output <- pois.exact(IR_imd$sdipresc_n_pregmax, IR_imd$pyears_ey_pregmax, conf.level=0.95)
IR_imd_output <- IR_imd_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_imd_output <- IR_imd_output %>%
  mutate(lower100000=lower*100000)
IR_imd_output <- IR_imd_output %>%
  mutate(upper100000=upper*100000)
imd <- IR_imd[,1]
IR_imd_output <- cbind(IR_imd_output, imd)
IR_imd_output <- IR_imd_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_imd_output <- IR_imd_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_imd_output$ci <- paste(IR_imd_output$lower100000, IR_imd_output$upper100000, sep ="-")
IR_imd_output$ir_ci <- paste(IR_imd_output$incidence_rate100000, IR_imd_output$ci, sep =",")
IR_imd_output_table <-  dplyr::select(IR_imd_output, imd, events, person_years, ir_ci)

## Generate IRR via negative binomial regression univariabe for imd
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(imd) + offset(log(pyears_ey_pregmax)), data = sdipresc_annual_counts_final_extra_preg_max)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_imd <- cbind(names,estimate,confint.default,p)
glm_imd <- as_tibble(glm_IRR_imd)
glm_imd$names <- factor(glm_imd$names, c("(Intercept)" , 
                                         "as.factor(imd)IMD 2",
                                         "as.factor(imd)IMD 3",
                                         "as.factor(imd)IMD 4",
                                         "as.factor(imd)IMD 5" ))
glm_imd$names <- recode(glm_imd$names, "(Intercept)" = "IMD 1" ,
                        "as.factor(imd)IMD 2" = "IMD 2",
                        "as.factor(imd)IMD 3" = "IMD 3",
                        "as.factor(imd)IMD 4" = "IMD 4",
                        "as.factor(imd)IMD 5" = "IMD 5")
glm_imd$estimate <- as.numeric(glm_imd$estimate)
glm_imd <- glm_imd %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_imd$lower <- as.numeric(glm_imd$lower)
glm_imd$upper <- as.numeric(glm_imd$upper)
glm_imd$p <- as.numeric(glm_imd$p)
glm_imd <- glm_imd %>% mutate(across(is.numeric, ~ round(.,2)))
glm_imd$ci <- paste(glm_imd$lower, glm_imd$upper, sep ="-")
glm_imd$estimate[1] <- 1.00
glm_imd$lower[1] <- 1.00
glm_imd$upper[1] <- 1.00
glm_imd$ci[1] <- "1.00-1.00"
glm_imd$irr_ci <- paste(glm_imd$estimate, glm_imd$ci, sep =",")
glm_imd_table <- dplyr::select(glm_imd,names, irr_ci ) 



## join glm + IR
univariable_imd <- full_join(IR_imd_output_table, glm_imd_table, by = c("imd" = "names"))
write_csv(univariable_imd, "filepath")

## IR & univariable IRR - year ------------------------------------------------------------------



## IR and 95%CI annually - whole cohort 
pyears_annual <- aggregate(pyears_ey_pregmax ~ eventyear, sdipresc_annual_counts_final_extra_preg_max, sum) 
pyears_annual <- pyears_annual %>% mutate(across(is.numeric, ~ round(.,)))
sdipresc_count_annual <- aggregate(sdipresc_n_pregmax ~ eventyear, sdipresc_annual_counts_final_extra_preg_max, sum) 
IR_annual <- inner_join(sdipresc_count_annual,pyears_annual, by= c("eventyear" = "eventyear"))
IR_annual_output <- pois.exact(IR_annual$sdipresc_n_pregmax, IR_annual$pyears_ey_pregmax, conf.level=0.95)
IR_annual_output <- IR_annual_output %>%
  mutate(rate100000=rate*100000) %>%
  mutate(lower100000=lower*100000) %>%
  mutate(upper100000=upper*100000)
eventyear <- IR_annual[,1]
IR_annual_output <- cbind(IR_annual_output, eventyear) 
IR_annual_output <- IR_annual_output %>% 
  relocate(eventyear) %>%
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) %>% 
  rename(incidence_rate_100000 = rate100000)
IR_annual_output <- IR_annual_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_annual_output$ci <- paste(IR_annual_output$lower100000, IR_annual_output$upper100000, sep ="-")
IR_annual_output$ir_ci <- paste(IR_annual_output$incidence_rate_100000, IR_annual_output$ci, sep =",")
IR_annual_output_table <-  dplyr::select(IR_annual_output, eventyear, events, person_years, ir_ci)
IR_annual_output_table$eventyear <- factor(IR_annual_output$eventyear, c("2009", "2010", "2011", "2012", 
                                                                         "2013", "2014", "2015", "2016", "2017", "2018"))



## Generate IRR via negative binomial regression univariabe for  year 
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(eventyear) + offset(log(pyears_ey_pregmax)), data = sdipresc_annual_counts_final_extra_preg_max)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_year <- cbind(names,estimate,confint.default,p)
glm_year <- as_tibble(glm_IRR_year)
glm_year$names <- factor(glm_year$names, c("(Intercept)" ,
                                           "as.factor(eventyear)2010",
                                           "as.factor(eventyear)2011",
                                           "as.factor(eventyear)2012",
                                           "as.factor(eventyear)2013",
                                           "as.factor(eventyear)2014",
                                           "as.factor(eventyear)2015",
                                           "as.factor(eventyear)2016",
                                           "as.factor(eventyear)2017",
                                           "as.factor(eventyear)2018" ))
glm_year$names <- recode(glm_year$names, "(Intercept)" = "2009" ,
                         "as.factor(eventyear)2010" = "2010",
                         "as.factor(eventyear)2011" = "2011",
                         "as.factor(eventyear)2012" = "2012",
                         "as.factor(eventyear)2013" = "2013",
                         "as.factor(eventyear)2014" = "2014",
                         "as.factor(eventyear)2015" = "2015",
                         "as.factor(eventyear)2016" = "2016",
                         "as.factor(eventyear)2017" = "2017",
                         "as.factor(eventyear)2018" = "2018")
glm_year$estimate <- as.numeric(glm_year$estimate)
glm_year <- glm_year %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_year$lower <- as.numeric(glm_year$lower)
glm_year$upper <- as.numeric(glm_year$upper)
glm_year$p <- as.numeric(glm_year$p)
glm_year <- glm_year %>% mutate(across(is.numeric, ~ round(.,2)))
glm_year$ci <- paste(glm_year$lower, glm_year$upper, sep ="-")
glm_year$estimate[1] <- 1.00
glm_year$lower[1] <- 1.00
glm_year$upper[1] <- 1.00
glm_year$ci[1] <- "1.00-1.00"
glm_year$irr_ci <- paste(glm_year$estimate, glm_year$ci, sep =",")
glm_year_table <- dplyr::select(glm_year,names, irr_ci ) 

## join glm + IR
univariable_year <- full_join(IR_annual_output_table, glm_year_table, by = c("eventyear" = "names"))
write_csv(univariable_year, "filepath")



## Multivariable IRR by migrant status  -------------------------------------------------------------------------
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey_pregmax)) , 
            data = sdipresc_annual_counts_final_extra_preg_max)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_all_mvnm <- cbind(names,estimate,confint.default,p)
glm_all_mvnm <- as_tibble(glm_IRR_all_mvnm)
glm_all_mvnm$names <- factor(glm_all_mvnm$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant", 
                                                   "as.factor(eventyear)2010",
                                                   "as.factor(eventyear)2011",
                                                   "as.factor(eventyear)2012",
                                                   "as.factor(eventyear)2013",
                                                   "as.factor(eventyear)2014",
                                                   "as.factor(eventyear)2015",
                                                   "as.factor(eventyear)2016",
                                                   "as.factor(eventyear)2017",
                                                   "as.factor(eventyear)2018" ,
                                                   "as.factor(eventyear_agecat)20-24",
                                                   "as.factor(eventyear_agecat)25-29",
                                                   "as.factor(eventyear_agecat)30-34",
                                                   "as.factor(eventyear_agecat)35-39",
                                                   "as.factor(eventyear_agecat)40-44",
                                                   "as.factor(eventyear_agecat)45-49",
                                                   "as.factor(imd)IMD 2",
                                                   "as.factor(imd)IMD 3",
                                                   "as.factor(imd)IMD 4",
                                                   "as.factor(imd)IMD 5",
                                                   "as.factor(prac_region)North East",
                                                   "as.factor(prac_region)North West",
                                                   "as.factor(prac_region)Yorkshire & The Humber",
                                                   "as.factor(prac_region)East Midlands",
                                                   "as.factor(prac_region)West Midlands",
                                                   "as.factor(prac_region)East of England", 
                                                   "as.factor(prac_region)South West",
                                                   "as.factor(prac_region)South Central",
                                                   "as.factor(prac_region)South East Coast"))
glm_all_mvnm$names <- recode(glm_all_mvnm$names,"(Intercept)" = "Non-migrant" ,
                             "as.factor(migrant_status)Migrant" = "Migrant",
                             "as.factor(eventyear)2010" = "2010",
                             "as.factor(eventyear)2011" = "2011",
                             "as.factor(eventyear)2012" = "2012",
                             "as.factor(eventyear)2013" = "2013",
                             "as.factor(eventyear)2014" = "2014",
                             "as.factor(eventyear)2015" = "2015",
                             "as.factor(eventyear)2016" = "2016",
                             "as.factor(eventyear)2017" = "2017",
                             "as.factor(eventyear)2018" = "2018",
                             "as.factor(eventyear_agecat)20-24" = "20-24",
                             "as.factor(eventyear_agecat)25-29" = "25-29",
                             "as.factor(eventyear_agecat)30-34" = "30-34",
                             "as.factor(eventyear_agecat)35-39" = "35-39",
                             "as.factor(eventyear_agecat)40-44" = "40-44",
                             "as.factor(eventyear_agecat)45-49" = "45-49",
                             "as.factor(imd)IMD 2" = "IMD 2",
                             "as.factor(imd)IMD 3" = "IMD 3",
                             "as.factor(imd)IMD 4" = "IMD 4",
                             "as.factor(imd)IMD 5" = "IMD 5",
                             "as.factor(prac_region)North East" = "North East",
                             "as.factor(prac_region)North West" = "North West",
                             "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                             "as.factor(prac_region)East Midlands" = "East Midlands",
                             "as.factor(prac_region)West Midlands" = "West Midlands",
                             "as.factor(prac_region)East of England" = "East of England", 
                             "as.factor(prac_region)South West" = "South West",
                             "as.factor(prac_region)South Central" = "South Central",
                             "as.factor(prac_region)South East Coast" = "South East Coast")
glm_all_mvnm$estimate <- as.numeric(glm_all_mvnm$estimate)
glm_all_mvnm <- glm_all_mvnm %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_all_mvnm$lower <- as.numeric(glm_all_mvnm$lower)
glm_all_mvnm$upper <- as.numeric(glm_all_mvnm$upper)
glm_all_mvnm$p <- as.numeric(glm_all_mvnm$p)
glm_all_mvnm <- glm_all_mvnm %>% mutate(across(is.numeric, ~ round(.,2)))
glm_all_mvnm$ci <- paste(glm_all_mvnm$lower, glm_all_mvnm$upper, sep ="-")
glm_all_mvnm$estimate[1] <- 1.00
glm_all_mvnm$lower[1] <- 1.00
glm_all_mvnm$upper[1] <- 1.00
glm_all_mvnm$ci[1] <- "1.00-1.00"
glm_all_mvnm$irr_ci <- paste(glm_all_mvnm$estimate, glm_all_mvnm$ci, sep =",")
glm_all_mvnm_table <- dplyr::select(glm_all_mvnm, names, irr_ci)
write_csv(glm_all_mvnm_table, "filepath")


## Forest plots -----------------------------------------------------------------------------------------

# FP of unadjusted and fully adjusted IRR
glm_all_mvnm_fp <- glm_all_mvnm[1:2,]
glm_all_mvnm_fp$names <- recode(glm_all_mvnm_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Migrant(adj)")
glm_adj_unadj <- full_join(glm_mig, glm_all_mvnm_fp, by = c("names" = "names", "estimate" = "estimate",
                                                            "lower" = "lower", "upper"= "upper",
                                                            "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci"))
sdi_glm_adj_unadj <- glm_adj_unadj
save(sdi_glm_adj_unadj, file = "filepath")


tabletext <- cbind(c("Non-migrants", "Migrants (unadjusted)", "Migrants (fully adjusted)"),
                   c(glm_adj_unadj$estimate),
                   c(glm_adj_unadj$ci))
dev.new()
png(file = "filepath", width = 600, height = 480)
unadj_irr_migrant_status <- forestplot(tabletext, mean = glm_adj_unadj$estimate, lower=glm_adj_unadj$lower, upper=glm_adj_unadj$upper,
                                       graph.pos = (ncol(tabletext)-1),
                                       hrzl_lines = TRUE, xlab = "IRR", zero = 1, 
                                       col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                       xticks = 0:2,
                                       ci.vertices = TRUE,
                                       boxsize = 0.05,
                                       title = "sdipresc",
                                       graphwidth = unit(80, 'mm'))
dev.off()



## SENSITIVITY ANALYSES ----------------------------

## Certainty of migration --------------------------

## IR & univariable IRR - migcertainy ---


## IR by migcertainty - whole cohort
pyears_dp_overall <- aggregate(pyears_ey_pregmax ~ migcertainty, sdipresc_annual_counts_final_extra_preg_max, sum) 
pyears_dp_overall <- pyears_dp_overall %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_dp <- aggregate(sdipresc_n_pregmax ~ migcertainty, sdipresc_annual_counts_final_extra_preg_max, sum) 
IR_dp <- inner_join(sdipresccount_dp,pyears_dp_overall, by= c("migcertainty" = "migcertainty"))
IR_dp_output <- pois.exact(IR_dp$sdipresc_n_pregmax, IR_dp$pyears_ey_pregmax, conf.level=0.95)
IR_dp_output <- IR_dp_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_dp_output <- IR_dp_output %>%
  mutate(lower100000=lower*100000)
IR_dp_output <- IR_dp_output %>%
  mutate(upper100000=upper*100000)
migcertainty <- IR_dp[,1]
IR_dp_output <- cbind(IR_dp_output, migcertainty)
IR_dp_output <- IR_dp_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_dp_output <- IR_dp_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_dp_output$ci <- paste(IR_dp_output$lower100000, IR_dp_output$upper100000, sep ="-")
IR_dp_output$ir_ci <- paste(IR_dp_output$incidence_rate100000, IR_dp_output$ci, sep =",")
IR_dp_output_table <-  dplyr::select(IR_dp_output, migcertainty, events, person_years, ir_ci)


##  generate IRR by negative binomial regression
sdipresc_annual_counts_final_extra_preg_max$migcertainty <- relevel(sdipresc_annual_counts_final_extra_preg_max$migcertainty, "Non-migrant")
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migcertainty) + offset(log(pyears_ey_pregmax)), data = sdipresc_annual_counts_final_extra_preg_max)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_dp <- cbind(names,estimate,confint.default,p)
glm_dp <- as_tibble(glm_IRR_dp)
glm_dp$names <- factor(glm_dp$names, c("(Intercept)" , 
                                       "as.factor(migcertainty)Definite", 
                                       "as.factor(migcertainty)Probable"))
glm_dp$names <- recode(glm_dp$names, "(Intercept)" = "Non-migrant" ,
                       "as.factor(migcertainty)Definite" = "Definite", 
                       "as.factor(migcertainty)Probable" = "Probable")
glm_dp$estimate <- as.numeric(glm_dp$estimate)
glm_dp <- glm_dp %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_dp$upper <- as.numeric(glm_dp$upper)
glm_dp$lower <- as.numeric(glm_dp$lower)
glm_dp$p <- as.numeric(glm_dp$p)
glm_dp <- glm_dp %>% mutate(across(is.numeric, ~ round(.,2)))
glm_dp$ci <- paste(glm_dp$lower, glm_dp$upper, sep ="-")
glm_dp$estimate[1] <- 1.00
glm_dp$lower[1] <- 1.00
glm_dp$upper[1] <- 1.00
glm_dp$ci[1] <- "1.00-1.00"
glm_dp$irr_ci <- paste(glm_dp$estimate, glm_dp$ci, sep =",")
glm_dp_table <- dplyr::select(glm_dp,names, irr_ci ) 

## join glm + IR
univariable_migcertainty <- full_join(IR_dp_output_table, glm_dp_table, by = c("migcertainty" = "names"))
write_csv(univariable_migcertainty, "filepath")

## Multivariable IRR controlling for year + agecat + imd + prac_region---

## Generate IRR via negative binomial regression
sdipresc_annual_counts_final_extra_preg_max$migcertainty <- relevel(sdipresc_annual_counts_final_extra_preg_max$migcertainty, "Non-migrant")
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migcertainty) + as.factor(eventyear) + as.factor(eventyear_agecat) +  as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey_pregmax)), 
            data = sdipresc_annual_counts_final_extra_preg_max)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_all_dp <- cbind(names,estimate,confint.default,p) 
glm_dp_all <- as_tibble(glm_IRR_all_dp)
glm_dp_all$names <- factor(glm_dp_all$names, c("(Intercept)" , 
                                               "as.factor(migcertainty)Definite", 
                                               "as.factor(migcertainty)Probable",
                                               "as.factor(eventyear)2010",
                                               "as.factor(eventyear)2011",
                                               "as.factor(eventyear)2012",
                                               "as.factor(eventyear)2013",
                                               "as.factor(eventyear)2014",
                                               "as.factor(eventyear)2015",
                                               "as.factor(eventyear)2016",
                                               "as.factor(eventyear)2017",
                                               "as.factor(eventyear)2018" ,
                                               "as.factor(eventyear_agecat)20-24",
                                               "as.factor(eventyear_agecat)25-29",
                                               "as.factor(eventyear_agecat)30-34",
                                               "as.factor(eventyear_agecat)35-39",
                                               "as.factor(eventyear_agecat)40-44",
                                               "as.factor(eventyear_agecat)45-49",
                                               "as.factor(imd)IMD 2",
                                               "as.factor(imd)IMD 3",
                                               "as.factor(imd)IMD 4",
                                               "as.factor(imd)IMD 5",
                                               "as.factor(prac_region)North East",
                                               "as.factor(prac_region)North West",
                                               "as.factor(prac_region)Yorkshire & The Humber",
                                               "as.factor(prac_region)East Midlands",
                                               "as.factor(prac_region)West Midlands",
                                               "as.factor(prac_region)East of England", 
                                               "as.factor(prac_region)South West",
                                               "as.factor(prac_region)South Central",
                                               "as.factor(prac_region)South East Coast",
                                               "as.factor(migcertainty)Definite",
                                               "as.factor(migcertainty)Probable"))
glm_dp_all$names <- recode(glm_dp_all$names,  "(Intercept)" = "Non-migrant" ,
                           "as.factor(migcertainty)Definite" = "Definite", 
                           "as.factor(migcertainty)Probable" = "Probable",
                           "as.factor(eventyear)2010" = "2010",
                           "as.factor(eventyear)2011" = "2011",
                           "as.factor(eventyear)2012" = "2012",
                           "as.factor(eventyear)2013" = "2013",
                           "as.factor(eventyear)2014" = "2014",
                           "as.factor(eventyear)2015" = "2015",
                           "as.factor(eventyear)2016" = "2016",
                           "as.factor(eventyear)2017" = "2017",
                           "as.factor(eventyear)2018" = "2018",
                           "as.factor(eventyear_agecat)20-24" = "20-24 years old",
                           "as.factor(eventyear_agecat)25-29" = "25-29 years old",
                           "as.factor(eventyear_agecat)30-34" = "30-34 years old",
                           "as.factor(eventyear_agecat)35-39" = "35-39 years old",
                           "as.factor(eventyear_agecat)40-44" = "40-44 years old",
                           "as.factor(eventyear_agecat)45-49" = "45-49 years old",
                           "as.factor(imd)IMD 2" = "IMD 2",
                           "as.factor(imd)IMD 3" = "IMD 3",
                           "as.factor(imd)IMD 4" = "IMD 4",
                           "as.factor(imd)IMD 5" = "IMD 5",
                           "as.factor(prac_region)North East" = "North East",
                           "as.factor(prac_region)North West" = "North West",
                           "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                           "as.factor(prac_region)East Midlands" = "East Midlands",
                           "as.factor(prac_region)West Midlands" = "West Midlands",
                           "as.factor(prac_region)East of England" = "East of England", 
                           "as.factor(prac_region)South West" = "South West",
                           "as.factor(prac_region)South Central" = "South Central",
                           "as.factor(prac_region)South East Coast" = "South East Coast",
                           "as.factor(migcertainty)Definite" = "Definite Migrants", 
                           "as.factor(migcertainty)Probable" = "Probable Migrants")
glm_dp_all$estimate <- as.numeric(glm_dp_all$estimate)
glm_dp_all <- glm_dp_all %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_dp_all$lower <- as.numeric(glm_dp_all$lower)
glm_dp_all$upper <- as.numeric(glm_dp_all$upper)
glm_dp_all$p <- as.numeric(glm_dp_all$p)
glm_dp_all <- glm_dp_all %>% mutate(across(is.numeric, ~ round(.,2)))
glm_dp_all$ci <- paste(glm_dp_all$lower, glm_dp_all$upper, sep ="-")
glm_dp_all$estimate[1] <- 1.00
glm_dp_all$lower[1] <- 1.00
glm_dp_all$upper[1] <- 1.00
glm_dp_all$ci[1] <- "1.00-1.00"
glm_dp_all$irr_ci <- paste(glm_dp_all$estimate, glm_dp_all$ci, sep =",")
glm_dp_all_table <- dplyr::select(glm_dp_all, names, irr_ci)
write_csv(glm_dp_all_table, "filepath")


# FP of adjusted and fully unadjusted IRR
glm_all_dp_fp <- glm_dp_all[1:3,]
glm_all_dp_fp$names <- recode(glm_all_dp_fp$names, "Non-migrants" = "Non-migrant", "Definite" = "Definite Migrants(adj)",
                              "Probable" = "Probable Migrants(adj)")
glm_dp_adj_unadj <- full_join(glm_dp, glm_all_dp_fp, by = c("names" = "names", "estimate" = "estimate",
                                                            "lower" = "lower", "upper"= "upper",
                                                            "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci"))
sdi_glm_dp_adj_unadj <- glm_dp_adj_unadj
save(sdi_glm_dp_adj_unadj, file = "filepath")


tabletext <- cbind(c("Non-migrants", "Definite Migrants (unadjusted)", "Probable Migrants (unadjusted)",
                     "Definite Migrants (fully adjusted)", "Probable Migrants (fully adjusted)"),
                   c(glm_dp_adj_unadj$estimate),
                   c(glm_dp_adj_unadj$ci))
dev.new()
png(file = "filepath", width = 600, height = 480)
irrs_migcertainty <- forestplot(tabletext, mean = glm_dp_adj_unadj$estimate, lower=glm_dp_adj_unadj$lower, upper=glm_dp_adj_unadj$upper,
                                graph.pos = (ncol(tabletext)-1),
                                hrzl_lines = TRUE, xlab = "IRR", zero = 1, 
                                col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                xticks = 0:2,
                                ci.vertices = TRUE,
                                boxsize = 0.05,
                                title = "sdipresc",
                                graphwidth = unit(80, 'mm'))
dev.off()



## Ethnicity -------------------


## white_british  ---

white_british <- sdipresc_annual_counts_final_extra_preg_max %>% filter(ethnicat6 == "White British")
white_british_pats <-  white_british %>% distinct(patid, .keep_all = TRUE) 
white_british_totals <- white_british_pats %>% group_by(migrant_status) %>% tally()


# IR by migrant status 
pyears_mvnm_white_british <- aggregate(pyears_ey_pregmax ~ migrant_status, white_british, sum) 
pyears_mvnm_white_british <- pyears_mvnm_white_british %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_mvnm_white_british<- aggregate(sdipresc_n_pregmax ~ migrant_status, white_british, sum) 
IR_mvnm_white_british <- inner_join(sdipresccount_mvnm_white_british,pyears_mvnm_white_british, by= c("migrant_status" = "migrant_status"))
IR_mvnm_white_british_output <- pois.exact(IR_mvnm_white_british$sdipresc_n_pregmax, IR_mvnm_white_british$pyears_ey_pregmax, conf.level=0.95)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>%
  mutate(lower100000=lower*100000)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>%
  mutate(upper100000=upper*100000)
migrant_status <- IR_mvnm_white_british[,1]
IR_mvnm_white_british_output <- cbind(IR_mvnm_white_british_output, migrant_status)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_white_british_output$ci <- paste(IR_mvnm_white_british_output$lower100000, IR_mvnm_white_british_output$upper100000, sep ="-")
IR_mvnm_white_british_output$ir_ci <- paste(IR_mvnm_white_british_output$incidence_rate100000, IR_mvnm_white_british_output$ci, sep =",")
IR_mvnm_white_british_output_table <-  dplyr::select(IR_mvnm_white_british_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + offset(log(pyears_ey_pregmax)), data = white_british)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_white_british <- cbind(names,estimate,confint.default,p)
glm_mig_white_british <- as_tibble(glm_IRR_mvnm_white_british)
glm_mig_white_british$names <- factor(glm_mig_white_british$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant"))
glm_mig_white_british$names <- recode(glm_mig_white_british$names, "(Intercept)" = "Non-migrant" ,"as.factor(migrant_status)Migrant" = "Migrant")
glm_mig_white_british$estimate <- as.numeric(glm_mig_white_british$estimate)
glm_mig_white_british <- glm_mig_white_british %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mig_white_british$upper <- as.numeric(glm_mig_white_british$upper)
glm_mig_white_british$lower <- as.numeric(glm_mig_white_british$lower)
glm_mig_white_british$p <- as.numeric(glm_mig_white_british$p)
glm_mig_white_british <- glm_mig_white_british %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mig_white_british$ci <- paste(glm_mig_white_british$lower, glm_mig_white_british$upper, sep ="-")
glm_mig_white_british$estimate[1] <- 1.00
glm_mig_white_british$lower[1] <- 1.00
glm_mig_white_british$upper[1] <- 1.00
glm_mig_white_british$ci[1] <- "1.00-1.00"
glm_mig_white_british$irr_ci <- paste(glm_mig_white_british$estimate, glm_mig_white_british$ci, sep =",")
glm_mig_white_british_table <- dplyr::select(glm_mig_white_british,names, irr_ci ) 



## join glm + IR + univariable_mig 
univariable_mig_white_british <- full_join(IR_mvnm_white_british_output_table, glm_mig_white_british_table, by = c("migrant_status" = "names"))
write_csv(univariable_mig_white_british, "filepath")

## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey_pregmax)) , 
            data = white_british)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_white_british <- cbind(names,estimate,confint.default,p)
glm_mvnm_white_british <- as_tibble(glm_IRR_mvnm_white_british)
glm_mvnm_white_british$names <- factor(glm_mvnm_white_british$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant", 
                                                                       "as.factor(eventyear)2010",
                                                                       "as.factor(eventyear)2011",
                                                                       "as.factor(eventyear)2012",
                                                                       "as.factor(eventyear)2013",
                                                                       "as.factor(eventyear)2014",
                                                                       "as.factor(eventyear)2015",
                                                                       "as.factor(eventyear)2016",
                                                                       "as.factor(eventyear)2017",
                                                                       "as.factor(eventyear)2018" ,
                                                                       "as.factor(eventyear_agecat)20-24",
                                                                       "as.factor(eventyear_agecat)25-29",
                                                                       "as.factor(eventyear_agecat)30-34",
                                                                       "as.factor(eventyear_agecat)35-39",
                                                                       "as.factor(eventyear_agecat)40-44",
                                                                       "as.factor(eventyear_agecat)45-49",
                                                                       "as.factor(imd)IMD 2",
                                                                       "as.factor(imd)IMD 3",
                                                                       "as.factor(imd)IMD 4",
                                                                       "as.factor(imd)IMD 5",
                                                                       "as.factor(prac_region)North East",
                                                                       "as.factor(prac_region)North West",
                                                                       "as.factor(prac_region)Yorkshire & The Humber",
                                                                       "as.factor(prac_region)East Midlands",
                                                                       "as.factor(prac_region)West Midlands",
                                                                       "as.factor(prac_region)East of England", 
                                                                       "as.factor(prac_region)South West",
                                                                       "as.factor(prac_region)South Central",
                                                                       "as.factor(prac_region)South East Coast"))
glm_mvnm_white_british$names <- recode(glm_mvnm_white_british$names,"(Intercept)" = "Non-migrant" ,
                                       "as.factor(migrant_status)Migrant" = "Migrant",
                                       "as.factor(eventyear)2010" = "2010",
                                       "as.factor(eventyear)2011" = "2011",
                                       "as.factor(eventyear)2012" = "2012",
                                       "as.factor(eventyear)2013" = "2013",
                                       "as.factor(eventyear)2014" = "2014",
                                       "as.factor(eventyear)2015" = "2015",
                                       "as.factor(eventyear)2016" = "2016",
                                       "as.factor(eventyear)2017" = "2017",
                                       "as.factor(eventyear)2018" = "2018",
                                       "as.factor(eventyear_agecat)20-24" = "20-24",
                                       "as.factor(eventyear_agecat)25-29" = "25-29",
                                       "as.factor(eventyear_agecat)30-34" = "30-34",
                                       "as.factor(eventyear_agecat)35-39" = "35-39",
                                       "as.factor(eventyear_agecat)40-44" = "40-44",
                                       "as.factor(eventyear_agecat)45-49" = "45-49",
                                       "as.factor(imd)IMD 2" = "IMD 2",
                                       "as.factor(imd)IMD 3" = "IMD 3",
                                       "as.factor(imd)IMD 4" = "IMD 4",
                                       "as.factor(imd)IMD 5" = "IMD 5",
                                       "as.factor(prac_region)North East" = "North East",
                                       "as.factor(prac_region)North West" = "North West",
                                       "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                                       "as.factor(prac_region)East Midlands" = "East Midlands",
                                       "as.factor(prac_region)West Midlands" = "West Midlands",
                                       "as.factor(prac_region)East of England" = "East of England", 
                                       "as.factor(prac_region)South West" = "South West",
                                       "as.factor(prac_region)South Central" = "South Central",
                                       "as.factor(prac_region)South East Coast" = "South East Coast")
glm_mvnm_white_british$estimate <- as.numeric(glm_mvnm_white_british$estimate)
glm_mvnm_white_british <- glm_mvnm_white_british %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mvnm_white_british$lower <- as.numeric(glm_mvnm_white_british$lower)
glm_mvnm_white_british$upper <- as.numeric(glm_mvnm_white_british$upper)
glm_mvnm_white_british$p <- as.numeric(glm_mvnm_white_british$p)
glm_mvnm_white_british <- glm_mvnm_white_british %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mvnm_white_british$ci <- paste(glm_mvnm_white_british$lower, glm_mvnm_white_british$upper, sep ="-")
glm_mvnm_white_british$estimate[1] <- 1.00
glm_mvnm_white_british$lower[1] <- 1.00
glm_mvnm_white_british$upper[1] <- 1.00
glm_mvnm_white_british$ci[1] <- "1.00-1.00"
glm_mvnm_white_british$irr_ci <- paste(glm_mvnm_white_british$estimate, glm_mvnm_white_british$ci, sep =",")
glm_mvnm_white_british_table <- dplyr::select(glm_mvnm_white_british, names, irr_ci)
write_csv(glm_mvnm_white_british_table, "filepath")

## white_nonbritish  ---

white_nonbritish <- sdipresc_annual_counts_final_extra_preg_max %>% filter(ethnicat6 == "White Non-British")
white_nonbritish_pats <-  white_nonbritish %>% distinct(patid, .keep_all = TRUE) 
white_nonbritish_totals <- white_nonbritish_pats %>% group_by(migrant_status) %>% tally()

# IR by migrant status 
pyears_mvnm_white_nonbritish <- aggregate(pyears_ey_pregmax ~ migrant_status, white_nonbritish, sum) 
pyears_mvnm_white_nonbritish <- pyears_mvnm_white_nonbritish %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_mvnm_white_nonbritish<- aggregate(sdipresc_n_pregmax ~ migrant_status, white_nonbritish, sum) 
IR_mvnm_white_nonbritish <- inner_join(sdipresccount_mvnm_white_nonbritish,pyears_mvnm_white_nonbritish, by= c("migrant_status" = "migrant_status"))
IR_mvnm_white_nonbritish_output <- pois.exact(IR_mvnm_white_nonbritish$sdipresc_n_pregmax, IR_mvnm_white_nonbritish$pyears_ey_pregmax, conf.level=0.95)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>%
  mutate(lower100000=lower*100000)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>%
  mutate(upper100000=upper*100000)
migrant_status <- IR_mvnm_white_nonbritish[,1]
IR_mvnm_white_nonbritish_output <- cbind(IR_mvnm_white_nonbritish_output, migrant_status)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_white_nonbritish_output$ci <- paste(IR_mvnm_white_nonbritish_output$lower100000, IR_mvnm_white_nonbritish_output$upper100000, sep ="-")
IR_mvnm_white_nonbritish_output$ir_ci <- paste(IR_mvnm_white_nonbritish_output$incidence_rate100000, IR_mvnm_white_nonbritish_output$ci, sep =",")
IR_mvnm_white_nonbritish_output_table <-  dplyr::select(IR_mvnm_white_nonbritish_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + offset(log(pyears_ey_pregmax)), data = white_nonbritish)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_white_nonbritish <- cbind(names,estimate,confint.default,p)
glm_mig_white_nonbritish <- as_tibble(glm_IRR_mvnm_white_nonbritish)
glm_mig_white_nonbritish$names <- factor(glm_mig_white_nonbritish$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant"))
glm_mig_white_nonbritish$names <- recode(glm_mig_white_nonbritish$names, "(Intercept)" = "Non-migrant" ,"as.factor(migrant_status)Migrant" = "Migrant")
glm_mig_white_nonbritish$estimate <- as.numeric(glm_mig_white_nonbritish$estimate)
glm_mig_white_nonbritish <- glm_mig_white_nonbritish %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mig_white_nonbritish$upper <- as.numeric(glm_mig_white_nonbritish$upper)
glm_mig_white_nonbritish$lower <- as.numeric(glm_mig_white_nonbritish$lower)
glm_mig_white_nonbritish$p <- as.numeric(glm_mig_white_nonbritish$p)
glm_mig_white_nonbritish <- glm_mig_white_nonbritish %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mig_white_nonbritish$ci <- paste(glm_mig_white_nonbritish$lower, glm_mig_white_nonbritish$upper, sep ="-")
glm_mig_white_nonbritish$estimate[1] <- 1.00
glm_mig_white_nonbritish$lower[1] <- 1.00
glm_mig_white_nonbritish$upper[1] <- 1.00
glm_mig_white_nonbritish$ci[1] <- "1.00-1.00"
glm_mig_white_nonbritish$irr_ci <- paste(glm_mig_white_nonbritish$estimate, glm_mig_white_nonbritish$ci, sep =",")
glm_mig_white_nonbritish_table <- dplyr::select(glm_mig_white_nonbritish,names, irr_ci ) 



## join glm + IR + univariable_mig 
univariable_mig_white_nonbritish <- full_join(IR_mvnm_white_nonbritish_output_table, glm_mig_white_nonbritish_table, by = c("migrant_status" = "names"))
write_csv(univariable_mig_white_nonbritish, "filepath")

## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey_pregmax)) , 
            data = white_nonbritish)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_white_nonbritish <- cbind(names,estimate,confint.default,p)
glm_mvnm_white_nonbritish <- as_tibble(glm_IRR_mvnm_white_nonbritish)
glm_mvnm_white_nonbritish$names <- factor(glm_mvnm_white_nonbritish$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant", 
                                                                             "as.factor(eventyear)2010",
                                                                             "as.factor(eventyear)2011",
                                                                             "as.factor(eventyear)2012",
                                                                             "as.factor(eventyear)2013",
                                                                             "as.factor(eventyear)2014",
                                                                             "as.factor(eventyear)2015",
                                                                             "as.factor(eventyear)2016",
                                                                             "as.factor(eventyear)2017",
                                                                             "as.factor(eventyear)2018" ,
                                                                             "as.factor(eventyear_agecat)20-24",
                                                                             "as.factor(eventyear_agecat)25-29",
                                                                             "as.factor(eventyear_agecat)30-34",
                                                                             "as.factor(eventyear_agecat)35-39",
                                                                             "as.factor(eventyear_agecat)40-44",
                                                                             "as.factor(eventyear_agecat)45-49",
                                                                             "as.factor(imd)IMD 2",
                                                                             "as.factor(imd)IMD 3",
                                                                             "as.factor(imd)IMD 4",
                                                                             "as.factor(imd)IMD 5",
                                                                             "as.factor(prac_region)North East",
                                                                             "as.factor(prac_region)North West",
                                                                             "as.factor(prac_region)Yorkshire & The Humber",
                                                                             "as.factor(prac_region)East Midlands",
                                                                             "as.factor(prac_region)West Midlands",
                                                                             "as.factor(prac_region)East of England", 
                                                                             "as.factor(prac_region)South West",
                                                                             "as.factor(prac_region)South Central",
                                                                             "as.factor(prac_region)South East Coast"))
glm_mvnm_white_nonbritish$names <- recode(glm_mvnm_white_nonbritish$names,"(Intercept)" = "Non-migrant" ,
                                          "as.factor(migrant_status)Migrant" = "Migrant",
                                          "as.factor(eventyear)2010" = "2010",
                                          "as.factor(eventyear)2011" = "2011",
                                          "as.factor(eventyear)2012" = "2012",
                                          "as.factor(eventyear)2013" = "2013",
                                          "as.factor(eventyear)2014" = "2014",
                                          "as.factor(eventyear)2015" = "2015",
                                          "as.factor(eventyear)2016" = "2016",
                                          "as.factor(eventyear)2017" = "2017",
                                          "as.factor(eventyear)2018" = "2018",
                                          "as.factor(eventyear_agecat)20-24" = "20-24",
                                          "as.factor(eventyear_agecat)25-29" = "25-29",
                                          "as.factor(eventyear_agecat)30-34" = "30-34",
                                          "as.factor(eventyear_agecat)35-39" = "35-39",
                                          "as.factor(eventyear_agecat)40-44" = "40-44",
                                          "as.factor(eventyear_agecat)45-49" = "45-49",
                                          "as.factor(imd)IMD 2" = "IMD 2",
                                          "as.factor(imd)IMD 3" = "IMD 3",
                                          "as.factor(imd)IMD 4" = "IMD 4",
                                          "as.factor(imd)IMD 5" = "IMD 5",
                                          "as.factor(prac_region)North East" = "North East",
                                          "as.factor(prac_region)North West" = "North West",
                                          "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                                          "as.factor(prac_region)East Midlands" = "East Midlands",
                                          "as.factor(prac_region)West Midlands" = "West Midlands",
                                          "as.factor(prac_region)East of England" = "East of England", 
                                          "as.factor(prac_region)South West" = "South West",
                                          "as.factor(prac_region)South Central" = "South Central",
                                          "as.factor(prac_region)South East Coast" = "South East Coast")
glm_mvnm_white_nonbritish$estimate <- as.numeric(glm_mvnm_white_nonbritish$estimate)
glm_mvnm_white_nonbritish <- glm_mvnm_white_nonbritish %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mvnm_white_nonbritish$lower <- as.numeric(glm_mvnm_white_nonbritish$lower)
glm_mvnm_white_nonbritish$upper <- as.numeric(glm_mvnm_white_nonbritish$upper)
glm_mvnm_white_nonbritish$p <- as.numeric(glm_mvnm_white_nonbritish$p)
glm_mvnm_white_nonbritish <- glm_mvnm_white_nonbritish %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mvnm_white_nonbritish$ci <- paste(glm_mvnm_white_nonbritish$lower, glm_mvnm_white_nonbritish$upper, sep ="-")
glm_mvnm_white_nonbritish$estimate[1] <- 1.00
glm_mvnm_white_nonbritish$lower[1] <- 1.00
glm_mvnm_white_nonbritish$upper[1] <- 1.00
glm_mvnm_white_nonbritish$ci[1] <- "1.00-1.00"
glm_mvnm_white_nonbritish$irr_ci <- paste(glm_mvnm_white_nonbritish$estimate, glm_mvnm_white_nonbritish$ci, sep =",")
glm_mvnm_white_nonbritish_table <- dplyr::select(glm_mvnm_white_nonbritish, names, irr_ci)
write_csv(glm_mvnm_white_nonbritish_table, "filepath")


## mixed ---

mixed <- sdipresc_annual_counts_final_extra_preg_max %>% filter(ethnicat6 == "Mixed")
mixed_pats <-  mixed %>% distinct(patid, .keep_all = TRUE) 
mixed_totals <- mixed_pats %>% group_by(migrant_status) %>% tally()


# IR by migrant status 
pyears_mvnm_mixed <- aggregate(pyears_ey_pregmax ~ migrant_status, mixed, sum) 
pyears_mvnm_mixed <- pyears_mvnm_mixed %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_mvnm_mixed<- aggregate(sdipresc_n_pregmax ~ migrant_status, mixed, sum) 
IR_mvnm_mixed <- inner_join(sdipresccount_mvnm_mixed,pyears_mvnm_mixed, by= c("migrant_status" = "migrant_status"))
IR_mvnm_mixed_output <- pois.exact(IR_mvnm_mixed$sdipresc_n_pregmax, IR_mvnm_mixed$pyears_ey_pregmax, conf.level=0.95)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>%
  mutate(lower100000=lower*100000)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>%
  mutate(upper100000=upper*100000)
migrant_status <- IR_mvnm_mixed[,1]
IR_mvnm_mixed_output <- cbind(IR_mvnm_mixed_output, migrant_status)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_mixed_output$ci <- paste(IR_mvnm_mixed_output$lower100000, IR_mvnm_mixed_output$upper100000, sep ="-")
IR_mvnm_mixed_output$ir_ci <- paste(IR_mvnm_mixed_output$incidence_rate100000, IR_mvnm_mixed_output$ci, sep =",")
IR_mvnm_mixed_output_table <-  dplyr::select(IR_mvnm_mixed_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + offset(log(pyears_ey_pregmax)), data = mixed)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_mixed <- cbind(names,estimate,confint.default,p)
glm_mig_mixed <- as_tibble(glm_IRR_mvnm_mixed)
glm_mig_mixed$names <- factor(glm_mig_mixed$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant"))
glm_mig_mixed$names <- recode(glm_mig_mixed$names, "(Intercept)" = "Non-migrant" ,"as.factor(migrant_status)Migrant" = "Migrant")
glm_mig_mixed$estimate <- as.numeric(glm_mig_mixed$estimate)
glm_mig_mixed <- glm_mig_mixed %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mig_mixed$upper <- as.numeric(glm_mig_mixed$upper)
glm_mig_mixed$lower <- as.numeric(glm_mig_mixed$lower)
glm_mig_mixed$p <- as.numeric(glm_mig_mixed$p)
glm_mig_mixed <- glm_mig_mixed %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mig_mixed$ci <- paste(glm_mig_mixed$lower, glm_mig_mixed$upper, sep ="-")
glm_mig_mixed$estimate[1] <- 1.00
glm_mig_mixed$lower[1] <- 1.00
glm_mig_mixed$upper[1] <- 1.00
glm_mig_mixed$ci[1] <- "1.00-1.00"
glm_mig_mixed$irr_ci <- paste(glm_mig_mixed$estimate, glm_mig_mixed$ci, sep =",")
glm_mig_mixed_table <- dplyr::select(glm_mig_mixed,names, irr_ci ) 

## join glm + IR + univariable_mig 
univariable_mig_mixed <- full_join(IR_mvnm_mixed_output_table, glm_mig_mixed_table, by = c("migrant_status" = "names"))
write_csv(univariable_mig_mixed, "filepath")

## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey_pregmax)) , 
            data = mixed)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_mixed <- cbind(names,estimate,confint.default,p)
glm_mvnm_mixed <- as_tibble(glm_IRR_mvnm_mixed)
glm_mvnm_mixed$names <- factor(glm_mvnm_mixed$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant", 
                                                       "as.factor(eventyear)2010",
                                                       "as.factor(eventyear)2011",
                                                       "as.factor(eventyear)2012",
                                                       "as.factor(eventyear)2013",
                                                       "as.factor(eventyear)2014",
                                                       "as.factor(eventyear)2015",
                                                       "as.factor(eventyear)2016",
                                                       "as.factor(eventyear)2017",
                                                       "as.factor(eventyear)2018" ,
                                                       "as.factor(eventyear_agecat)20-24",
                                                       "as.factor(eventyear_agecat)25-29",
                                                       "as.factor(eventyear_agecat)30-34",
                                                       "as.factor(eventyear_agecat)35-39",
                                                       "as.factor(eventyear_agecat)40-44",
                                                       "as.factor(eventyear_agecat)45-49",
                                                       "as.factor(imd)IMD 2",
                                                       "as.factor(imd)IMD 3",
                                                       "as.factor(imd)IMD 4",
                                                       "as.factor(imd)IMD 5",
                                                       "as.factor(prac_region)North East",
                                                       "as.factor(prac_region)North West",
                                                       "as.factor(prac_region)Yorkshire & The Humber",
                                                       "as.factor(prac_region)East Midlands",
                                                       "as.factor(prac_region)West Midlands",
                                                       "as.factor(prac_region)East of England", 
                                                       "as.factor(prac_region)South West",
                                                       "as.factor(prac_region)South Central",
                                                       "as.factor(prac_region)South East Coast"))
glm_mvnm_mixed$names <- recode(glm_mvnm_mixed$names,"(Intercept)" = "Non-migrant" ,
                               "as.factor(migrant_status)Migrant" = "Migrant",
                               "as.factor(eventyear)2010" = "2010",
                               "as.factor(eventyear)2011" = "2011",
                               "as.factor(eventyear)2012" = "2012",
                               "as.factor(eventyear)2013" = "2013",
                               "as.factor(eventyear)2014" = "2014",
                               "as.factor(eventyear)2015" = "2015",
                               "as.factor(eventyear)2016" = "2016",
                               "as.factor(eventyear)2017" = "2017",
                               "as.factor(eventyear)2018" = "2018",
                               "as.factor(eventyear_agecat)20-24" = "20-24",
                               "as.factor(eventyear_agecat)25-29" = "25-29",
                               "as.factor(eventyear_agecat)30-34" = "30-34",
                               "as.factor(eventyear_agecat)35-39" = "35-39",
                               "as.factor(eventyear_agecat)40-44" = "40-44",
                               "as.factor(eventyear_agecat)45-49" = "45-49",
                               "as.factor(imd)IMD 2" = "IMD 2",
                               "as.factor(imd)IMD 3" = "IMD 3",
                               "as.factor(imd)IMD 4" = "IMD 4",
                               "as.factor(imd)IMD 5" = "IMD 5",
                               "as.factor(prac_region)North East" = "North East",
                               "as.factor(prac_region)North West" = "North West",
                               "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                               "as.factor(prac_region)East Midlands" = "East Midlands",
                               "as.factor(prac_region)West Midlands" = "West Midlands",
                               "as.factor(prac_region)East of England" = "East of England", 
                               "as.factor(prac_region)South West" = "South West",
                               "as.factor(prac_region)South Central" = "South Central",
                               "as.factor(prac_region)South East Coast" = "South East Coast")
glm_mvnm_mixed$estimate <- as.numeric(glm_mvnm_mixed$estimate)
glm_mvnm_mixed <- glm_mvnm_mixed %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mvnm_mixed$lower <- as.numeric(glm_mvnm_mixed$lower)
glm_mvnm_mixed$upper <- as.numeric(glm_mvnm_mixed$upper)
glm_mvnm_mixed$p <- as.numeric(glm_mvnm_mixed$p)
glm_mvnm_mixed <- glm_mvnm_mixed %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mvnm_mixed$ci <- paste(glm_mvnm_mixed$lower, glm_mvnm_mixed$upper, sep ="-")
glm_mvnm_mixed$estimate[1] <- 1.00
glm_mvnm_mixed$lower[1] <- 1.00
glm_mvnm_mixed$upper[1] <- 1.00
glm_mvnm_mixed$ci[1] <- "1.00-1.00"
glm_mvnm_mixed$irr_ci <- paste(glm_mvnm_mixed$estimate, glm_mvnm_mixed$ci, sep =",")
glm_mvnm_mixed_table <- dplyr::select(glm_mvnm_mixed, names, irr_ci)
write_csv(glm_mvnm_mixed_table, "filepath")


## asian ---

asian <- sdipresc_annual_counts_final_extra_preg_max %>% filter(ethnicat6 == "Asian/Asian British")
asian_pats <-  asian %>% distinct(patid, .keep_all = TRUE) 
asian_totals <- asian_pats %>% group_by(migrant_status) %>% tally()


# IR by migrant status 
pyears_mvnm_asian <- aggregate(pyears_ey_pregmax ~ migrant_status, asian, sum) 
pyears_mvnm_asian <- pyears_mvnm_asian %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_mvnm_asian<- aggregate(sdipresc_n_pregmax ~ migrant_status, asian, sum) 
IR_mvnm_asian <- inner_join(sdipresccount_mvnm_asian,pyears_mvnm_asian, by= c("migrant_status" = "migrant_status"))
IR_mvnm_asian_output <- pois.exact(IR_mvnm_asian$sdipresc_n_pregmax, IR_mvnm_asian$pyears_ey_pregmax, conf.level=0.95)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>%
  mutate(lower100000=lower*100000)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>%
  mutate(upper100000=upper*100000)
migrant_status <- IR_mvnm_asian[,1]
IR_mvnm_asian_output <- cbind(IR_mvnm_asian_output, migrant_status)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_asian_output <- IR_mvnm_asian_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_asian_output$ci <- paste(IR_mvnm_asian_output$lower100000, IR_mvnm_asian_output$upper100000, sep ="-")
IR_mvnm_asian_output$ir_ci <- paste(IR_mvnm_asian_output$incidence_rate100000, IR_mvnm_asian_output$ci, sep =",")
IR_mvnm_asian_output_table <-  dplyr::select(IR_mvnm_asian_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + offset(log(pyears_ey_pregmax)), data = asian)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_asian <- cbind(names,estimate,confint.default,p)
glm_mig_asian <- as_tibble(glm_IRR_mvnm_asian)
glm_mig_asian$names <- factor(glm_mig_asian$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant"))
glm_mig_asian$names <- recode(glm_mig_asian$names, "(Intercept)" = "Non-migrant" ,"as.factor(migrant_status)Migrant" = "Migrant")
glm_mig_asian$estimate <- as.numeric(glm_mig_asian$estimate)
glm_mig_asian <- glm_mig_asian %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mig_asian$upper <- as.numeric(glm_mig_asian$upper)
glm_mig_asian$lower <- as.numeric(glm_mig_asian$lower)
glm_mig_asian$p <- as.numeric(glm_mig_asian$p)
glm_mig_asian <- glm_mig_asian %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mig_asian$ci <- paste(glm_mig_asian$lower, glm_mig_asian$upper, sep ="-")
glm_mig_asian$estimate[1] <- 1.00
glm_mig_asian$lower[1] <- 1.00
glm_mig_asian$upper[1] <- 1.00
glm_mig_asian$ci[1] <- "1.00-1.00"
glm_mig_asian$irr_ci <- paste(glm_mig_asian$estimate, glm_mig_asian$ci, sep =",")
glm_mig_asian_table <- dplyr::select(glm_mig_asian,names, irr_ci ) 

## join glm + IR + univariable_mig 
univariable_mig_asian <- full_join(IR_mvnm_asian_output_table, glm_mig_asian_table, by = c("migrant_status" = "names"))
write_csv(univariable_mig_asian,"filepath")

## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey_pregmax)) , 
            data = asian)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_asian <- cbind(names,estimate,confint.default,p)
glm_mvnm_asian <- as_tibble(glm_IRR_mvnm_asian)
glm_mvnm_asian$names <- factor(glm_mvnm_asian$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant", 
                                                       "as.factor(eventyear)2010",
                                                       "as.factor(eventyear)2011",
                                                       "as.factor(eventyear)2012",
                                                       "as.factor(eventyear)2013",
                                                       "as.factor(eventyear)2014",
                                                       "as.factor(eventyear)2015",
                                                       "as.factor(eventyear)2016",
                                                       "as.factor(eventyear)2017",
                                                       "as.factor(eventyear)2018" ,
                                                       "as.factor(eventyear_agecat)20-24",
                                                       "as.factor(eventyear_agecat)25-29",
                                                       "as.factor(eventyear_agecat)30-34",
                                                       "as.factor(eventyear_agecat)35-39",
                                                       "as.factor(eventyear_agecat)40-44",
                                                       "as.factor(eventyear_agecat)45-49",
                                                       "as.factor(imd)IMD 2",
                                                       "as.factor(imd)IMD 3",
                                                       "as.factor(imd)IMD 4",
                                                       "as.factor(imd)IMD 5",
                                                       "as.factor(prac_region)North East",
                                                       "as.factor(prac_region)North West",
                                                       "as.factor(prac_region)Yorkshire & The Humber",
                                                       "as.factor(prac_region)East Midlands",
                                                       "as.factor(prac_region)West Midlands",
                                                       "as.factor(prac_region)East of England", 
                                                       "as.factor(prac_region)South West",
                                                       "as.factor(prac_region)South Central",
                                                       "as.factor(prac_region)South East Coast"))
glm_mvnm_asian$names <- recode(glm_mvnm_asian$names,"(Intercept)" = "Non-migrant" ,
                               "as.factor(migrant_status)Migrant" = "Migrant",
                               "as.factor(eventyear)2010" = "2010",
                               "as.factor(eventyear)2011" = "2011",
                               "as.factor(eventyear)2012" = "2012",
                               "as.factor(eventyear)2013" = "2013",
                               "as.factor(eventyear)2014" = "2014",
                               "as.factor(eventyear)2015" = "2015",
                               "as.factor(eventyear)2016" = "2016",
                               "as.factor(eventyear)2017" = "2017",
                               "as.factor(eventyear)2018" = "2018",
                               "as.factor(eventyear_agecat)20-24" = "20-24",
                               "as.factor(eventyear_agecat)25-29" = "25-29",
                               "as.factor(eventyear_agecat)30-34" = "30-34",
                               "as.factor(eventyear_agecat)35-39" = "35-39",
                               "as.factor(eventyear_agecat)40-44" = "40-44",
                               "as.factor(eventyear_agecat)45-49" = "45-49",
                               "as.factor(imd)IMD 2" = "IMD 2",
                               "as.factor(imd)IMD 3" = "IMD 3",
                               "as.factor(imd)IMD 4" = "IMD 4",
                               "as.factor(imd)IMD 5" = "IMD 5",
                               "as.factor(prac_region)North East" = "North East",
                               "as.factor(prac_region)North West" = "North West",
                               "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                               "as.factor(prac_region)East Midlands" = "East Midlands",
                               "as.factor(prac_region)West Midlands" = "West Midlands",
                               "as.factor(prac_region)East of England" = "East of England", 
                               "as.factor(prac_region)South West" = "South West",
                               "as.factor(prac_region)South Central" = "South Central",
                               "as.factor(prac_region)South East Coast" = "South East Coast")
glm_mvnm_asian$estimate <- as.numeric(glm_mvnm_asian$estimate)
glm_mvnm_asian <- glm_mvnm_asian %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mvnm_asian$lower <- as.numeric(glm_mvnm_asian$lower)
glm_mvnm_asian$upper <- as.numeric(glm_mvnm_asian$upper)
glm_mvnm_asian$p <- as.numeric(glm_mvnm_asian$p)
glm_mvnm_asian <- glm_mvnm_asian %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mvnm_asian$ci <- paste(glm_mvnm_asian$lower, glm_mvnm_asian$upper, sep ="-")
glm_mvnm_asian$estimate[1] <- 1.00
glm_mvnm_asian$lower[1] <- 1.00
glm_mvnm_asian$upper[1] <- 1.00
glm_mvnm_asian$ci[1] <- "1.00-1.00"
glm_mvnm_asian$irr_ci <- paste(glm_mvnm_asian$estimate, glm_mvnm_asian$ci, sep =",")
glm_mvnm_asian_table <- dplyr::select(glm_mvnm_asian, names, irr_ci)
write_csv(glm_mvnm_asian_table,"filepath")



## black ---

black <- sdipresc_annual_counts_final_extra_preg_max %>% filter(ethnicat6 == "Black/Black British")
black_pats <-  black %>% distinct(patid, .keep_all = TRUE) 
black_totals <- black_pats %>% group_by(migrant_status) %>% tally()

# IR by migrant status 
pyears_mvnm_black <- aggregate(pyears_ey_pregmax ~ migrant_status, black, sum) 
pyears_mvnm_black <- pyears_mvnm_black %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_mvnm_black<- aggregate(sdipresc_n_pregmax ~ migrant_status, black, sum) 
IR_mvnm_black <- inner_join(sdipresccount_mvnm_black,pyears_mvnm_black, by= c("migrant_status" = "migrant_status"))
IR_mvnm_black_output <- pois.exact(IR_mvnm_black$sdipresc_n_pregmax, IR_mvnm_black$pyears_ey_pregmax, conf.level=0.95)
IR_mvnm_black_output <- IR_mvnm_black_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_mvnm_black_output <- IR_mvnm_black_output %>%
  mutate(lower100000=lower*100000)
IR_mvnm_black_output <- IR_mvnm_black_output %>%
  mutate(upper100000=upper*100000)
migrant_status <- IR_mvnm_black[,1]
IR_mvnm_black_output <- cbind(IR_mvnm_black_output, migrant_status)
IR_mvnm_black_output <- IR_mvnm_black_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_black_output <- IR_mvnm_black_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_black_output$ci <- paste(IR_mvnm_black_output$lower100000, IR_mvnm_black_output$upper100000, sep ="-")
IR_mvnm_black_output$ir_ci <- paste(IR_mvnm_black_output$incidence_rate100000, IR_mvnm_black_output$ci, sep =",")
IR_mvnm_black_output_table <-  dplyr::select(IR_mvnm_black_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + offset(log(pyears_ey_pregmax)), data = black)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_black <- cbind(names,estimate,confint.default,p)
glm_mig_black <- as_tibble(glm_IRR_mvnm_black)
glm_mig_black$names <- factor(glm_mig_black$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant"))
glm_mig_black$names <- recode(glm_mig_black$names, "(Intercept)" = "Non-migrant" ,"as.factor(migrant_status)Migrant" = "Migrant")
glm_mig_black$estimate <- as.numeric(glm_mig_black$estimate)
glm_mig_black <- glm_mig_black %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mig_black$upper <- as.numeric(glm_mig_black$upper)
glm_mig_black$lower <- as.numeric(glm_mig_black$lower)
glm_mig_black$p <- as.numeric(glm_mig_black$p)
glm_mig_black <- glm_mig_black %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mig_black$ci <- paste(glm_mig_black$lower, glm_mig_black$upper, sep ="-")
glm_mig_black$estimate[1] <- 1.00
glm_mig_black$lower[1] <- 1.00
glm_mig_black$upper[1] <- 1.00
glm_mig_black$ci[1] <- "1.00-1.00"
glm_mig_black$irr_ci <- paste(glm_mig_black$estimate, glm_mig_black$ci, sep =",")
glm_mig_black_table <- dplyr::select(glm_mig_black,names, irr_ci ) 

## join glm + IR + univariable_mig 
univariable_mig_black <- full_join(IR_mvnm_black_output_table, glm_mig_black_table, by = c("migrant_status" = "names"))
write_csv(univariable_mig_black, "filepath")

## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey_pregmax)) , 
            data = black)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_black <- cbind(names,estimate,confint.default,p)
glm_mvnm_black <- as_tibble(glm_IRR_mvnm_black)
glm_mvnm_black$names <- factor(glm_mvnm_black$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant", 
                                                       "as.factor(eventyear)2010",
                                                       "as.factor(eventyear)2011",
                                                       "as.factor(eventyear)2012",
                                                       "as.factor(eventyear)2013",
                                                       "as.factor(eventyear)2014",
                                                       "as.factor(eventyear)2015",
                                                       "as.factor(eventyear)2016",
                                                       "as.factor(eventyear)2017",
                                                       "as.factor(eventyear)2018" ,
                                                       "as.factor(eventyear_agecat)20-24",
                                                       "as.factor(eventyear_agecat)25-29",
                                                       "as.factor(eventyear_agecat)30-34",
                                                       "as.factor(eventyear_agecat)35-39",
                                                       "as.factor(eventyear_agecat)40-44",
                                                       "as.factor(eventyear_agecat)45-49",
                                                       "as.factor(imd)IMD 2",
                                                       "as.factor(imd)IMD 3",
                                                       "as.factor(imd)IMD 4",
                                                       "as.factor(imd)IMD 5",
                                                       "as.factor(prac_region)North East",
                                                       "as.factor(prac_region)North West",
                                                       "as.factor(prac_region)Yorkshire & The Humber",
                                                       "as.factor(prac_region)East Midlands",
                                                       "as.factor(prac_region)West Midlands",
                                                       "as.factor(prac_region)East of England", 
                                                       "as.factor(prac_region)South West",
                                                       "as.factor(prac_region)South Central",
                                                       "as.factor(prac_region)South East Coast"))
glm_mvnm_black$names <- recode(glm_mvnm_black$names,"(Intercept)" = "Non-migrant" ,
                               "as.factor(migrant_status)Migrant" = "Migrant",
                               "as.factor(eventyear)2010" = "2010",
                               "as.factor(eventyear)2011" = "2011",
                               "as.factor(eventyear)2012" = "2012",
                               "as.factor(eventyear)2013" = "2013",
                               "as.factor(eventyear)2014" = "2014",
                               "as.factor(eventyear)2015" = "2015",
                               "as.factor(eventyear)2016" = "2016",
                               "as.factor(eventyear)2017" = "2017",
                               "as.factor(eventyear)2018" = "2018",
                               "as.factor(eventyear_agecat)20-24" = "20-24",
                               "as.factor(eventyear_agecat)25-29" = "25-29",
                               "as.factor(eventyear_agecat)30-34" = "30-34",
                               "as.factor(eventyear_agecat)35-39" = "35-39",
                               "as.factor(eventyear_agecat)40-44" = "40-44",
                               "as.factor(eventyear_agecat)45-49" = "45-49",
                               "as.factor(imd)IMD 2" = "IMD 2",
                               "as.factor(imd)IMD 3" = "IMD 3",
                               "as.factor(imd)IMD 4" = "IMD 4",
                               "as.factor(imd)IMD 5" = "IMD 5",
                               "as.factor(prac_region)North East" = "North East",
                               "as.factor(prac_region)North West" = "North West",
                               "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                               "as.factor(prac_region)East Midlands" = "East Midlands",
                               "as.factor(prac_region)West Midlands" = "West Midlands",
                               "as.factor(prac_region)East of England" = "East of England", 
                               "as.factor(prac_region)South West" = "South West",
                               "as.factor(prac_region)South Central" = "South Central",
                               "as.factor(prac_region)South East Coast" = "South East Coast")
glm_mvnm_black$estimate <- as.numeric(glm_mvnm_black$estimate)
glm_mvnm_black <- glm_mvnm_black %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mvnm_black$lower <- as.numeric(glm_mvnm_black$lower)
glm_mvnm_black$upper <- as.numeric(glm_mvnm_black$upper)
glm_mvnm_black$p <- as.numeric(glm_mvnm_black$p)
glm_mvnm_black <- glm_mvnm_black %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mvnm_black$ci <- paste(glm_mvnm_black$lower, glm_mvnm_black$upper, sep ="-")
glm_mvnm_black$estimate[1] <- 1.00
glm_mvnm_black$lower[1] <- 1.00
glm_mvnm_black$upper[1] <- 1.00
glm_mvnm_black$ci[1] <- "1.00-1.00"
glm_mvnm_black$irr_ci <- paste(glm_mvnm_black$estimate, glm_mvnm_black$ci, sep =",")
glm_mvnm_black_table <- dplyr::select(glm_mvnm_black, names, irr_ci)
write_csv(glm_mvnm_black_table, "filepath")


## Other ---

other <- sdipresc_annual_counts_final_extra_preg_max %>% filter(ethnicat6 == "Other ethnic group")
other_pats <-  other %>% distinct(patid, .keep_all = TRUE) 
other_totals <- other_pats %>% group_by(migrant_status) %>% tally()

# IR by migrant status 
pyears_mvnm_other <- aggregate(pyears_ey_pregmax ~ migrant_status, other, sum) 
pyears_mvnm_other <- pyears_mvnm_other %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_mvnm_other<- aggregate(sdipresc_n_pregmax ~ migrant_status, other, sum) 
IR_mvnm_other <- inner_join(sdipresccount_mvnm_other,pyears_mvnm_other, by= c("migrant_status" = "migrant_status"))
IR_mvnm_other_output <- pois.exact(IR_mvnm_other$sdipresc_n_pregmax, IR_mvnm_other$pyears_ey_pregmax, conf.level=0.95)
IR_mvnm_other_output <- IR_mvnm_other_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_mvnm_other_output <- IR_mvnm_other_output %>%
  mutate(lower100000=lower*100000)
IR_mvnm_other_output <- IR_mvnm_other_output %>%
  mutate(upper100000=upper*100000)
migrant_status <- IR_mvnm_other[,1]
IR_mvnm_other_output <- cbind(IR_mvnm_other_output, migrant_status)
IR_mvnm_other_output <- IR_mvnm_other_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_other_output <- IR_mvnm_other_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_other_output$ci <- paste(IR_mvnm_other_output$lower100000, IR_mvnm_other_output$upper100000, sep ="-")
IR_mvnm_other_output$ir_ci <- paste(IR_mvnm_other_output$incidence_rate100000, IR_mvnm_other_output$ci, sep =",")
IR_mvnm_other_output_table <-  dplyr::select(IR_mvnm_other_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + offset(log(pyears_ey_pregmax)), data = other)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_other <- cbind(names,estimate,confint.default,p)
glm_mig_other <- as_tibble(glm_IRR_mvnm_other)
glm_mig_other$names <- factor(glm_mig_other$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant"))
glm_mig_other$names <- recode(glm_mig_other$names, "(Intercept)" = "Non-migrant" ,"as.factor(migrant_status)Migrant" = "Migrant")
glm_mig_other$estimate <- as.numeric(glm_mig_other$estimate)
glm_mig_other <- glm_mig_other %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mig_other$upper <- as.numeric(glm_mig_other$upper)
glm_mig_other$lower <- as.numeric(glm_mig_other$lower)
glm_mig_other$p <- as.numeric(glm_mig_other$p)
glm_mig_other <- glm_mig_other %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mig_other$ci <- paste(glm_mig_other$lower, glm_mig_other$upper, sep ="-")
glm_mig_other$estimate[1] <- 1.00
glm_mig_other$lower[1] <- 1.00
glm_mig_other$upper[1] <- 1.00
glm_mig_other$ci[1] <- "1.00-1.00"
glm_mig_other$irr_ci <- paste(glm_mig_other$estimate, glm_mig_other$ci, sep =",")
glm_mig_other_table <- dplyr::select(glm_mig_other,names, irr_ci ) 

## join glm + IR + univariable_mig 
univariable_mig_other <- full_join(IR_mvnm_other_output_table, glm_mig_other_table, by = c("migrant_status" = "names"))
write_csv(univariable_mig_other, "filepath")

## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey_pregmax)) , 
            data = other)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm_other <- cbind(names,estimate,confint.default,p)
glm_mvnm_other <- as_tibble(glm_IRR_mvnm_other)
glm_mvnm_other$names <- factor(glm_mvnm_other$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant", 
                                                       "as.factor(eventyear)2010",
                                                       "as.factor(eventyear)2011",
                                                       "as.factor(eventyear)2012",
                                                       "as.factor(eventyear)2013",
                                                       "as.factor(eventyear)2014",
                                                       "as.factor(eventyear)2015",
                                                       "as.factor(eventyear)2016",
                                                       "as.factor(eventyear)2017",
                                                       "as.factor(eventyear)2018" ,
                                                       "as.factor(eventyear_agecat)20-24",
                                                       "as.factor(eventyear_agecat)25-29",
                                                       "as.factor(eventyear_agecat)30-34",
                                                       "as.factor(eventyear_agecat)35-39",
                                                       "as.factor(eventyear_agecat)40-44",
                                                       "as.factor(eventyear_agecat)45-49",
                                                       "as.factor(imd)IMD 2",
                                                       "as.factor(imd)IMD 3",
                                                       "as.factor(imd)IMD 4",
                                                       "as.factor(imd)IMD 5",
                                                       "as.factor(prac_region)North East",
                                                       "as.factor(prac_region)North West",
                                                       "as.factor(prac_region)Yorkshire & The Humber",
                                                       "as.factor(prac_region)East Midlands",
                                                       "as.factor(prac_region)West Midlands",
                                                       "as.factor(prac_region)East of England", 
                                                       "as.factor(prac_region)South West",
                                                       "as.factor(prac_region)South Central",
                                                       "as.factor(prac_region)South East Coast"))
glm_mvnm_other$names <- recode(glm_mvnm_other$names,"(Intercept)" = "Non-migrant" ,
                               "as.factor(migrant_status)Migrant" = "Migrant",
                               "as.factor(eventyear)2010" = "2010",
                               "as.factor(eventyear)2011" = "2011",
                               "as.factor(eventyear)2012" = "2012",
                               "as.factor(eventyear)2013" = "2013",
                               "as.factor(eventyear)2014" = "2014",
                               "as.factor(eventyear)2015" = "2015",
                               "as.factor(eventyear)2016" = "2016",
                               "as.factor(eventyear)2017" = "2017",
                               "as.factor(eventyear)2018" = "2018",
                               "as.factor(eventyear_agecat)20-24" = "20-24",
                               "as.factor(eventyear_agecat)25-29" = "25-29",
                               "as.factor(eventyear_agecat)30-34" = "30-34",
                               "as.factor(eventyear_agecat)35-39" = "35-39",
                               "as.factor(eventyear_agecat)40-44" = "40-44",
                               "as.factor(eventyear_agecat)45-49" = "45-49",
                               "as.factor(imd)IMD 2" = "IMD 2",
                               "as.factor(imd)IMD 3" = "IMD 3",
                               "as.factor(imd)IMD 4" = "IMD 4",
                               "as.factor(imd)IMD 5" = "IMD 5",
                               "as.factor(prac_region)North East" = "North East",
                               "as.factor(prac_region)North West" = "North West",
                               "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                               "as.factor(prac_region)East Midlands" = "East Midlands",
                               "as.factor(prac_region)West Midlands" = "West Midlands",
                               "as.factor(prac_region)East of England" = "East of England", 
                               "as.factor(prac_region)South West" = "South West",
                               "as.factor(prac_region)South Central" = "South Central",
                               "as.factor(prac_region)South East Coast" = "South East Coast")
glm_mvnm_other$estimate <- as.numeric(glm_mvnm_other$estimate)
glm_mvnm_other <- glm_mvnm_other %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mvnm_other$lower <- as.numeric(glm_mvnm_other$lower)
glm_mvnm_other$upper <- as.numeric(glm_mvnm_other$upper)
glm_mvnm_other$p <- as.numeric(glm_mvnm_other$p)
glm_mvnm_other <- glm_mvnm_other %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mvnm_other$ci <- paste(glm_mvnm_other$lower, glm_mvnm_other$upper, sep ="-")
glm_mvnm_other$estimate[1] <- 1.00
glm_mvnm_other$lower[1] <- 1.00
glm_mvnm_other$upper[1] <- 1.00
glm_mvnm_other$ci[1] <- "1.00-1.00"
glm_mvnm_other$irr_ci <- paste(glm_mvnm_other$estimate, glm_mvnm_other$ci, sep =",")
glm_mvnm_other_table <- dplyr::select(glm_mvnm_other, names, irr_ci)
write_csv(glm_mvnm_other_table, "filepath")


## Forest plots ---

# FP of adjusted and fully unadjusted IRR
glm_mvnm_white_british_fp <- glm_mvnm_white_british[1:2,]
glm_mvnm_white_british_fp$names <- recode(glm_mvnm_white_british_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "White British ethnicity - Migrant(adj)")
glm_mvnm_white_nonbritish_fp <- glm_mvnm_white_nonbritish[1:2,]
glm_mvnm_white_nonbritish_fp$names <- recode(glm_mvnm_white_nonbritish_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "White Non-British ethnicity - Migrant(adj)")
glm_mvnm_mixed_fp <- glm_mvnm_mixed[1:2,]
glm_mvnm_mixed_fp$names <- recode(glm_mvnm_mixed_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Mixed ethnicity - Migrant(adj)")
glm_mvnm_asian_fp <- glm_mvnm_asian[1:2,]
glm_mvnm_asian_fp$names <- recode(glm_mvnm_asian_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Asian ethnicity - Migrant(adj)")
glm_mvnm_black_fp <- glm_mvnm_black[1:2,]
glm_mvnm_black_fp$names <- recode(glm_mvnm_black_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Black ethnicity - Migrant(adj)")
glm_mvnm_other_fp <- glm_mvnm_other[1:2,]
glm_mvnm_other_fp$names <- recode(glm_mvnm_other_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Other ethnicity - Migrant(adj)")


glm_adj_unadj<- full_join(glm_mig_white_british, glm_mvnm_white_british_fp, by = c("names" = "names", "estimate" = "estimate",
                                                                                   "lower" = "lower", "upper"= "upper",
                                                                                   "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_white_nonbritish, by = c("names" = "names", "estimate" = "estimate",
                                             "lower" = "lower", "upper"= "upper",
                                             "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_white_nonbritish_fp, by = c("names" = "names", "estimate" = "estimate",
                                                 "lower" = "lower", "upper"= "upper",
                                                 "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_mixed, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_mixed_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_asian, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_asian_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_black, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_black_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_other, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_other_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) 


sdi_glm_adj_unadj_ethnicity <- glm_adj_unadj
save(sdi_glm_adj_unadj_ethnicity, file = "filepath")
sdi_glm_adj_unadj_ethnicity_deduped <- sdi_glm_adj_unadj_ethnicity %>% dplyr::select(-p) %>% distinct()

tabletext <- cbind(c("Non-migrants", "White British ethnicity - Migrants (unadjusted)", "White British ethnicity - Migrants (fully adjusted)",
                     "White Non-British ethnicity - Migrants (unadjusted)", "White Non-British ethnicity - Migrants (fully adjusted)",
                     "Mixed ethnicity - Migrants (unadjusted)", "Mixed ethnicity - Migrants (fully adjusted)",
                     "Asian/Asian British ethnicity - Migrants (unadjusted)", "Asian/Asian British ethnicity - Migrants (fully adjusted)",
                     "Black/Black British ethnicity - Migrants (unadjusted)", "Black/Black British ethnicity - Migrants (fully adjusted)",
                     "Other ethnicity - Migrants (unadjusted)", "Other ethnicity - Migrants (fully adjusted)"),
                   c(sdi_glm_adj_unadj_ethnicity_deduped$estimate),
                   c(sdi_glm_adj_unadj_ethnicity_deduped$ci))
dev.new()
png(file = "filepath", width = 800, height = 700)
irr_migrant_status_ethnicity <- forestplot(tabletext, mean = sdi_glm_adj_unadj_ethnicity_deduped$estimate, lower=sdi_glm_adj_unadj_ethnicity_deduped$lower, upper=sdi_glm_adj_unadj_ethnicity_deduped$upper,
                                           graph.pos = (ncol(tabletext)-1),
                                           hrzl_lines = TRUE, xlab = "IRR", zero = 1, 
                                           col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                           xticks = 0:2,
                                           ci.vertices = TRUE,
                                           boxsize = 0.05,
                                           title = "sdipresc",
                                           graphwidth = unit(80, 'mm'))
dev.off()





## Pregnancy min assumption --------------------


# based on preg min assumption 
# some years have pdays/years_pregmin <0 - this means that those years should be dropped for these analyses as based on min assumption the person was pregnant for the whole period they were in the cohort that year
# for the pyears/pdays of baseline characteristics, the dropping of these will automatically occur 

# drop event years with pdays less than or equal to 0 - 4,892,032 x 28
sdipresc_annual_counts_final_extra_preg_min <- sdipresc_annual_counts_final_extra %>% filter(pdays_ey_pregmin > 0)

## IR & univariable IRR - migrant status 


# IR by migrant status - whole cohort
pyears_mvnm_overall <- aggregate(pyears_ey_pregmin~ migrant_status, sdipresc_annual_counts_final_extra_preg_min, sum) 
pyears_mvnm_overall <- pyears_mvnm_overall %>% mutate(across(is.numeric, ~ round(.,)))
sdipresccount_mvnm <- aggregate(sdipresc_n_pregmin ~ migrant_status, sdipresc_annual_counts_final_extra_preg_min, sum) 
IR_mvnm <- inner_join(sdipresccount_mvnm,pyears_mvnm_overall, by= c("migrant_status" = "migrant_status"))
IR_mvnm_output <- pois.exact(IR_mvnm$sdipresc_n_pregmin, IR_mvnm$pyears_ey_pregmin, conf.level=0.95)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(lower100000=lower*100000)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(upper100000=upper*100000)
migrant_status <- IR_mvnm[,1]
IR_mvnm_output <- cbind(IR_mvnm_output, migrant_status)
IR_mvnm_output <- IR_mvnm_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_output <- IR_mvnm_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_output$ci <- paste(IR_mvnm_output$lower100000, IR_mvnm_output$upper100000, sep ="-")
IR_mvnm_output$ir_ci <- paste(IR_mvnm_output$incidence_rate100000, IR_mvnm_output$ci, sep =",")
IR_mvnm_output_table <-  dplyr::select(IR_mvnm_output, migrant_status, events, person_years, ir_ci)


##  generate IRR by negative binomial regression

x <- glm.nb(sdipresc_n_pregmin ~ as.factor(migrant_status) + offset(log(pyears_ey_pregmin)), data = sdipresc_annual_counts_final_extra_preg_min)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm <- cbind(names,estimate,confint.default,p)
glm_mig <- as_tibble(glm_IRR_mvnm)
glm_mig$names <- factor(glm_mig$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant"))
glm_mig$names <- recode(glm_mig$names, "(Intercept)" = "Non-migrant" ,"as.factor(migrant_status)Migrant" = "Migrant")
glm_mig$estimate <- as.numeric(glm_mig$estimate)
glm_mig <- glm_mig %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mig$upper <- as.numeric(glm_mig$upper)
glm_mig$lower <- as.numeric(glm_mig$lower)
glm_mig$p <- as.numeric(glm_mig$p)
glm_mig <- glm_mig %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mig$ci <- paste(glm_mig$lower, glm_mig$upper, sep ="-")
glm_mig$estimate[1] <- 1.00
glm_mig$lower[1] <- 1.00
glm_mig$upper[1] <- 1.00
glm_mig$ci[1] <- "1.00-1.00"
glm_mig$irr_ci <- paste(glm_mig$estimate, glm_mig$ci, sep =",")
glm_mig_table <- dplyr::select(glm_mig,names, irr_ci ) 



## join glm + IR + univariable_mig 
univariable_mig <- full_join(IR_mvnm_output_table, glm_mig_table, by = c("migrant_status" = "names"))
write_csv(univariable_mig,"filepath")

## Multivariable IRR by migrant status  
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(sdipresc_n_pregmin ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey_pregmin)) , 
            data = sdipresc_annual_counts_final_extra_preg_min)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_all_mvnm <- cbind(names,estimate,confint.default,p)
glm_all_mvnm <- as_tibble(glm_IRR_all_mvnm)
glm_all_mvnm$names <- factor(glm_all_mvnm$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant", 
                                                   "as.factor(eventyear)2010",
                                                   "as.factor(eventyear)2011",
                                                   "as.factor(eventyear)2012",
                                                   "as.factor(eventyear)2013",
                                                   "as.factor(eventyear)2014",
                                                   "as.factor(eventyear)2015",
                                                   "as.factor(eventyear)2016",
                                                   "as.factor(eventyear)2017",
                                                   "as.factor(eventyear)2018" ,
                                                   "as.factor(eventyear_agecat)20-24",
                                                   "as.factor(eventyear_agecat)25-29",
                                                   "as.factor(eventyear_agecat)30-34",
                                                   "as.factor(eventyear_agecat)35-39",
                                                   "as.factor(eventyear_agecat)40-44",
                                                   "as.factor(eventyear_agecat)45-49",
                                                   "as.factor(imd)IMD 2",
                                                   "as.factor(imd)IMD 3",
                                                   "as.factor(imd)IMD 4",
                                                   "as.factor(imd)IMD 5",
                                                   "as.factor(prac_region)North East",
                                                   "as.factor(prac_region)North West",
                                                   "as.factor(prac_region)Yorkshire & The Humber",
                                                   "as.factor(prac_region)East Midlands",
                                                   "as.factor(prac_region)West Midlands",
                                                   "as.factor(prac_region)East of England", 
                                                   "as.factor(prac_region)South West",
                                                   "as.factor(prac_region)South Central",
                                                   "as.factor(prac_region)South East Coast"))
glm_all_mvnm$names <- recode(glm_all_mvnm$names,"(Intercept)" = "Non-migrant" ,
                             "as.factor(migrant_status)Migrant" = "Migrant",
                             "as.factor(eventyear)2010" = "2010",
                             "as.factor(eventyear)2011" = "2011",
                             "as.factor(eventyear)2012" = "2012",
                             "as.factor(eventyear)2013" = "2013",
                             "as.factor(eventyear)2014" = "2014",
                             "as.factor(eventyear)2015" = "2015",
                             "as.factor(eventyear)2016" = "2016",
                             "as.factor(eventyear)2017" = "2017",
                             "as.factor(eventyear)2018" = "2018",
                             "as.factor(eventyear_agecat)20-24" = "20-24",
                             "as.factor(eventyear_agecat)25-29" = "25-29",
                             "as.factor(eventyear_agecat)30-34" = "30-34",
                             "as.factor(eventyear_agecat)35-39" = "35-39",
                             "as.factor(eventyear_agecat)40-44" = "40-44",
                             "as.factor(eventyear_agecat)45-49" = "45-49",
                             "as.factor(imd)IMD 2" = "IMD 2",
                             "as.factor(imd)IMD 3" = "IMD 3",
                             "as.factor(imd)IMD 4" = "IMD 4",
                             "as.factor(imd)IMD 5" = "IMD 5",
                             "as.factor(prac_region)North East" = "North East",
                             "as.factor(prac_region)North West" = "North West",
                             "as.factor(prac_region)Yorkshire & The Humber" = "Yorkshire & The Humber",
                             "as.factor(prac_region)East Midlands" = "East Midlands",
                             "as.factor(prac_region)West Midlands" = "West Midlands",
                             "as.factor(prac_region)East of England" = "East of England", 
                             "as.factor(prac_region)South West" = "South West",
                             "as.factor(prac_region)South Central" = "South Central",
                             "as.factor(prac_region)South East Coast" = "South East Coast")
glm_all_mvnm$estimate <- as.numeric(glm_all_mvnm$estimate)
glm_all_mvnm <- glm_all_mvnm %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_all_mvnm$lower <- as.numeric(glm_all_mvnm$lower)
glm_all_mvnm$upper <- as.numeric(glm_all_mvnm$upper)
glm_all_mvnm$p <- as.numeric(glm_all_mvnm$p)
glm_all_mvnm <- glm_all_mvnm %>% mutate(across(is.numeric, ~ round(.,2)))
glm_all_mvnm$ci <- paste(glm_all_mvnm$lower, glm_all_mvnm$upper, sep ="-")
glm_all_mvnm$estimate[1] <- 1.00
glm_all_mvnm$lower[1] <- 1.00
glm_all_mvnm$upper[1] <- 1.00
glm_all_mvnm$ci[1] <- "1.00-1.00"
glm_all_mvnm$irr_ci <- paste(glm_all_mvnm$estimate, glm_all_mvnm$ci, sep =",")
glm_all_mvnm_table <- dplyr::select(glm_all_mvnm, names, irr_ci)
write_csv(glm_all_mvnm_table, "filepath")



# FP of unadjusted and fully adjusted IRR
glm_all_mvnm_fp <- glm_all_mvnm[1:2,]
glm_all_mvnm_fp$names <- recode(glm_all_mvnm_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Migrant(adj)")
glm_adj_unadj <- full_join(glm_mig, glm_all_mvnm_fp, by = c("names" = "names", "estimate" = "estimate",
                                                            "lower" = "lower", "upper"= "upper",
                                                            "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci"))
sdi_glm_adj_unadj_pregmin <- glm_adj_unadj
save(sdi_glm_adj_unadj_pregmin, file = "filepath")


tabletext <- cbind(c("Non-migrants", "Migrants (unadjusted)", "Migrants (fully adjusted)"),
                   c(glm_adj_unadj$estimate),
                   c(glm_adj_unadj$ci))
dev.new()
png(file = "filepath", width = 600, height = 480)
unadj_irr_migrant_status <- forestplot(tabletext, mean = glm_adj_unadj$estimate, lower=glm_adj_unadj$lower, upper=glm_adj_unadj$upper,
                                       graph.pos = (ncol(tabletext)-1),
                                       hrzl_lines = TRUE, xlab = "IRR", zero = 1, 
                                       col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                       xticks = 0:2,
                                       ci.vertices = TRUE,
                                       boxsize = 0.05,
                                       title = "sdipresc",
                                       graphwidth = unit(80, 'mm'))
dev.off()


## Matched cohort 4:1 with 365D washout from data start --------------

# Load final datasets for analysis .Rdata files 

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")

exact_match_sdipresc_annual_counts_final_4to1$prac_region <- relevel(exact_match_sdipresc_annual_counts_final_4to1$prac_region, "London")
exact_match_sdipresc_annual_counts_final_extra_4to1$prac_region <- relevel(exact_match_sdipresc_annual_counts_final_extra_4to1$prac_region, "London")


# Using preg max assumption

exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max <- exact_match_sdipresc_annual_counts_final_extra_4to1 %>% filter(pyears_ey_pregmax > 0)


# IR by migrant status - whole cohort
pyears_mvnm_overall <- aggregate(pyears_ey_pregmax ~ migrant_status, exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max, sum) 
pyears_mvnm_overall <- pyears_mvnm_overall %>% mutate(across(is.numeric, ~ round(.,0)))
sdipresccount_mvnm <- aggregate(sdipresc_n_pregmax ~ migrant_status, exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max, sum) 
IR_mvnm <- inner_join(sdipresccount_mvnm,pyears_mvnm_overall, by= c("migrant_status" = "migrant_status"))
IR_mvnm_output <- pois.exact(IR_mvnm$sdipresc_n_pregmax, IR_mvnm$pyears_ey_pregmax, conf.level=0.95)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(incidence_rate100000=rate*100000)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(lower100000=lower*100000)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(upper100000=upper*100000)
migrant_status <- IR_mvnm[,1]
IR_mvnm_output <- cbind(IR_mvnm_output, migrant_status)
IR_mvnm_output <- IR_mvnm_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_output <- IR_mvnm_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_output$ci <- paste(IR_mvnm_output$lower100000, IR_mvnm_output$upper100000, sep ="-")
IR_mvnm_output$ir_ci <- paste(IR_mvnm_output$incidence_rate100000, IR_mvnm_output$ci, sep =",")
IR_mvnm_output_table <-  dplyr::select(IR_mvnm_output, migrant_status, events, person_years, ir_ci)




##  generate IRR by negative binomial regression for mig status ---

x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + offset(log(pyears_ey_pregmax)), data = exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_mvnm <- cbind(names,estimate,confint.default,p)
glm_mig <- as_tibble(glm_IRR_mvnm)
glm_mig$names <- factor(glm_mig$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant"))
glm_mig$names <- recode(glm_mig$names, "(Intercept)" = "Non-migrant" ,"as.factor(migrant_status)Migrant" = "Migrant")
glm_mig$estimate <- as.numeric(glm_mig$estimate)
glm_mig <- glm_mig %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_mig$upper <- as.numeric(glm_mig$upper)
glm_mig$lower <- as.numeric(glm_mig$lower)
glm_mig$p <- as.numeric(glm_mig$p)
glm_mig <- glm_mig %>% mutate(across(is.numeric, ~ round(.,2)))
glm_mig$ci <- paste(glm_mig$lower, glm_mig$upper, sep ="-")
glm_mig$estimate[1] <- 1.00
glm_mig$lower[1] <- 1.00
glm_mig$upper[1] <- 1.00
glm_mig$ci[1] <- "1.00-1.00"
glm_mig$irr_ci <- paste(glm_mig$estimate, glm_mig$ci, sep =",")
glm_mig_table <- dplyr::select(glm_mig,names, irr_ci ) 



## join glm + IR + univariable_mig 
univariable_mig <- full_join(IR_mvnm_output_table, glm_mig_table, by = c("migrant_status" = "names"))
write_csv(univariable_mig, "filepath")


## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd (dropped prac_region as matched on prac region, kept age cat for study year as indviduals not matched by time in study just age at cohort entry + year of ochort entry)


## Generate IRR via negative binomial regression
x <- glm.nb(sdipresc_n_pregmax ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + offset(log(pyears_ey_pregmax)) , 
            data = exact_match_sdipresc_annual_counts_final_extra_4to1_preg_max)
names <- names(coef(x))
estimate<- exp(coef(x))
confint.default <- exp(confint.default(x))
p <- coef(summary(x))[,4]
glm_IRR_all_mvnm <- cbind(names,estimate,confint.default,p)
glm_all_mvnm <- as_tibble(glm_IRR_all_mvnm)
glm_all_mvnm$names <- factor(glm_all_mvnm$names, c("(Intercept)" ,"as.factor(migrant_status)Migrant", 
                                                   "as.factor(eventyear)2010",
                                                   "as.factor(eventyear)2011",
                                                   "as.factor(eventyear)2012",
                                                   "as.factor(eventyear)2013",
                                                   "as.factor(eventyear)2014",
                                                   "as.factor(eventyear)2015",
                                                   "as.factor(eventyear)2016",
                                                   "as.factor(eventyear)2017",
                                                   "as.factor(eventyear)2018" ,
                                                   "as.factor(eventyear_agecat)20-24",
                                                   "as.factor(eventyear_agecat)25-29",
                                                   "as.factor(eventyear_agecat)30-34",
                                                   "as.factor(eventyear_agecat)35-39",
                                                   "as.factor(eventyear_agecat)40-44",
                                                   "as.factor(eventyear_agecat)45-49",
                                                   "as.factor(imd)IMD 2",
                                                   "as.factor(imd)IMD 3",
                                                   "as.factor(imd)IMD 4",
                                                   "as.factor(imd)IMD 5"))
glm_all_mvnm$names <- recode(glm_all_mvnm$names,"(Intercept)" = "Non-migrant" ,
                             "as.factor(migrant_status)Migrant" = "Migrant",
                             "as.factor(eventyear)2010" = "2010",
                             "as.factor(eventyear)2011" = "2011",
                             "as.factor(eventyear)2012" = "2012",
                             "as.factor(eventyear)2013" = "2013",
                             "as.factor(eventyear)2014" = "2014",
                             "as.factor(eventyear)2015" = "2015",
                             "as.factor(eventyear)2016" = "2016",
                             "as.factor(eventyear)2017" = "2017",
                             "as.factor(eventyear)2018" = "2018",
                             "as.factor(eventyear_agecat)20-24" = "20-24",
                             "as.factor(eventyear_agecat)25-29" = "25-29",
                             "as.factor(eventyear_agecat)30-34" = "30-34",
                             "as.factor(eventyear_agecat)35-39" = "35-39",
                             "as.factor(eventyear_agecat)40-44" = "40-44",
                             "as.factor(eventyear_agecat)45-49" = "45-49",
                             "as.factor(imd)IMD 2" = "IMD 2",
                             "as.factor(imd)IMD 3" = "IMD 3",
                             "as.factor(imd)IMD 4" = "IMD 4",
                             "as.factor(imd)IMD 5" = "IMD 5")
glm_all_mvnm$estimate <- as.numeric(glm_all_mvnm$estimate)
glm_all_mvnm <- glm_all_mvnm %>% rename(lower = "2.5 %") %>% rename(upper = "97.5 %") 
glm_all_mvnm$lower <- as.numeric(glm_all_mvnm$lower)
glm_all_mvnm$upper <- as.numeric(glm_all_mvnm$upper)
glm_all_mvnm$p <- as.numeric(glm_all_mvnm$p)
glm_all_mvnm <- glm_all_mvnm %>% mutate(across(is.numeric, ~ round(.,2)))
glm_all_mvnm$ci <- paste(glm_all_mvnm$lower, glm_all_mvnm$upper, sep ="-")
glm_all_mvnm$estimate[1] <- 1.00
glm_all_mvnm$lower[1] <- 1.00
glm_all_mvnm$upper[1] <- 1.00
glm_all_mvnm$ci[1] <- "1.00-1.00"
glm_all_mvnm$irr_ci <- paste(glm_all_mvnm$estimate, glm_all_mvnm$ci, sep =",")
glm_all_mvnm_table <- dplyr::select(glm_all_mvnm, names, irr_ci)
write_csv(glm_all_mvnm_table, "filepath")


## Forest plots ---

# FP of adjusted and fully unadjusted IRR
glm_all_mvnm_fp <- glm_all_mvnm[1:2,]
glm_all_mvnm_fp$names <- recode(glm_all_mvnm_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Migrant(adj)")
glm_adj_unadj <- full_join(glm_mig, glm_all_mvnm_fp, by = c("names" = "names", "estimate" = "estimate",
                                                            "lower" = "lower", "upper"= "upper",
                                                            "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci"))



sdi_glm_adj_unadj_em <- glm_adj_unadj
save(sdi_glm_adj_unadj_em, file = "filepath")


tabletext <- cbind(c("Non-migrants", "Migrants (unadjusted)", "Migrants (fully adjusted)"),
                   c(glm_adj_unadj$estimate),
                   c(glm_adj_unadj$ci))
dev.new()
png(file = "filepath", width = 600, height = 480)
unadj_irr_migrant_status <- forestplot(tabletext, mean = glm_adj_unadj$estimate, lower=glm_adj_unadj$lower, upper=glm_adj_unadj$upper,
                                       graph.pos = (ncol(tabletext)-1),
                                       hrzl_lines = TRUE, xlab = "IRR", zero = 1, 
                                       col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                       xticks = 0:2,
                                       ci.vertices = TRUE,
                                       boxsize = 0.05,
                                       title = "sdipresc (Exact Matched Cohort)",
                                       graphwidth = unit(80, 'mm'))
dev.off()











