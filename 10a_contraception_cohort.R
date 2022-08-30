
####---- Description -------------------------------------------------------------------------

## Cleaning data for Neha's contraception prescribing chapter in females of reproductive age (15-49yo) 
## Date started: 30/09/2020
## Author: Neha Pathak


## Load packages -------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Hmisc)

## Set working directory ------------------------------------------------------------------

setwd("S:/CALIBER_19_062R/01_gold/02_females_15_49")

## load final contraception cohort .Rdata file --------------------------------

load(file = "cleaned_files/contraception_cohort_final.Rdata") 
contraception_cohort_final <- contraception_cohort_entry_exit_pdays_pyears
contraception_cohort_final$contr_cohort_entry <- as_date(contraception_cohort_final$contr_cohort_entry)
contraception_cohort_final$contr_cohort_exit <- as_date(contraception_cohort_final$contr_cohort_exit)

load(file = "cleaned_files/contrpresc_annual_counts_final.Rdata")
load(file = "cleaned_files/contrpresc_annual_counts_final_extra.Rdata")

## BASELINE CHARACTERISTICS - MAIN ANALYSIS COHORT ----------------------------------------------------------------------------------


## Totals ---

## Whole cohort 
total_cohort_n <- count(contraception_cohort_final)
total_contr_pyears <- sum(contraception_cohort_final$contr_pyears)
total_wholecohort <- bind_cols(total_cohort_n, total_contr_pyears)
colnames(total_wholecohort) <- c("individuals", "contr_pyears")
total_wholecohort <- total_wholecohort %>% mutate(group = "Whole cohort") 

## By migrant status
total_mvnm_n <- contraception_cohort_final %>% group_by(migrant_status) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migrant_status)
total_mvnm_contr_pyears <- contraception_cohort_final %>% group_by(migrant_status) %>%
  tally(contr_pyears) %>%
  rename(contr_pyears = n) %>% 
  rename(group = migrant_status)
total_mvnm <- full_join(total_mvnm_n, total_mvnm_contr_pyears,
                        by = c("group" = "group"))

## By migrant certainty
total_dp_n <- contraception_cohort_final %>% group_by(migcertainty) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migcertainty)
total_dp_contr_pyears <- contraception_cohort_final %>% group_by(migcertainty) %>%
  tally(contr_pyears) %>%
  rename(contr_pyears = n) %>% 
  rename(group = migcertainty)
total_dp <- full_join(total_dp_n, total_dp_contr_pyears,
                      by = c("group" = "group"))

## Totals for export
totals <- full_join(total_mvnm, total_dp,
                    by = c("group" = "group", "individuals" = "individuals", "contr_pyears" = "contr_pyears")) %>% 
  full_join(total_wholecohort, by = c("group" = "group", "individuals" = "individuals", "contr_pyears" = "contr_pyears")) %>% 
  mutate(across(is.numeric, ~ round(.,0)))
total_cohort_n<- as.vector(total_cohort_n$n[1])
totals_percent <- totals %>% mutate(percent = (individuals/total_cohort_n)*100) %>% 
  mutate(percent = round(percent, 1))

totals_percent$individuals <- paste(totals_percent$individuals, totals_percent$percent, sep =", ")
totals_percent <- dplyr::select(totals_percent, c(group, individuals, contr_pyears))
write_csv(totals_percent, "results/contraception_prescribing/all/baseline/1_total_percent.csv" )


## Age ---

## Mean + Median + SD + IQR Age at cohort entry 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

contraception_cohort_final_age <- contraception_cohort_final %>%
  mutate(age_cohort_entry = calc_age(dob, contr_cohort_entry))
mean_ages_overall <- psych::describe(contraception_cohort_final_age$age_cohort_entry, IQR = TRUE)
mean_ages_overall

nm <- contraception_cohort_final_age %>% filter(migrant_status == "Non-migrant")
nm_age <- psych::describe(nm$age_cohort_entry, IQR = TRUE)
nm_age

m <- contraception_cohort_final_age %>% filter(migrant_status == "Migrant")
m_age <- psych::describe(m$age_cohort_entry, IQR = TRUE)
m_age

d <- contraception_cohort_final_age %>% filter(migcertainty == "Definite")
d_age <- psych::describe(d$age_cohort_entry, IQR = TRUE)
d_age

p <- contraception_cohort_final_age %>% filter(migcertainty == "Probable")
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
write_csv(all_mean_ages, "results/contraception_prescribing/all/baseline/2_mean_median_age.csv" )


## Age category - No individuals and p years per group 
contraception_cohort_final_agecat <- contraception_cohort_final_age %>%
  mutate(agecat_cohort_entry = age_cohort_entry)
contraception_cohort_final_agecat <- contraception_cohort_final_agecat %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 15, 19), 1)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 20, 24), 2)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 25, 29), 3)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 30, 34), 4)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 35, 39), 5)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 40, 44), 6)) %>%
  mutate(agecat_cohort_entry=replace(agecat_cohort_entry, between (agecat_cohort_entry, 45, 49), 7)) 
contraception_cohort_final_agecat$agecat_cohort_entry <- factor(contraception_cohort_final_agecat$agecat_cohort_entry, levels = c(1,2,3,4,5,6,7),
                                                                labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

agecat <- contraception_cohort_final_agecat %>% 
  group_by(agecat_cohort_entry) %>% count() 
agecat <- agecat %>% mutate(percent = (n/sum(agecat$n))*100) %>%
  mutate(group = "Whole Cohort")
agecat_mvnm <- contraception_cohort_final_agecat %>% 
  group_by(agecat_cohort_entry, migrant_status) %>% count() %>% 
  rename(group = migrant_status)
agecat_m <- agecat_mvnm %>% filter(group == "Migrant")
agecat_m <- agecat_m %>% mutate(percent = (n/sum(agecat_m$n))*100)
agecat_nm <- agecat_mvnm %>% filter(group == "Non-migrant")
agecat_nm <- agecat_nm %>% mutate(percent = (n/sum(agecat_nm$n))*100)
agecat_dp <- contraception_cohort_final_agecat %>% 
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

contr_pyears_agecat <- aggregate(contr_pyears ~ agecat_cohort_entry, contraception_cohort_final_agecat, sum) %>% 
  mutate(group = "Whole Cohort")
contr_pyears_agecat_mvnm <- aggregate(contr_pyears ~ agecat_cohort_entry + migrant_status, contraception_cohort_final_agecat, sum) %>% 
  rename(group = migrant_status)
contr_pyears_agecat_dp <- aggregate(contr_pyears ~ agecat_cohort_entry + migcertainty, contraception_cohort_final_agecat, sum) %>% 
  rename(group = migcertainty)
contr_pyears_agecat_all <- contr_pyears_agecat_mvnm %>% full_join(contr_pyears_agecat_dp,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                                                                                            "group"="group","contr_pyears"="contr_pyears")) %>% 
  full_join(contr_pyears_agecat,by = c("agecat_cohort_entry"="agecat_cohort_entry",
                                           "group"="group","contr_pyears"="contr_pyears")) %>%
  rename(agecat = agecat_cohort_entry) 

contr_pyears_agecat_export <- full_join(contr_pyears_agecat_all, age_cat_all, 
                                            by = c("agecat"="agecat","group"="group")) %>% 
  mutate(across(is.numeric, ~ round(.,0))) %>% 
  dplyr::select(c(group, agecat, n_percent, contr_pyears)) 

write_csv(contr_pyears_agecat_export, "results/contraception_prescribing/all/baseline/3_age_cat.csv")


## Ethnicity ---


contraception_cohort_final$ethnicat6 <- contraception_cohort_final$ethnicat6 %>% fct_explicit_na(na_level = "Not known")
levels(contraception_cohort_final$ethnicat6)

ethnicat6 <- contraception_cohort_final %>% 
  group_by(ethnicat6) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
ethnicat6_mvnm <- contraception_cohort_final %>% 
  group_by(ethnicat6, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(ethnicat6, group, n, percent))
ethnicat6_dp <- contraception_cohort_final %>% 
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



contr_pyears_ethnicat6 <- contraception_cohort_final %>% group_by(ethnicat6) %>% tally(contr_pyears) %>% 
  mutate(group = "Whole Cohort")
contr_pyears_ethnicat6_mvnm <- contraception_cohort_final %>% group_by(migrant_status, ethnicat6) %>% tally(contr_pyears) %>% 
  rename(group = migrant_status)
contr_pyears_ethnicat6_dp <- contraception_cohort_final %>% group_by(migcertainty, ethnicat6) %>% tally(contr_pyears) %>% 
  rename(group = migcertainty)
contr_pyears_ethnicat6_all <- contr_pyears_ethnicat6_mvnm %>% full_join(contr_pyears_ethnicat6_dp,by = c("ethnicat6"="ethnicat6",
                                                                                                                     "group"="group","n"="n")) %>% 
  full_join(contr_pyears_ethnicat6,by = c("ethnicat6"="ethnicat6",
                                              "group"="group","n"="n")) %>%
  rename(contr_pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

contr_pyears_ethnicat6_export <- full_join(contr_pyears_ethnicat6_all, ethnicat6_all, 
                                               by = c("ethnicat6"="ethnicat6","group"="group")) %>% 
  relocate(group,ethnicat6,n_percent,contr_pyears)  %>% 
  arrange(group)

write_csv(contr_pyears_ethnicat6_export, "results/contraception_prescribing/all/baseline/4_ethnicat6.csv")


## practice region ---

contraception_cohort_final$prac_region <- contraception_cohort_final$prac_region %>% fct_explicit_na(na_level = "Not known")
levels(contraception_cohort_final$prac_region)

prac_region <- contraception_cohort_final %>% 
  group_by(prac_region) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
prac_region_mvnm <- contraception_cohort_final %>% 
  group_by(prac_region, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(prac_region, group, n, percent))
prac_region_dp <- contraception_cohort_final %>% 
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



contr_pyears_prac_region <- contraception_cohort_final %>% group_by(prac_region) %>% tally(contr_pyears) %>% 
  mutate(group = "Whole Cohort")
contr_pyears_prac_region_mvnm <- contraception_cohort_final %>% group_by(migrant_status, prac_region) %>% tally(contr_pyears) %>% 
  rename(group = migrant_status)
contr_pyears_prac_region_dp <- contraception_cohort_final %>% group_by(migcertainty, prac_region) %>% tally(contr_pyears) %>% 
  rename(group = migcertainty)
contr_pyears_prac_region_all <- contr_pyears_prac_region_mvnm %>% full_join(contr_pyears_prac_region_dp,by = c("prac_region"="prac_region",
                                                                                                                           "group"="group","n"="n")) %>% 
  full_join(contr_pyears_prac_region,by = c("prac_region"="prac_region",
                                                "group"="group","n"="n")) %>%
  rename(contr_pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

contr_pyears_prac_region_export <- full_join(contr_pyears_prac_region_all, prac_region_all, 
                                                 by = c("prac_region"="prac_region","group"="group")) %>% 
  relocate(group,prac_region,n_percent,contr_pyears)  %>% 
  arrange(group)

write_csv(contr_pyears_prac_region_export, "results/contraception_prescribing/all/baseline/5_prac_region.csv")


## IMD ---

contraception_cohort_final$imd<- contraception_cohort_final$imd%>% fct_explicit_na(na_level = "Not known")
levels(contraception_cohort_final$imd)

imd<- contraception_cohort_final %>% 
  group_by(imd) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
imd_mvnm <- contraception_cohort_final %>% 
  group_by(imd, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(imd, group, n, percent))
imd_dp <- contraception_cohort_final %>% 
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

contr_pyears_imd <- contraception_cohort_final %>% group_by(imd) %>% tally(contr_pyears) %>% 
  mutate(group = "Whole Cohort")
contr_pyears_imd_mvnm <- contraception_cohort_final %>% group_by(migrant_status, imd) %>% tally(contr_pyears) %>% 
  rename(group = migrant_status)
contr_pyears_imd_dp <- contraception_cohort_final %>% group_by(migcertainty, imd) %>% tally(contr_pyears) %>% 
  rename(group = migcertainty)
contr_pyears_imd_all <- contr_pyears_imd_mvnm %>% full_join(contr_pyears_imd_dp,by = c("imd"="imd",
                                                                                                   "group"="group","n"="n")) %>% 
  full_join(contr_pyears_imd,by = c("imd"="imd",
                                        "group"="group","n"="n")) %>%
  rename(contr_pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

contr_pyears_imd_export <- full_join(contr_pyears_imd_all, imd_all, 
                                         by = c("imd"="imd","group"="group")) %>% 
  relocate(group,imd,n_percent,contr_pyears)  %>% 
  arrange(group)

write_csv(contr_pyears_imd_export, "results/contraception_prescribing/all/baseline/6_imd.csv")



## Year of cohort entry ---

contraception_cohort_final_yearofentry <- contraception_cohort_final %>% mutate(yearofentry = year(contr_cohort_entry))
mean_yearofentry_overall <- psych::describe(contraception_cohort_final_yearofentry$yearofentry, IQR = TRUE)
mean_yearofentry_overall

nm <- contraception_cohort_final_yearofentry %>% filter(migrant_status == "Non-migrant")
nm_yearofentry <- psych::describe(nm$yearofentry, IQR = TRUE)
nm_yearofentry

m <- contraception_cohort_final_yearofentry %>% filter(migrant_status == "Migrant")
m_yearofentry <- psych::describe(m$yearofentry, IQR = TRUE)
m_yearofentry

d <- contraception_cohort_final_yearofentry %>% filter(migcertainty == "Definite")
d_yearofentry  <- psych::describe(d$yearofentry, IQR = TRUE)
d_yearofentry 

p <- contraception_cohort_final_yearofentry %>% filter(migcertainty == "Probable")
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
write_csv(all_mean_yearofentry, "results/contraception_prescribing/all/baseline/mean_median_yearofentry.csv" )


contraception_cohort_final_yearofentry$yearofentry <- factor(contraception_cohort_final_yearofentry$yearofentry,  levels = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018), 
                                                             labels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
yearofentry <- contraception_cohort_final_yearofentry %>% 
  group_by(yearofentry) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
yearofentry_mvnm <- contraception_cohort_final_yearofentry %>% 
  group_by(yearofentry, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(yearofentry, group, n, percent))
yearofentry_dp <- contraception_cohort_final_yearofentry %>% 
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

contr_pyears_yearofentry <- contraception_cohort_final_yearofentry %>% group_by(yearofentry) %>% tally(contr_pyears) %>% 
  mutate(group = "Whole Cohort")
contr_pyears_yearofentry_mvnm <- contraception_cohort_final_yearofentry %>% group_by(migrant_status, yearofentry) %>% tally(contr_pyears) %>% 
  rename(group = migrant_status)
contr_pyears_yearofentry_dp <- contraception_cohort_final_yearofentry %>% group_by(migcertainty, yearofentry) %>% tally(contr_pyears) %>% 
  rename(group = migcertainty)
contr_pyears_yearofentry_all <- contr_pyears_yearofentry_mvnm %>% full_join(contr_pyears_yearofentry_dp,by = c("yearofentry"="yearofentry",
                                                                                                                           "group"="group","n"="n")) %>% 
  full_join(contr_pyears_yearofentry,by = c("yearofentry"="yearofentry",
                                                "group"="group","n"="n")) %>%
  rename(contr_pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

contr_pyears_yearofentry_export <- full_join(contr_pyears_yearofentry_all, yearofentry_all, 
                                                 by = c("group"="group","yearofentry"="yearofentry")) %>% 
  relocate(group,yearofentry,n_percent,contr_pyears)  %>% 
  arrange(group)

write_csv(contr_pyears_yearofentry_export, "results/contraception_prescribing/all/baseline/6_yearofentry.csv")


## Year ---

eventyear<- contrpresc_annual_counts_final_extra %>% 
  group_by(eventyear) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
eventyear_mvnm <- contrpresc_annual_counts_final_extra  %>% 
  group_by(eventyear, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(eventyear, group, n, percent))
eventyear_dp <- contrpresc_annual_counts_final_extra  %>% 
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

pyears_eventyear <- contrpresc_annual_counts_final_extra  %>% group_by(eventyear) %>% tally(pyears_ey) %>% 
  mutate(group = "Whole Cohort")
pyears_eventyear_mvnm <- contrpresc_annual_counts_final_extra  %>% group_by(migrant_status, eventyear) %>% tally(pyears_ey) %>% 
  rename(group = migrant_status)
pyears_eventyear_dp <- contrpresc_annual_counts_final_extra  %>% group_by(migcertainty, eventyear) %>% tally(pyears_ey) %>% 
  rename(group = migcertainty)
pyears_eventyear_all <- pyears_eventyear_mvnm %>% full_join(pyears_eventyear_dp,by = c("eventyear"="eventyear",
                                                                                       "group"="group","n"="n")) %>% 
  full_join(pyears_eventyear,by = c("eventyear"="eventyear",
                                    "group"="group","n"="n")) %>%
  rename(contr_pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_eventyear_export <- full_join(pyears_eventyear_all, eventyear_all, 
                                     by = c("eventyear"="eventyear","group"="group")) %>% 
  relocate(group,eventyear,n_percent,contr_pyears)  %>% 
  arrange(group)

write_csv(pyears_eventyear_export, "results/contraception_prescribing/all/baseline/7_eventyear.csv")

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
ggsave("results/contraception_prescribing/all/baseline/eventyear_line_point.jpg", width = 7, height = 5, dpi = 300)



## Person years ---

mean_contr_pyears_overall <- psych::describe(contraception_cohort_final$contr_pyears, IQR = TRUE)
mean_contr_pyears_overall

nm <- contraception_cohort_final %>% filter(migrant_status == "Non-migrant")
nm_contr_pyears <- psych::describe(nm$contr_pyears, IQR = TRUE)
nm_contr_pyears

m <- contraception_cohort_final %>% filter(migrant_status == "Migrant")
m_contr_pyears <- psych::describe(m$contr_pyears, IQR = TRUE)
m_contr_pyears

d <- contraception_cohort_final %>% filter(migcertainty == "Definite")
d_contr_pyears <- psych::describe(d$contr_pyears, IQR = TRUE)
d_contr_pyears

p <- contraception_cohort_final %>% filter(migcertainty == "Probable")
p_contr_pyears <- psych::describe(p$contr_pyears, IQR = TRUE)
p_contr_pyears

## join export mean + median ages + sd + IQR
mean_contr_pyears_overall <- as.data.frame((mean_contr_pyears_overall))
mean_contr_pyears_overall <- dplyr::select(mean_contr_pyears_overall, c(mean , sd, median, IQR)) %>% mutate(group = "Whole cohort") 
nm_contr_pyears <- as.data.frame((nm_contr_pyears))
nm_contr_pyears <- dplyr::select(nm_contr_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Non-migrant") 
m_contr_pyears <- as.data.frame((m_contr_pyears))
m_contr_pyears <- dplyr::select(m_contr_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Migrant") 
d_contr_pyears <- as.data.frame((d_contr_pyears))
d_contr_pyears <- dplyr::select(d_contr_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Definite") 
p_contr_pyears <- as.data.frame((p_contr_pyears))
p_contr_pyears <- dplyr::select(p_contr_pyears, c(mean , sd, median, IQR)) %>% mutate(group = "Probable") 
all_mean_contr_pyears <- full_join(mean_contr_pyears_overall, nm_contr_pyears,
                                       by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR")) %>% 
  full_join(m_contr_pyears, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") ) %>%
  full_join(d_contr_pyears, by = c( "group" = "group", "mean" = "mean", "sd" = "sd", "median" = "median",  "IQR" = "IQR" ) ) %>%
  full_join(p_contr_pyears, by = c( "group" = "group", "mean" = "mean", "sd" = "sd" , "median" = "median", "IQR" = "IQR") )  %>%
  mutate(across(is.numeric, ~ round(.,2)))
all_mean_contr_pyears$mean_sd <- paste(all_mean_contr_pyears$mean, all_mean_contr_pyears$sd, sep =", ")
all_mean_contr_pyears$median_iqr <- paste(all_mean_contr_pyears$median, all_mean_contr_pyears$IQR, sep =", ")
all_mean_contr_pyears <- dplyr::select(all_mean_contr_pyears, c(group, mean_sd, median_iqr))
write_csv(all_mean_contr_pyears, "results/contraception_prescribing/all/baseline/8_contr_pyears_means.csv" )



## Proportion ever LARC against follow up time ---------------

contraception_cohort_final_pdcat <- contraception_cohort_final %>%
  mutate(pdcat = contr_pdays)
contraception_cohort_final_pdcat <- contraception_cohort_final_pdcat %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 0, 365), 1)) %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 366, 731), 2)) %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 732, 1097), 3)) %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 1098, 1463), 4)) %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 1464, 1829), 5)) %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 1830, 2195), 6)) %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 2196, 2561), 7)) %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 2562, 2927), 8)) %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 2928, 3293), 9)) %>%
  mutate(pdcat=replace(pdcat, between (pdcat, 3294, 3659), 10)) 
contraception_cohort_final_pdcat$pdcat <- factor(contraception_cohort_final_pdcat$pdcat, levels = c(1,2,3,4,5,6,7,8,9,10),
                                                                labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10"))



larcpresc_annual_counts_final <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Progesterone-only injectable contraceptive" | contrpresccat == "Implant" | 
                                            contrpresccat == "Hormonal intra-uterine system" |  contrpresccat ==  "Copper intra-uterine device")

larcever <- larcpresc_annual_counts_final %>% group_by(patid) %>% tally(contrpresc_n) %>% mutate(larcever = ifelse(n>0, "yes", "no"))
cohort_larcever <- full_join(contraception_cohort_final_pdcat, larcever, by = c("patid" = "patid"))
cohort_larcever$larcever <- cohort_larcever$larcever %>% replace_na("no")  
cohort_larcever$larcever <- factor(cohort_larcever$larcever, levels =  c("no", "yes"))
larcever_pdcat_all <- cohort_larcever %>% group_by(pdcat) %>% tally(larcever == "yes") %>% mutate(group = "Whole cohort")
larcever_pdcat_mvnm <- cohort_larcever %>% group_by(pdcat, migrant_status) %>% tally(larcever == "yes") %>% rename(group = migrant_status)
larcever_pdcat_all_mvnm <- full_join(larcever_pdcat_all, larcever_pdcat_mvnm, by = c("pdcat" = "pdcat", "group" = "group", "n" = "n")) 


denominator_whole <- cohort_larcever %>% group_by(pdcat) %>% tally() %>% rename (N = n) %>% mutate(group = "Whole cohort")
denominator_mvnm <- cohort_larcever %>% group_by(migrant_status, pdcat) %>% tally() %>% rename (N = n) %>% rename(group = migrant_status)



larcever_pdcat_all <- full_join(larcever_pdcat_all, denominator_whole, by = c("pdcat" = "pdcat", "group" = "group")) 
larcever_pdcat_all_percent <-   larcever_pdcat_all %>% mutate(percent = (n/N)*100)
larcever_pdcat_mvnm <- full_join(larcever_pdcat_mvnm, denominator_mvnm, by = c("pdcat" = "pdcat", "group" = "group")) 
larcever_pdcat_mvnm_percent <- larcever_pdcat_mvnm %>% mutate(percent = (n/N)*100)
larcever_pdcat_percent <- full_join(larcever_pdcat_all_percent,larcever_pdcat_mvnm_percent, 
                                    by = c("pdcat" = "pdcat", "group" = "group", "n" = "n", "N" = "N", "percent" = "percent"))
write_csv(larcever_pdcat_percent, "results/contraception_prescribing/all/baseline/larcever_pdcat_percent.csv")

# number of larc ever plot

larcever_pdcat_plot <- ggplot(larcever_pdcat_all_mvnm, aes(group = group, x = pdcat, y = n, fill = group)) + 
  geom_col(position = "dodge", width = 0.7) + 
  ggtitle("larc ever per follow up time") +
  labs(y="Number of individuals", x = "Person years at risk per patient",  color = "Key")
print(larcever_pdcat_plot)
ggsave("results/contraception_prescribing/all/baseline/larcever_pdcat_plot.jpg", width = 7, height = 5, dpi = 750)


## proportion of pop larcever plot

larcever_pdcat_percent_plot <- ggplot(larcever_pdcat_percent, aes(group = group, x = pdcat, y = percent, fill = group)) + 
  geom_col(position = "dodge", width = 0.7) + 
  ggtitle("percentage with larc ever per follow up time") +
  labs(y="Percentage of population", x = "Person years at risk",  fill = "Key")
print(larcever_pdcat_percent_plot)
ggsave("results/contraception_prescribing/all/baseline/larcever_pdcat_plot.jpg", width = 7, height = 5, dpi = 750)

## larcever across time period --------------

larcever_all <- cohort_larcever %>% tally(larcever == "yes") %>% mutate(group = "Whole cohort")
larcever_mvnm <- cohort_larcever %>% group_by( migrant_status) %>% tally(larcever == "yes") %>% rename(group = migrant_status)


denominator_whole <- cohort_larcever %>% tally() %>% rename (N = n) %>% mutate(group = "Whole cohort")
denominator_mvnm <- cohort_larcever %>% group_by(migrant_status) %>% tally() %>% rename (N = n) %>% rename(group = migrant_status)


larcever_all <- full_join(larcever_all, denominator_whole, by = c( "group" = "group")) 
larcever_all_percent <-   larcever_all %>% mutate(percent = (n/N)*100)
larcever_mvnm <- full_join(larcever_mvnm, denominator_mvnm, by = c("group" = "group")) 
larcever_mvnm_percent <- larcever_mvnm %>% mutate(percent = (n/N)*100)
larcever_percent <- full_join(larcever_all_percent,larcever_mvnm_percent, 
                                    by = c("group" = "group", "n" = "n", "N" = "N", "percent" = "percent"))
write_csv(larcever_percent, "results/contraception_prescribing/all/baseline/larcever_percent.csv")





## summary outcomes ---------- INCORRECT CODE FOR ESTABLISHING THIS - FINAL CORRECT CODE IN SPECIFIC LARC FILES


load(file = "cleaned_files/contrpresc_annual_counts_final.Rdata")

# All
total_all <- contrpresc_annual_counts_final %>%  tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Whole Cohort") %>% mutate(outcome = "All"  )
total_all_nm <- contrpresc_annual_counts_final %>% filter(migrant_status == "Non-migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Non-migrant") %>% mutate(outcome = "All"  )
total_all_m <- contrpresc_annual_counts_final %>%filter(migrant_status == "Migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Migrant") %>% mutate(outcome = "All"  )

total_larc <-  contrpresc_annual_counts_final %>%  filter(contrpresccat == "Copper intra-uterine device" |  
                                                            contrpresccat =="Hormonal intra-uterine system" |
                                                            contrpresccat =="Implant" |
                                                            contrpresccat == "Progesterone-only injectable contraceptive") %>% 
                                                          tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Whole Cohort") %>% mutate(outcome = "All LARC"  )


all_larc <- contrpresc_annual_counts_final %>%  filter(contrpresccat == "Copper intra-uterine device" |  
                                                             contrpresccat =="Hormonal intra-uterine system" |
                                                             contrpresccat =="Implant" |
                                                             contrpresccat == "Progesterone-only injectable contraceptive") %>% 
   group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "All LARC"  )



  


all <- contrpresc_annual_counts_final %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "All"  )
all_nm <-  contrpresc_annual_counts_final   %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "All")
all_m <-  contrpresc_annual_counts_final %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "All")

#


# Cuiud 
a <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Copper intra-uterine device") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "Copper intra-uterine device"  )
b <-  contrpresc_annual_counts_final  %>% filter(contrpresccat == "Copper intra-uterine device")  %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "Copper intra-uterine device")
c <-  contrpresc_annual_counts_final   %>% filter(contrpresccat == "Copper intra-uterine device") %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "Copper intra-uterine device")

# IUS

d <- contrpresc_annual_counts_final %>% filter(contrpresccat =="Hormonal intra-uterine system"  ) %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "Hormonal intra-uterine system"  )
e <-  contrpresc_annual_counts_final  %>% filter(contrpresccat == "Hormonal intra-uterine system"  )  %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome ="Hormonal intra-uterine system"  )
f <-  contrpresc_annual_counts_final   %>% filter(contrpresccat == "Hormonal intra-uterine system"  ) %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "Hormonal intra-uterine system"  )

# SDI

g<- contrpresc_annual_counts_final %>% filter(contrpresccat =="Implant"  ) %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "Implant"  )
h <-  contrpresc_annual_counts_final  %>% filter(contrpresccat == "Implant"  )  %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome ="Implant"  )
i<-  contrpresc_annual_counts_final   %>% filter(contrpresccat == "Implant"  ) %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome = "Implant"   )



# POI

j <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Progesterone-only injectable contraceptive") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise( mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Whole Cohort") %>% mutate(outcome = "Progesterone-only injectable contraceptive" )
k <-  contrpresc_annual_counts_final  %>% filter(contrpresccat == "Progesterone-only injectable contraceptive")  %>% filter(migrant_status == "Non-migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Non-migrant")  %>% mutate(outcome = "Progesterone-only injectable contraceptive" )
l <-  contrpresc_annual_counts_final   %>% filter(contrpresccat == "Progesterone-only injectable contraceptive") %>% filter(migrant_status == "Migrant") %>% group_by(patid) %>% tally(contrpresc_n) %>% rename(n_events = n) %>% 
  summarise(mean = mean(n_events), sd = sd(n_events), median = median(n_events), iqr = IQR(n_events), 
            min = min(n_events), max = max(n_events)) %>% mutate(across(is.numeric, ~ round(.,2))) %>% mutate(group = "Migrant")  %>% mutate(outcome ="Progesterone-only injectable contraceptive" )




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
  full_join(l, by = c("outcome" ="outcome", "group" ="group", "mean"="mean","sd"="sd","median"="median", "iqr"="iqr", "min"="min","max"="max"))


# total events cuiud

total_cuiud <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Copper intra-uterine device") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Whole Cohort") %>% mutate(outcome = "Copper intra-uterine device"  )
total_cuiud_nm <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Copper intra-uterine device") %>% filter(migrant_status == "Non-migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Non-migrant") %>% mutate(outcome = "Copper intra-uterine device"  )
total_cuiud_m <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Copper intra-uterine device") %>% filter(migrant_status == "Migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Migrant") %>% mutate(outcome = "Copper intra-uterine device"  )

# total events ius

total_ius <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Hormonal intra-uterine system") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Whole Cohort") %>% mutate(outcome ="Hormonal intra-uterine system" )
total_ius_nm <- contrpresc_annual_counts_final %>% filter(contrpresccat =="Hormonal intra-uterine system") %>% filter(migrant_status == "Non-migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Non-migrant") %>% mutate(outcome ="Hormonal intra-uterine system"  )
total_ius_m <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Hormonal intra-uterine system") %>% filter(migrant_status == "Migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Migrant") %>% mutate(outcome = "Hormonal intra-uterine system" )

# total events sdi

total_sdi <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Implant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Whole Cohort") %>% mutate(outcome ="Implant")
total_sdi_nm <- contrpresc_annual_counts_final %>% filter(contrpresccat =="Implant") %>% filter(migrant_status == "Non-migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Non-migrant") %>% mutate(outcome ="Implant" )
total_sdi_m <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Implant") %>% filter(migrant_status == "Migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Migrant") %>% mutate(outcome = "Implant" )

# total events poi

total_poi <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Progesterone-only injectable contraceptive") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Whole Cohort") %>% mutate(outcome ="Progesterone-only injectable contraceptive")
total_poi_nm <- contrpresc_annual_counts_final %>% filter(contrpresccat =="Progesterone-only injectable contraceptive") %>% filter(migrant_status == "Non-migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Non-migrant") %>% mutate(outcome ="Progesterone-only injectable contraceptive" )
total_poi_m <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Progesterone-only injectable contraceptive") %>% filter(migrant_status == "Migrant") %>% tally(contrpresc_n) %>% rename(n_events = n)  %>% 
  mutate(group = "Migrant") %>% mutate(outcome = "Progesterone-only injectable contraceptive")


#join events
summary_outcomes_n <- full_join(total_cuiud,total_cuiud_nm, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_cuiud_m, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_ius, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_ius_nm, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_ius_m, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_sdi, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_sdi_nm, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_sdi_m, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_poi, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_poi_nm, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) %>%
  full_join(total_poi_m, by = c("outcome" ="outcome", "group" ="group", "n_events" = "n_events" )) 


# join outcomes and events
summary_outcomes <- full_join(summary_outcomes_n, summary_outcomes_all, by = c("outcome" = "outcome", "group" = "group"))

write_csv(summary_outcomes, "results/contraception_prescribing/all/summary_outcomesl.csv")

## histograms ----------


## distribution of cuiudcounts
library(ggforce)
cuiud <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Copper intra-uterine device")
cuiud_distribution <- ggplot(cuiud) + geom_histogram(aes(x = contrpresc_n), binwidth = 1) +
  facet_zoom(xlim = c(1, 5), ylim = c(0,500)) +
  ggtitle("Distribution of Cu-IUD prescribing events") +
  labs(y="Frequency", x = "Cu-IUD prescribing events")
print(cuiud_distribution)
ggsave("results/contraception_prescribing/all/cuiud_distribution.jpg", width = 7, height = 7, dpi = 300)


## distribution of ius counts
library(ggforce)
ius <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Hormonal intra-uterine system"  )
ius_distribution <- ggplot(ius) + geom_histogram(aes(x = contrpresc_n), binwidth = 1) +
  facet_zoom(xlim = c(1, 3), ylim = c(0,5000)) +
  ggtitle("Distribution of IUS prescribing events") +
  labs(y="Frequency", x = "IUS prescribing events")
print(ius_distribution)
ggsave("results/contraception_prescribing/all/ius_distribution.jpg", width = 7, height = 7, dpi = 300)

## distribution of sdi counts
library(ggforce)
sdi <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Implant"  )
sdi_distribution <- ggplot(sdi) + geom_histogram(aes(x = contrpresc_n), binwidth = 1) +
  facet_zoom(xlim = c(1, 3), ylim = c(0,5000)) +
  ggtitle("Distribution of sdi prescribing events") +
  labs(y="Frequency", x = "SDI prescribing events")
print(sdi_distribution)
ggsave("results/contraception_prescribing/all/sdi_distribution.jpg", width = 7, height = 7, dpi = 300)

## distribution of poi counts
library(ggforce)
poi <- contrpresc_annual_counts_final %>% filter(contrpresccat == "Progesterone-only injectable contraceptive" )
poi_distribution <- ggplot(poi) + geom_histogram(aes(x = contrpresc_n), binwidth = 1) +
  facet_zoom(xlim = c(1, 12), ylim = c(0,500)) +
  ggtitle("Distribution of POI prescribing events") +
  labs(y="Frequency", x = "POI prescribing events")
print(poi_distribution)
ggsave("results/contraception_prescribing/all/poi_distribution.jpg", width = 7, height = 7, dpi = 300)

## create bar chart of rates of all LARC -----

cuiud_mig <- read_csv("results/contraception_prescribing/cuiud/univariable_mig.csv" )
cuiud_mig <- cuiud_mig %>% mutate(LARC = "Cu-IUD") %>% mutate(rate =c(404,725))
ius_mig <- read_csv("results/contraception_prescribing/ius/univariable_mig.csv" )
ius_mig <- ius_mig %>% mutate(LARC = "IUS")  %>% mutate(rate =c(1268,672))
sdi_mig <- read_csv("results/contraception_prescribing/sdi/univariable_mig.csv" )
sdi_mig <- sdi_mig %>% mutate(LARC = "SDI")  %>% mutate(rate =c(1451,1039))
poi_mig <- read_csv("results/contraception_prescribing/poi/univariable_mig.csv" )
poi_mig <- poi_mig %>% mutate(LARC = "POI")  %>% mutate(rate =c(8114,2688))

larc_mig_rates <- full_join(cuiud_mig,ius_mig, by = c("migrant_status" ="migrant_status", "events" ="events", "person_years" = "person_years",
                                                      "ir_ci" = "ir_ci", "irr_ci", "LARC" = "LARC", "rate" = "rate")) %>%
  full_join(sdi_mig, by = c("migrant_status" ="migrant_status", "events" ="events", "person_years" = "person_years",
                                      "ir_ci" = "ir_ci", "irr_ci", "LARC" = "LARC", "rate" = "rate")) %>%
  full_join(poi_mig, by = c("migrant_status" ="migrant_status", "events" ="events", "person_years" = "person_years",
                                      "ir_ci" = "ir_ci", "irr_ci", "LARC" = "LARC", "rate" = "rate"))

larc_mig_rates$LARC <- factor(larc_mig_rates$LARC, 
                           labels = c("Cu-IUD", "IUS", "POI", "SDI"))
larc_mig_rates$migrant_status <- factor(larc_mig_rates$migrant_status, 
                              labels = c("Migrant", "Non-migrant"))

larcrates_plot <- ggplot(larc_mig_rates, aes(group = migrant_status, x =LARC, y = rate, fill = migrant_status)) + 
  geom_col(position = "dodge", width = 0.7) + 
  ggtitle("LARC rates in migrants versus non-migrants") +
  labs(y="Rate of prescriptions per 100,000 Pyar", x = "LARC type",  group = "Migrant Status", color = "Migrant Status", fill = "Migrant Status")
print(larcrates_plot)
ggsave("results/contraception_prescribing/all/larcrates_plot.jpg", width = 7, height = 5, dpi = 750)


