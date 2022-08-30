####---- Description -------------------------------------------------------------------------

## Secular trends and matched cohort analysis for Neha's SRHR overview chapter in females of reproductive age (15-49yo) 
## Date started: 14/20/2020
## Author: Neha Pathak

## Load packages -------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(MASS)
library(epitools)
library(forestplot)


## Set working directory ------------------------------------------------------------------

setwd("filepath")

## Load final datasets for analysis .Rdata files ------------------------------------------------------

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")



## SECULAR TRENDS --------------------------------------------------------

## Per-person pyears follow-up ----------------------------

# All patients
pyears_summary_overall_allpatients <- reproductive_cohort_final %>% 
  summarise(mean = mean(pyears), sd = sd(pyears), median = median(pyears), iqr = IQR(pyears), 
            min = min(pyears), max = max(pyears))

# All patients by migrant status
pyears_summary_mig_status_allpatients <- reproductive_cohort_final %>% group_by(migrant_status) %>%
  summarise(mean = mean(pyears), sd = sd(pyears), median = median(pyears), iqr = IQR(pyears), 
            min = min(pyears), max = max(pyears))

# All patients by migcertainty
pyears_summary_migcertainty_allpatients <- reproductive_cohort_final %>% group_by(migcertainty) %>%
  summarise(mean = mean(pyears), sd = sd(pyears), median = median(pyears), iqr = IQR(pyears), 
            min = min(pyears), max = max(pyears))

# Join for overall all patients + by migrant status
pyears_summary_overall <- full_join(pyears_summary_overall_allpatients, pyears_summary_mig_status_allpatients, 
                                    by = c("mean" = "mean",
                                           "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max")) %>%
  full_join(pyears_summary_migcertainty_allpatients, by = c( "mean" = "mean",
                                                             "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max",
                                                             "migrant_status" = "migcertainty"))

pyears_summary_overall$migrant_status <- fct_explicit_na(pyears_summary_overall$migrant_status, na_level = "Whole Cohort")


# save file
View(pyears_summary_overall)
write_csv(pyears_summary_overall, "filepath")


## Year of cohort entry  ---------------------------------

reproductive_cohort_final <- reproductive_cohort_final %>%
  mutate(year_cohort_entry = year(cohort_entry))

total_cohort_n <- count(reproductive_cohort_final)
total_mvnm_n <- reproductive_cohort_final %>% group_by(migrant_status) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migrant_status)
total_dp_n <- reproductive_cohort_final %>% group_by(migcertainty) %>% 
  count() %>% 
  rename(individuals = n) %>% 
  rename(group = migcertainty)

year_cohort_entry <- reproductive_cohort_final %>% 
  group_by(year_cohort_entry) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n$n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
year_cohort_entry_mvnm <- reproductive_cohort_final %>% 
  group_by(year_cohort_entry, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm_n, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(year_cohort_entry, group, n, percent))
year_cohort_entry_dp <- reproductive_cohort_final %>% 
  group_by(year_cohort_entry, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp_n, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(year_cohort_entry, group, n, percent))

year_cohort_entry_all <- year_cohort_entry_mvnm %>% full_join(year_cohort_entry,by = c("year_cohort_entry"="year_cohort_entry",
                                                                                          "group"="group","n"="n", "percent" = "percent")) 
 
year_cohort_entry_all <- year_cohort_entry_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, year_cohort_entry, n, percent)
year_cohort_entry_all$n_percent <- paste(year_cohort_entry_all$n, year_cohort_entry_all$percent, sep =",")

pyears_year_cohort_entry <- reproductive_cohort_final %>% group_by(year_cohort_entry) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_year_cohort_entry_mvnm <- reproductive_cohort_final %>% group_by(migrant_status, year_cohort_entry) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_year_cohort_entry_dp <- reproductive_cohort_final %>% group_by(migcertainty, year_cohort_entry) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_year_cohort_entry_all <- pyears_year_cohort_entry_mvnm %>% full_join(pyears_year_cohort_entry_dp,by = c("year_cohort_entry"="year_cohort_entry",
                                                                                                               "group"="group","n"="n")) %>% 
  full_join(pyears_year_cohort_entry,by = c("year_cohort_entry"="year_cohort_entry",
                                            "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_year_cohort_entry_export <- full_join(pyears_year_cohort_entry_all, year_cohort_entry_all, 
                                             by = c("year_cohort_entry"="year_cohort_entry","group"="group")) %>% 
  relocate(group,year_cohort_entry,n_percent,pyears)  %>% 
  arrange(group)

pyears_year_cohort_entry_export$group <- factor(pyears_year_cohort_entry_export$group, c("Whole Cohort", "Non-migrant", "Migrant" , "Definite", "Probable"))
pyears_year_cohort_entry_export$year_cohort_entry <- factor(pyears_year_cohort_entry_export$year_cohort_entry,  c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))


write_csv(pyears_year_cohort_entry_export, "filepath")

# unstacked bar chart 
library(ggplot2) 
year_cohort_entry_plot <-  ggplot(year_cohort_entry_mvnm) + 
  geom_point(aes(x= year_cohort_entry, y = percent, group = group, color = group)) +
  geom_line(aes(x= year_cohort_entry, y = percent, group = group, color = group)) +
  ggtitle("Percentage of population by year of cohort entry stratified by migration status ") +
  labs(y="Percentage of Population", x = "Year of Cohort Entry", color = "Key")
print(year_cohort_entry_plot)
ggsave(width = 8, height = 4, dpi = 450, "filepath")
write_csv(year_cohort_entry_mvnm,"filepath" )


## Year of cohort exit -------------------------------------

reproductive_cohort_final <- reproductive_cohort_final %>%
  mutate(year_cohort_exit = year(cohort_exit))

year_cohort_exit <- reproductive_cohort_final %>% 
  group_by(year_cohort_exit) %>% count() %>% 
  mutate(group = "Whole Cohort") %>% 
  mutate(percent = (n/total_cohort_n$n)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1)))
year_cohort_exit_mvnm <- reproductive_cohort_final %>% 
  group_by(year_cohort_exit, migrant_status) %>% count() %>% 
  rename(group = migrant_status) %>% 
  full_join(total_mvnm_n, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(year_cohort_exit, group, n, percent))
year_cohort_exit_dp <- reproductive_cohort_final %>% 
  group_by(year_cohort_exit, migcertainty) %>% count() %>% 
  rename(group = migcertainty) %>% 
  full_join(total_dp_n, by = c("group" = "group")) %>% 
  mutate(percent = (n/individuals)*100) %>% 
  mutate(across(is.numeric, ~ round(.,1))) %>%
  dplyr::select(c(year_cohort_exit, group, n, percent))

year_cohort_exit_all <- year_cohort_exit_mvnm %>% full_join(year_cohort_exit_dp,by = c("year_cohort_exit"="year_cohort_exit",
                                                                                       "group"="group","n"="n", "percent" = "percent")) %>% 
  full_join(year_cohort_exit,by = c("year_cohort_exit"="year_cohort_exit",
                                    "group"="group","n"="n", "percent" = "percent")) %>%
  rename(year_cohort_exit= year_cohort_exit)
year_cohort_exit_all <- year_cohort_exit_all %>% relocate(group) %>% arrange(group) %>% dplyr::select(group, year_cohort_exit, n, percent)

year_cohort_exit_all$n_percent <- paste(year_cohort_exit_all$n, year_cohort_exit_all$percent, sep =",")

pyears_year_cohort_exit <- reproductive_cohort_final %>% group_by(year_cohort_exit) %>% tally(pyears) %>% 
  mutate(group = "Whole Cohort")
pyears_year_cohort_exit_mvnm <- reproductive_cohort_final %>% group_by(migrant_status, year_cohort_exit) %>% tally(pyears) %>% 
  rename(group = migrant_status)
pyears_year_cohort_exit_dp <- reproductive_cohort_final %>% group_by(migcertainty, year_cohort_exit) %>% tally(pyears) %>% 
  rename(group = migcertainty)
pyears_year_cohort_exit_all <- pyears_year_cohort_exit_mvnm %>% full_join(pyears_year_cohort_exit_dp,by = c("year_cohort_exit"="year_cohort_exit",
                                                                                                            "group"="group","n"="n")) %>% 
  full_join(pyears_year_cohort_exit,by = c("year_cohort_exit"="year_cohort_exit",
                                           "group"="group","n"="n")) %>%
  rename(pyears = n) %>% 
  mutate(across(is.numeric, ~ round(.,0)))

pyears_year_cohort_exit_export <- full_join(pyears_year_cohort_exit_all, year_cohort_exit_all, 
                                            by = c("year_cohort_exit"="year_cohort_exit","group"="group")) %>% 
  relocate(group,year_cohort_exit,n_percent,pyears)  %>% 
  arrange(group)

pyears_year_cohort_entry_export$group <- factor(pyears_year_cohort_entry_export$group, c("Whole Cohort", "Non-migrant", "Migrant" , "Definite", "Probable"))
pyears_year_cohort_entry_export$year_cohort_entry <- factor(pyears_year_cohort_entry_export$year_cohort_entry,  c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))


write_csv(pyears_year_cohort_exit_export, "filepath")

# unstacked bar chart 
library(ggplot2) 
year_cohort_exit_plot <-  ggplot(pyears_year_cohort_exit_export) + 
  geom_col(aes(x= year_cohort_exit, y = percent, group = group, fill = group), position = "dodge",  width = 0.9) +
  ggtitle("Percentage of population by year of cohort exit stratified by migration status ") +
  labs(y="Percentage of Population", x = "Year of Cohort Exit", fill = "Key")
print(year_cohort_exit_plot)
ggsave(width = 8, height = 4, dpi = 450, "filepath")


## Summary age at cohort entry ------------

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
write_csv(all_mean_ages, "filepath")


## Summary age at cohort exit ------------

## Mean + Median + SD + IQR Age at cohort exit 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

reproductive_cohort_final_age <- reproductive_cohort_final %>%
  mutate(age_cohort_exit = calc_age(dob, cohort_exit))
mean_ages_overall <- describe(reproductive_cohort_final_age$age_cohort_exit, IQR = TRUE)
mean_ages_overall

nm <- reproductive_cohort_final_age %>% filter(migrant_status == "Non-migrant")
nm_age <- describe(nm$age_cohort_exit, IQR = TRUE)
nm_age

m <- reproductive_cohort_final_age %>% filter(migrant_status == "Migrant")
m_age <- describe(m$age_cohort_exit, IQR = TRUE)
m_age

d <- reproductive_cohort_final_age %>% filter(migcertainty == "Definite")
d_age <- describe(d$age_cohort_exit, IQR = TRUE)
d_age

p <- reproductive_cohort_final_age %>% filter(migcertainty == "Probable")
p_age <- describe(p$age_cohort_exit, IQR = TRUE)
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


##  IRs by years since cohort entry ----------------------------------------

## Derive years since cohort entry
srh_annual_counts_final <- srh_annual_counts_final %>%
  mutate(year_cohort_entry = year(cohort_entry)) %>%
  mutate(years_since_cohort_entry = (eventyear-year_cohort_entry))

# IR and 95%CI annually  migrants and non-migrants 
pyears_annual_migrant_status <- aggregate(pyears ~ migrant_status + years_since_cohort_entry, srh_annual_counts_final, sum) 
cons_count_annual_migrant_status <- aggregate(consultations_n ~ migrant_status + years_since_cohort_entry, srh_annual_counts_final, sum) 
IR_annual_migrant_status <- full_join(cons_count_annual_migrant_status, pyears_annual_migrant_status, 
                                      by = c("migrant_status" = "migrant_status", "years_since_cohort_entry"="years_since_cohort_entry"))
IR_annual_migrant_status_output <- pois.exact(IR_annual_migrant_status$consultations_n,  IR_annual_migrant_status$pyears, conf.level=0.95)
IR_annual_migrant_status_output <- IR_annual_migrant_status_output %>%
  mutate(rate100000=rate*100000) %>%
  mutate(lower100000=lower*100000) %>%
  mutate(upper100000=upper*100000)
label_annual_migrant_status <- IR_annual_migrant_status[,1:2]
IR_annual_migrant_status_output <- cbind(IR_annual_migrant_status_output, label_annual_migrant_status) 
IR_annual_migrant_status_output <- IR_annual_migrant_status_output %>% 
  relocate(migrant_status, years_since_cohort_entry) %>%
  rename(group = migrant_status)
IR_annual_migrant_status_output <- IR_annual_migrant_status_output %>% 
  rename(events = "x") %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) %>%
  rename(incidence_rate_100000 = rate100000) 
IR_annual_migrant_status_output <- IR_annual_migrant_status_output %>% arrange(desc(group), years_since_cohort_entry)

# IR and 95%CI annually definite and probable migrants 
pyears_annual_migcertainty <- aggregate(pyears ~ migcertainty + years_since_cohort_entry, srh_annual_counts_final, sum) 
cons_count_annual_migcertainty <- aggregate(consultations_n ~ migcertainty + years_since_cohort_entry, srh_annual_counts_final, sum) 
IR_annual_migcertainty <- full_join(cons_count_annual_migcertainty, pyears_annual_migcertainty , by = c("migcertainty" = "migcertainty", "years_since_cohort_entry"="years_since_cohort_entry"))
IR_annual_migcertainty_output <- pois.exact(IR_annual_migcertainty$consultations_n, IR_annual_migcertainty$pyears, conf.level=0.95)
IR_annual_migcertainty_output  <- IR_annual_migcertainty_output %>%
  mutate(rate100000=rate*100000) %>%
  mutate(lower100000=lower*100000) %>%
  mutate(upper100000=upper*100000)
label_migcertainty <- IR_annual_migcertainty[,1:2]
IR_annual_migcertainty_output <- cbind(IR_annual_migcertainty_output, label_migcertainty)
IR_annual_migcertainty_output <- IR_annual_migcertainty_output %>% 
  relocate(migcertainty, years_since_cohort_entry) %>%
  rename(group = migcertainty)
IR_annual_migcertainty_output <- IR_annual_migcertainty_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) %>% 
  rename(incidence_rate_100000 = rate100000) %>%
  filter(group == "Definite" | group == "Probable") %>% 
  arrange(group, years_since_cohort_entry)

## join IR annuals 

IR_annual_all <- IR_annual_migrant_status_output %>%  
  full_join(IR_annual_migcertainty_output,  
            by = c("group"="group", "years_since_cohort_entry" = "years_since_cohort_entry", "events"="events", "person_years"="person_years", 
                   "incidence_rate"="incidence_rate", "lower"="lower", "upper"="upper",
                   "conf.level"="conf.level", "incidence_rate_100000" = "incidence_rate_100000",
                   "lower100000" = "lower100000", "upper100000"="upper100000")) 
IR_annual_all <- IR_annual_all %>% relocate(group)
IR_annual_all <- IR_annual_all %>% mutate(across(is.numeric, ~ round(.,2)))
IR_annual_all$ci_100000 <- paste(IR_annual_all$lower100000, IR_annual_all$upper100000, sep ="-")
write_csv(IR_annual_all, "filepath")


# unstacked bar chart 
IR_annual_all$years_since_cohort_entry <- factor(IR_annual_all$years_since_cohort_entry, 
                                                 c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
library(ggplot2) 
years_since_cohort_entry_plot <-  ggplot(IR_annual_all) + 
  geom_col(aes(x= years_since_cohort_entry, y = incidence_rate_100000, group = group, fill = group), position = "dodge",  width = 0.9) +
  ggtitle("Incidence rate per 100000 person years by years since cohort entry stratified by migration status") +
  labs(y="Incidence rate per 100000 person years", x = "Years Since Cohort Entry", fill = "Key")
print(years_since_cohort_entry_plot)
ggsave(width = 8, height = 4, dpi = 450, "filepath")

## scatterplot
IR_annual_migrant_status_output  <- IR_annual_migrant_status_output %>%
  mutate(rate100=incidence_rate*100) %>%
  mutate(lower100=lower*100) %>%
  mutate(upper100=upper*100)


library(ggplot2) 
ir_since_data_start_plot <-  ggplot(IR_annual_migrant_status_output) + 
  geom_point(aes(x= years_since_data_start, y = rate100, group = group, color = group)) +
  geom_line(aes(x= years_since_data_start, y = rate100, group = group, color = group)) +
  ggtitle("Incidence rate of all-cause consultations per 100 person years since data start stratified by migration status ") +
  labs(y="Incidence rate of all-cause consultations per 100 person years", x = "Years since data start", color = "Key")
print(ir_since_data_start_plot)
ggsave(width = 8, height = 6, dpi = 450, "filepath")
write_csv(IR_annual_migrant_status_output,"filepath")



 ## IRs by years since data_start ----------------------------------------

## Derive years since data_start
srh_annual_counts_final_extra <- srh_annual_counts_final_extra %>%
  mutate(year_data_start = year(data_start)) %>%
  mutate(years_since_data_start = (eventyear-year_data_start))

# IR and 95%CI annually  migrants and non-migrants 
pyears_annual_migrant_status <- aggregate(pyears_ey ~ migrant_status + years_since_data_start, srh_annual_counts_final_extra, sum) 
cons_count_annual_migrant_status <- aggregate(consultations_n ~ migrant_status + years_since_data_start, srh_annual_counts_final_extra, sum) 
IR_annual_migrant_status <- full_join(cons_count_annual_migrant_status, pyears_annual_migrant_status, 
                                      by = c("migrant_status" = "migrant_status", "years_since_data_start"="years_since_data_start"))
IR_annual_migrant_status_output <- pois.exact(IR_annual_migrant_status$consultations_n,  IR_annual_migrant_status$pyears_ey, conf.level=0.95)
IR_annual_migrant_status_output <- IR_annual_migrant_status_output %>%
  mutate(rate100000=rate*100000) %>%
  mutate(lower100000=lower*100000) %>%
  mutate(upper100000=upper*100000)
label_annual_migrant_status <- IR_annual_migrant_status[,1:2]
IR_annual_migrant_status_output <- cbind(IR_annual_migrant_status_output, label_annual_migrant_status) 
IR_annual_migrant_status_output <- IR_annual_migrant_status_output %>% 
  relocate(migrant_status, years_since_data_start) %>%
  rename(group = migrant_status)
IR_annual_migrant_status_output <- IR_annual_migrant_status_output %>% 
  rename(events = "x") %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) %>%
  rename(incidence_rate_100000 = rate100000) 
IR_annual_migrant_status_output <- IR_annual_migrant_status_output %>% arrange(desc(group), years_since_data_start)

# IR and 95%CI annually definite and probable migrants 
pyears_annual_migcertainty <- aggregate(pyears_ey ~ migcertainty + years_since_data_start, srh_annual_counts_final_extra, sum) 
cons_count_annual_migcertainty <- aggregate(consultations_n ~ migcertainty + years_since_data_start, srh_annual_counts_final_extra, sum) 
IR_annual_migcertainty <- full_join(cons_count_annual_migcertainty, pyears_annual_migcertainty , by = c("migcertainty" = "migcertainty", "years_since_data_start"="years_since_data_start"))
IR_annual_migcertainty_output <- pois.exact(IR_annual_migcertainty$consultations_n, IR_annual_migcertainty$pyears_ey, conf.level=0.95)
IR_annual_migcertainty_output  <- IR_annual_migcertainty_output %>%
  mutate(rate100000=rate*100000) %>%
  mutate(lower100000=lower*100000) %>%
  mutate(upper100000=upper*100000)
label_migcertainty <- IR_annual_migcertainty[,1:2]
IR_annual_migcertainty_output <- cbind(IR_annual_migcertainty_output, label_migcertainty)
IR_annual_migcertainty_output <- IR_annual_migcertainty_output %>% 
  relocate(migcertainty, years_since_data_start) %>%
  rename(group = migcertainty)
IR_annual_migcertainty_output <- IR_annual_migcertainty_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) %>% 
  rename(incidence_rate_100000 = rate100000) %>%
  filter(group == "Definite" | group == "Probable") %>% 
  arrange(group, years_since_data_start)

## join IR annuals 

IR_annual_all <- IR_annual_migrant_status_output %>%  
  full_join(IR_annual_migcertainty_output,  
            by = c("group"="group", "years_since_data_start" = "years_since_data_start", "events"="events", "person_years"="person_years", 
                   "incidence_rate"="incidence_rate", "lower"="lower", "upper"="upper",
                   "conf.level"="conf.level", "incidence_rate_100000" = "incidence_rate_100000",
                   "lower100000" = "lower100000", "upper100000"="upper100000")) 
IR_annual_all <- IR_annual_all %>% relocate(group)
IR_annual_all <- IR_annual_all %>% mutate(across(is.numeric, ~ round(.,2)))
IR_annual_all$ci_100000 <- paste(IR_annual_all$lower100000, IR_annual_all$upper100000, sep ="-")
write_csv(IR_annual_all, "filepath")

# unstacked bar chart 
IR_annual_all$years_since_data_start <- factor(IR_annual_all$years_since_data_start, 
                                                 c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21"))
library(ggplot2) 
years_since_data_start_plot <-  ggplot(IR_annual_all) + 
  geom_col(aes(x= years_since_data_start, y = incidence_rate_100000, group = group, fill = group), position = "dodge",  width = 0.9) +
  ggtitle("Incidence rate per 100000 person years by years since data start stratified by migration status") +
  labs(y="Incidence rate per 100000 person years", x = "Years Since Data Start", fill = "Key")
print(years_since_data_start_plot)
ggsave(width = 8, height = 4, dpi = 450, "filepath")

# investigating anomaly in high rate at 21 years in migrants
a <- srh_annual_counts_final_extra %>% group_by(years_since_data_start, migrant_status) %>% summarise(n_distinct(patid))
# drop years with 100 or less migrants and replot
IR_annual_all_18years <- IR_annual_all %>% filter(years_since_data_start != "19" & years_since_data_start != "20" & years_since_data_start != "21"  )

library(ggplot2) 
years_since_data_start_plot <-  ggplot(IR_annual_all_18years) + 
  geom_col(aes(x= years_since_data_start, y = incidence_rate_100000, group = group, fill = group), position = "dodge",  width = 0.9) +
  ggtitle("Incidence rate per 100000 person years by years since data start stratified by migration status") +
  labs(y="Incidence rate per 100000 person years", x = "Years Since Data Start", fill = "Key")
print(years_since_data_start_plot)
ggsave(width = 8, height = 4, dpi = 450, "filepath")



# scatterpolot migrants v non-migrants - restricted to 18 years (exclude years with <100 migrants)

IR_annual_migrant_status_output  <- IR_annual_migrant_status_output %>%
  mutate(rate100=incidence_rate*100) %>%
  mutate(lower100=lower*100) %>%
  mutate(upper100=upper*100)

IR_annual_migrant_status_output_18years <- IR_annual_migrant_status_output %>% filter(years_since_data_start != "19" & years_since_data_start != "20" & years_since_data_start != "21"  )


library(ggplot2) 
ir_since_data_start_plot <-  ggplot(IR_annual_migrant_status_output) + 
  geom_point(aes(x= years_since_data_start, y = rate100, group = group, color = group)) +
  geom_line(aes(x= years_since_data_start, y = rate100, group = group, color = group)) +
  ggtitle("Rate of all-cause consultations per 100 person years since data start stratified by migration status ") +
  labs(y="Rate of all-cause consultations per 100 person years", x = "Years since data start", color = "Key")
print(ir_since_data_start_plot)
ggsave(width = 8, height = 6, dpi = 450, "filepath")
write_csv(IR_annual_migrant_status_output,"filepath" )




## Time from data_start (i.e. NOT cohort_entry) to first event ------

## Determine earliest eventyear for each patient
first_event_analysis <- srh_annual_counts_final %>%
  group_by(patid) %>%
  slice(which.min(eventyear))
# Calculate time (in years) since earliest eventyear and data_start
first_event_analysis$time_1st_event <- first_event_analysis$eventyear - year(first_event_analysis$data_start)
str(first_event_analysis$time_1st_event)

## Histogram - migrant_status
# Combined
hist_firstevent_migrant_status <- ggplot(first_event_analysis, aes(x=time_1st_event, color = migrant_status, fill = migrant_status)) + 
  geom_histogram(binwidth=1) + 
 facet_wrap(~migrant_status) + 
  ggtitle("Distribution of population by time in years of first event since each individual's data start") +
  labs(x='Time of first event since data start (years)', y = "Number of individuals")
print(hist_firstevent_migrant_status)
ggsave(width = 8, height = 4, dpi = 450, "filepath")


## Unstacked bar chart of percentage of migrants against time to 1st event 
# Combined

mvnm_n_timetofirstevent <- first_event_analysis %>% group_by(time_1st_event, migrant_status) %>% count()
mvnm_totaln_timetofirstevent <- mvnm_n_timetofirstevent %>% group_by(migrant_status) %>% tally(n) %>% rename(total_n = n)
mvnm_percentage_timetofirstevent <- left_join(mvnm_n_timetofirstevent, mvnm_totaln_timetofirstevent, by = c("migrant_status" = "migrant_status")) %>% 
  mutate(percentage = (n/total_n)*100)
write_csv(mvnm_percentage_timetofirstevent, "filepath" )
firstevent_mvnm_percent <- ggplot(mvnm_percentage_timetofirstevent) +
  geom_col(aes(x=time_1st_event, y = percentage, group = migrant_status, fill = migrant_status), position = "dodge",  width = 0.7 ) + 
  ggtitle("Distribution of population by time in years of first event since each individual's data start") +
  labs(x='Time of first event since data start (years)', y = "Percentage of individuals", fill = "Migrant status")
print(firstevent_mvnm_percent)
ggsave(width = 8, height = 4, dpi = 450, "filepath")


## Histogram - migcertainty
# Combined
hist_firstevent_migcertainty <- ggplot(first_event_analysis, aes(x=time_1st_event, color = migcertainty, fill = migcertainty)) + 
  geom_histogram(binwidth=1) + 
  facet_wrap(~migcertainty) + 
  ggtitle("Distribution of population by time in years of first event since each individual's data start") +
  labs(x='Time of first event since data start (years)', y = "Number of individuals" )
print(hist_firstevent_migcertainty)
ggsave(width = 8, height = 4, dpi = 450, "filepath")


## Summary statistics
# Summary statistics - migrant status 
first_event_analysis_table_migrant_status <- first_event_analysis %>%
  group_by(migrant_status) %>% 
  summarise(mean = mean(time_1st_event), sd = sd(time_1st_event), median = median(time_1st_event), 
            min = min(time_1st_event), max = max(time_1st_event), IQR = IQR(time_1st_event), min_date = min(data_start), min_eventyear = min(eventyear),
            max_eventyear = max(eventyear))
# Summary statistics - migcertainty
first_event_analysis_table_migcertainty <- first_event_analysis %>%
  group_by(migcertainty) %>% 
  summarise(mean = mean(time_1st_event), sd = sd(time_1st_event), median = median(time_1st_event), 
            min = min(time_1st_event), max = max(time_1st_event), IQR = IQR(time_1st_event), min_date = min(data_start), min_eventyear = min(eventyear),
            max_eventyear = max(eventyear))
# Join and export 
first_event_analysis_table_migrant_status <- first_event_analysis_table_migrant_status %>%
  rename(Group = migrant_status)
first_event_analysis_table_migcertainty <- first_event_analysis_table_migcertainty %>%
  rename(Group = migcertainty)
cons_first_event_analysis <- dplyr::union(first_event_analysis_table_migrant_status,first_event_analysis_table_migcertainty)
write_csv(cons_first_event_analysis, "filepath")


## Time from cohort entry ------------------------


## Determine earliest eventyear for each patient
first_event_analysis <- srh_annual_counts_final %>%
  group_by(patid) %>%
  slice(which.min(eventyear))
# Calculate time (in years) since earliest eventyear and cohort entry
first_event_analysis$time_1st_event <- first_event_analysis$eventyear - year(first_event_analysis$cohort_entry)
str(first_event_analysis$time_1st_event)


## Histogram - migrant_status
# Combined
hist_firstevent_migrant_status <- ggplot(first_event_analysis, aes(x=time_1st_event, color = migrant_status, fill = migrant_status)) + 
  geom_histogram(binwidth=1) + 
  facet_wrap(~migrant_status) + 
  ggtitle("Distribution of population by time in years of first event since each individual's cohort entry") +
  labs(x='Time of first event since data start (years)', y = "Number of individuals")
print(hist_firstevent_migrant_status)
ggsave(width = 8, height = 4, dpi = 450, "filepath")

# unstacked bar chart 
mvnm_n_timetofirstevent <- first_event_analysis %>% group_by(time_1st_event, migrant_status) %>% count()
mvnm_totaln_timetofirstevent <- mvnm_n_timetofirstevent %>% group_by(migrant_status) %>% tally(n) %>% rename(total_n = n)
mvnm_percentage_timetofirstevent <- left_join(mvnm_n_timetofirstevent, mvnm_totaln_timetofirstevent, by = c("migrant_status" = "migrant_status")) %>% 
  mutate(percentage = (n/total_n)*100)
write_csv(mvnm_percentage_timetofirstevent, "filepath" )
firstevent_mvnm_percent <- ggplot(mvnm_percentage_timetofirstevent) +
  geom_col(aes(x=time_1st_event, y = percentage, group = migrant_status, fill = migrant_status), position = "dodge",  width = 0.7 ) + 
  ggtitle("Distribution of population by time in years of first event since each individual's cohort entry") +
  labs(x='Time of first event since cohort entry (years)', y = "Percentage of individuals", fill = "Migrant status")
print(firstevent_mvnm_percent)
ggsave(width = 8, height = 4, dpi = 450, "filepath")



## Histogram - migcertainty
# Combined
hist_firstevent_migcertainty <- ggplot(first_event_analysis, aes(x=time_1st_event, color = migcertainty, fill = migcertainty)) + 
  geom_histogram(binwidth=1) + 
  facet_wrap(~migcertainty) + 
  ggtitle("Distribution of population by time in years of first event since each individual's cohort entry") +
  labs(x='Time of first event since data start (years)', y = "Number of individuals" )
hist_firstevent_migcertainty
ggsave(width = 8, height = 4, dpi = 450, "filepath")


## Summary statistics
# Summary statistics - migrant status 
first_event_analysis_table_migrant_status <- first_event_analysis %>%
  group_by(migrant_status) %>% 
  summarise(mean = mean(time_1st_event), sd = sd(time_1st_event), median = median(time_1st_event), 
            min = min(time_1st_event), max = max(time_1st_event), IQR = IQR(time_1st_event), min_date = min(cohort_entry), min_eventyear = min(eventyear),
            max_eventyear = max(eventyear))
# Summary statistics - migcertainty
first_event_analysis_table_migcertainty <- first_event_analysis %>%
  group_by(migcertainty) %>% 
  summarise(mean = mean(time_1st_event), sd = sd(time_1st_event), median = median(time_1st_event), 
            min = min(time_1st_event), max = max(time_1st_event), IQR = IQR(time_1st_event), min_date = min(cohort_entry), min_eventyear = min(eventyear),
            max_eventyear = max(eventyear))
# Join and export 
first_event_analysis_table_migrant_status <- first_event_analysis_table_migrant_status %>%
  rename(Group = migrant_status)
first_event_analysis_table_migcertainty <- first_event_analysis_table_migcertainty %>%
  rename(Group = migcertainty)
cons_first_event_analysis_cohort <- dplyr::union(first_event_analysis_table_migrant_status,first_event_analysis_table_migcertainty)
write_csv(cons_first_event_analysis, "filepath")


## Lewis plot ------------------------
# Code from Dan Lewer @ UCL: 
# following the observation in Lewis et al (2005) https://onlinelibrary.wiley.com/doi/abs/10.1002/pds.1115
# that incidence of various events is higher in the months after patients join an electronic database
# this code provides a function for measuring incidence stratified by time after joining a cohort

# -- function reporting incidence stratified by duration after cohort entry --

# reformat dates as integers with an arbitrary origin, e.g. 1970-01-01
# entry = date of cohort entry
# exit = date of cohort exit
# diagnosis = date of event
# interval = length of windows in same base as dates, e.g. days
# nInterval = number of intervals, such that you have *nInterval* windows of duration *interval*
# events are excluded if they are before entry, after exit, or of value NA

Lewisf <- function(entry, exit, diagnosis, interval = 30, nInterval = 30) {
  i <- 0:nInterval * interval
  e <- diagnosis - entry
  e <- e[diagnosis >= entry & diagnosis <= exit & !is.na(diagnosis)]
  e <- findInterval(e, i)
  e <- factor(e, seq_len(nInterval))
  e <- table(e)
  s <- exit - entry
  z <- sapply(i[-1], function(x) sum(s >= x)) * interval
  x <- findInterval(s, i)
  x <- factor(x, seq_len(nInterval))
  d <- s %% interval
  d <- sapply(split(d, x), sum)
  f <- rowSums(cbind(z, d))
  cbind(events = e, follow_up = f)
}

# -- example data --

n <- 1e5
d <- data.frame(id = seq_len(n), start = sample(100:500, n, replace = T))
d$end <- d$start + sample(200:3000, n, replace = T)
d$event <- d$start + rnbinom(n, 0.7, mu = 1000)

# -- do calculation --

lewis <- Lewisf(entry = d$start, exit = d$end, diagnosis = d$event)

# -- add incidence rates and poisson confidence intervals -- 

vpc <- function(x, n, base = 365 * 1000) {
  f <- function(x, n) c(x/(n/base), poisson.test(x, n/base)$conf.int[1:2])
  t(mapply(f, x, n, SIMPLIFY = T))
}
lewis <- vpc(lewis[,1], lewis[,2])

# -- plot --

xs <- seq_len(nrow(lewis))
par(xpd = NA, mar = c(5, 5, 3, 1))
plot(1, type = 'n', xlim = c(0, 30),  ylim = c(0, 900), axes = F, xlab = NA, ylab = NA)
rect(xs - 1, 0, xs, lewis[,1], col = "#FBB4AE")
arrows(xs - 0.5, lewis[,2], xs - 0.5, lewis[,3], code = 3, angle = 90, length = 0.05)
axis(1, 0:max(xs), pos = 0, labels = F, tck = -0.01)
axis(1, seq(0, 30, 5), pos = 0)
axis(2, las = 2, pos = 0)
title(ylab = 'Incidence of event per 1,000 patient-years')
title(xlab = 'Months after joining cohort')