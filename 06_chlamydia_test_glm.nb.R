####---- Description -------------------------------------------------------------------------

## Cleaning and analysing chlamydia_test data for Neha's SRHR overview chapter in females of reproductive age (15-49yo) 
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
srh_annual_counts_final$prac_region <- relevel(srh_annual_counts_final$prac_region, "London")
srh_annual_counts_final_extra$prac_region <- relevel(srh_annual_counts_final_extra$prac_region, "London")

## See separate script for cohort baseline characteristics

## SUMMARY OUTCOME MEASURES  ------------------------------------------------------------------

## Overall - whole cohort
chlamydia_test_count_overall_whole_cohort <- srh_annual_counts_final %>% tally(chlamydia_test_n) %>% rename(n_events = n)
chlamydia_test_summary_overall_whole_cohort <- srh_annual_counts_final %>% 
  summarise(mean = mean(chlamydia_test_n), sd = sd(chlamydia_test_n), median = median(chlamydia_test_n), iqr = IQR(chlamydia_test_n), 
            min = min(chlamydia_test_n), max = max(chlamydia_test_n))
chlamydia_test_count_summary_overall_whole_cohort <- merge(chlamydia_test_count_overall_whole_cohort, chlamydia_test_summary_overall_whole_cohort)
chlamydia_test_count_summary_overall_whole_cohort

## Overall by migrant status
chlamydia_test_count_overall_migrant_status <- srh_annual_counts_final %>% group_by(migrant_status) %>%
  tally(chlamydia_test_n) %>% rename(n_events = n)
chlamydia_test_summary_overall_migrant_status <- srh_annual_counts_final %>% group_by(migrant_status) %>%
  summarise(mean = mean(chlamydia_test_n), sd = sd(chlamydia_test_n), median = median(chlamydia_test_n), iqr = IQR(chlamydia_test_n), 
            min = min(chlamydia_test_n), max = max(chlamydia_test_n))
chlamydia_test_count_summary_overall_migrant_status <- left_join(chlamydia_test_count_overall_migrant_status, chlamydia_test_summary_overall_migrant_status,
                                                      by = c("migrant_status"="migrant_status" ))

## Overall by migcertainty
chlamydia_test_count_overall_migcertainty <- srh_annual_counts_final %>% group_by(migcertainty) %>%
  tally(chlamydia_test_n) %>% rename(n_events = n)
chlamydia_test_summary_overall_migcertainty <- srh_annual_counts_final %>% group_by(migcertainty) %>%
  summarise(mean = mean(chlamydia_test_n), sd = sd(chlamydia_test_n), median = median(chlamydia_test_n), iqr = IQR(chlamydia_test_n), 
            min = min(chlamydia_test_n), max = max(chlamydia_test_n))
chlamydia_test_count_summary_overall_migcertainty <- left_join(chlamydia_test_count_overall_migcertainty, chlamydia_test_summary_overall_migcertainty,
                                                    by = c("migcertainty"="migcertainty" ))

## Join for overall whole cohort + by migrant status
chlamydia_test_count_summary_overall_final <- full_join(chlamydia_test_count_summary_overall_whole_cohort, chlamydia_test_count_summary_overall_migrant_status, 
                                             by = c("n_events"="n_events", "mean" = "mean",
                                                    "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max")) %>% 
  full_join(chlamydia_test_count_summary_overall_migcertainty, by = c("n_events"="n_events", "mean" = "mean",
                                                           "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max",
                                                           "migrant_status" = "migcertainty")) 
chlamydia_test_count_summary_overall_final$migrant_status <- fct_explicit_na(chlamydia_test_count_summary_overall_final$migrant_status, na_level = "whole_cohort")
chlamydia_test_count_summary_overall_final <- chlamydia_test_count_summary_overall_final %>% relocate(migrant_status)
chlamydia_test_count_summary_overall_final

write_csv(chlamydia_test_count_summary_overall_final, "filepath")

## Annual - whole cohort 
chlamydia_test_count_annual_whole_cohort <- srh_annual_counts_final %>% group_by(eventyear) %>%
  tally(chlamydia_test_n) %>% rename(n_events = n)
chlamydia_test_summary_annual_whole_cohort <- srh_annual_counts_final %>% group_by(eventyear) %>%
  summarise(mean = mean(chlamydia_test_n), sd = sd(chlamydia_test_n), median = median(chlamydia_test_n), iqr = IQR(chlamydia_test_n), 
            min = min(chlamydia_test_n), max = max(chlamydia_test_n))
chlamydia_test_count_summary_annual_whole_cohort <- left_join(chlamydia_test_count_annual_whole_cohort, chlamydia_test_summary_annual_whole_cohort,
                                                   by = c("eventyear"="eventyear"))


## Annual - by migrant status
chlamydia_test_count_annual_migrant_status <- srh_annual_counts_final %>% group_by(migrant_status, eventyear) %>%
  tally(chlamydia_test_n) %>% rename(n_events = n)
chlamydia_test_summary_annual_migrant_status <- srh_annual_counts_final %>% group_by(migrant_status, eventyear) %>%
  summarise(mean = mean(chlamydia_test_n), sd = sd(chlamydia_test_n), median = median(chlamydia_test_n), iqr = IQR(chlamydia_test_n), 
            min = min(chlamydia_test_n), max = max(chlamydia_test_n))
chlamydia_test_count_summary_annual_migrant_status <- left_join(chlamydia_test_count_annual_migrant_status, chlamydia_test_summary_annual_migrant_status,
                                                     by = c("migrant_status"="migrant_status" , "eventyear"="eventyear"))

## Annual - by migcertainty 
chlamydia_test_count_annual_migcertainty <- srh_annual_counts_final %>% group_by(migcertainty, eventyear) %>%
  tally(chlamydia_test_n) %>% rename(n_events = n)
chlamydia_test_summary_annual_migcertainty <- srh_annual_counts_final %>% group_by(migcertainty, eventyear) %>%
  summarise(mean = mean(chlamydia_test_n), sd = sd(chlamydia_test_n), median = median(chlamydia_test_n), iqr = IQR(chlamydia_test_n), 
            min = min(chlamydia_test_n), max = max(chlamydia_test_n))
chlamydia_test_count_summary_annual_migcertainty <- left_join(chlamydia_test_count_annual_migcertainty, chlamydia_test_summary_annual_migcertainty,
                                                   by = c("migcertainty"="migcertainty" , "eventyear"="eventyear"))



## Join to give annual for whole cohort + migrant status
chlamydia_test_count_summary_annual_final <- full_join(chlamydia_test_count_summary_annual_whole_cohort, chlamydia_test_count_summary_annual_migrant_status, 
                                            by = c("eventyear" = "eventyear", "n_events"="n_events", "mean" = "mean",
                                                   "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max")) 
chlamydia_test_count_summary_annual_final <-   full_join(chlamydia_test_count_summary_annual_final, chlamydia_test_count_summary_annual_migcertainty,
                                              by = c("eventyear" = "eventyear", "n_events"="n_events", "mean" = "mean",
                                                     "sd"="sd", "median"="median", "iqr"="iqr", "min"="min", "max"="max",
                                                     "migrant_status" = "migcertainty"))

chlamydia_test_count_summary_annual_final$migrant_status <- fct_explicit_na(chlamydia_test_count_summary_annual_final$migrant_status, na_level = "whole_cohort") 
chlamydia_test_count_summary_annual_final <- chlamydia_test_count_summary_annual_final %>% 
  relocate(migrant_status)


## Export chlamydia_test summary measures
write_csv(chlamydia_test_count_summary_annual_final, "filepath")

## distribution of chlamydia_test counts
library(ggforce)
chlamydia_test_distribution <- ggplot(srh_annual_counts_final) + geom_histogram(aes(x = chlamydia_test_n), binwidth = 1) +
  facet_zoom(xlim = c(1, 8), ylim = c(0,70000)) +
  ggtitle("Distribution of chlamydia_test counts") +
  labs(y="Frequency", x = "chlamydia_test counts")
print(chlamydia_test_distribution)
ggsave("filepath", width = 7, height = 7, dpi = 300)

## yearly CT 15to 24 year olds females compared to National CT screening programme 15-24yo  females
yearly_events_1524_ncsp <- read_csv("filepath")

# all events
yearly_events_1524_ncsp$Data <- factor(yearly_events_1524_ncsp$Data, c("CPRD GOLD",  "NCSP - GP", "NCSP - Internet" , "NCSP - SRH/Specialist"))
options(scipen = 1000000)
yearly_events_1524_ncsp_plot <- ggplot(yearly_events_1524_ncsp, aes(x = eventyear, y = Events, group = Data, colour = Data )) + 
  geom_line() + 
  geom_point() +
  ggtitle("CT yearly vs NCSP") +
  labs(y="No. chlamydia testing events", x = "Year",  color = "Key") 
print(yearly_events_1524_ncsp_plot)
ggsave("filepath")

# ncsp events only
yearly_events_1524_ncsponly <- yearly_events_1524_ncsp %>% filter(Data != "CPRD GOLD")
options(scipen = 1000000)
yearly_events_1524_ncsponly_plot <- ggplot(yearly_events_1524_ncsponly, aes(x = eventyear, y = Events, group = Data, colour = Data )) + 
  geom_line() + 
  geom_point() +
  ggtitle("NCSP only") +
  labs(y="No. chlamydia testing events", x = "Year",  color = "Key") 
print(yearly_events_1524_ncsponly_plot)
ggsave("filepath")

# cprd gold events only
yearly_events_1524_cprd <- yearly_events_1524_ncsp %>% filter(Data == "CPRD GOLD")
yearly_events_1524_cprd_plot <- ggplot(yearly_events_1524_cprd, aes(x = eventyear, y = Events, colour = "red")) + 
  geom_line() + 
  geom_point() +
  ggtitle("CPRD GOLD") +
  labs(y="No. chlamydia testing events", x = "Year")
print(yearly_events_1524_cprd_plot)
ggsave("filepath")


## MAIN ANALYSIS ----------------------------------------------------------------------

## IR & univariable IRR - migrant status ----------------------------------------------------------------------

# IR by migrant status - whole cohort
pyears_mvnm_overall <- aggregate(pyears_ey ~ migrant_status, srh_annual_counts_final_extra, sum) 
pyears_mvnm_overall <- pyears_mvnm_overall %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm <- aggregate(chlamydia_test_n ~ migrant_status, srh_annual_counts_final_extra, sum) 
IR_mvnm <- inner_join(chlamydia_testcount_mvnm,pyears_mvnm_overall, by= c("migrant_status" = "migrant_status"))
IR_mvnm_output <- pois.exact(IR_mvnm$chlamydia_test_n, IR_mvnm$pyears_ey, conf.level=0.95)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(lower100=lower*100)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm[,1]
IR_mvnm_output <- cbind(IR_mvnm_output, migrant_status)
IR_mvnm_output <- IR_mvnm_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_output <- IR_mvnm_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_output$ci <- paste(IR_mvnm_output$lower100, IR_mvnm_output$upper100, sep ="-")
IR_mvnm_output$ir_ci <- paste(IR_mvnm_output$incidence_rate100, IR_mvnm_output$ci, sep =",")
IR_mvnm_output_table <-  dplyr::select(IR_mvnm_output, migrant_status, events, person_years, ir_ci)


##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = srh_annual_counts_final_extra)
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
a<- srh_annual_counts_final_extra %>% group_by(patid, eventyear_agecat) %>% 
  tally(chlamydia_test_n)
b<- srh_annual_counts_final_extra %>% group_by(patid, eventyear_agecat) %>% 
  tally(pyears_ey)
c<- full_join(a,b, by = c("patid"="patid", "eventyear_agecat" = "eventyear_agecat"))
d<- left_join(c, reproductive_cohort_final, by = c("patid" = "patid")) %>% 
  rename(chlamydia_test_n = n.x) %>% 
  rename(pyears_agecat = n.y)


# IR by age cat - whole cohort
pyears_agecat_overall <- aggregate(pyears_ey ~ eventyear_agecat, srh_annual_counts_final_extra, sum) 
pyears_agecat_overall <- pyears_agecat_overall %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_agecat <- aggregate(chlamydia_test_n ~ eventyear_agecat, srh_annual_counts_final_extra, sum) 
IR_agecat <- inner_join(chlamydia_testcount_agecat,pyears_agecat_overall, by= c("eventyear_agecat" = "eventyear_agecat"))
IR_agecat_output <- pois.exact(IR_agecat$chlamydia_test_n, IR_agecat$pyears_ey, conf.level=0.95)
IR_agecat_output <- IR_agecat_output %>%
  mutate(incidence_rate100=rate*100) %>%
  mutate(lower100=lower*100) %>%
  mutate(upper100=upper*100)
eventyear_agecat <- IR_agecat[,1]
IR_agecat_output <- cbind(IR_agecat_output, eventyear_agecat)
IR_agecat_output <- IR_agecat_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate)  
IR_agecat_output <- IR_agecat_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_agecat_output$ci <- paste(IR_agecat_output$lower100, IR_agecat_output$upper100, sep ="-")
IR_agecat_output$ir_ci <- paste(IR_agecat_output$incidence_rate100, IR_agecat_output$ci, sep =",")
IR_agecat_output_table <-  dplyr::select(IR_agecat_output, eventyear_agecat, events, person_years, ir_ci)

## Generate IRR via negative binomial regression univariabe for  year 
x <- glm.nb(chlamydia_test_n ~ as.factor(eventyear_agecat) + offset(log(pyears_agecat)), data = d)
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
pyears_prac_region_overall <- aggregate(pyears_ey ~ prac_region, srh_annual_counts_final_extra, sum) 
pyears_prac_region_overall <- pyears_prac_region_overall %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_prac_region <- aggregate(chlamydia_test_n ~ prac_region, srh_annual_counts_final_extra, sum) 
IR_prac_region <- inner_join(chlamydia_testcount_prac_region,pyears_prac_region_overall, by= c("prac_region" = "prac_region"))
IR_prac_region_output <- pois.exact(IR_prac_region$chlamydia_test_n, IR_prac_region$pyears_ey, conf.level=0.95)
IR_prac_region_output <- IR_prac_region_output %>%
  mutate(incidence_rate100=rate*100)
IR_prac_region_output <- IR_prac_region_output %>%
  mutate(lower100=lower*100)
IR_prac_region_output <- IR_prac_region_output %>%
  mutate(upper100=upper*100)
prac_region <- IR_prac_region[,1]
IR_prac_region_output <- cbind(IR_prac_region_output, prac_region)
IR_prac_region_output <- IR_prac_region_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_prac_region_output <- IR_prac_region_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_prac_region_output$ci <- paste(IR_prac_region_output$lower100, IR_prac_region_output$upper100, sep ="-")
IR_prac_region_output$ir_ci <- paste(IR_prac_region_output$incidence_rate100, IR_prac_region_output$ci, sep =",")
IR_prac_region_output_table <-  dplyr::select(IR_prac_region_output, prac_region, events, person_years, ir_ci)

## Generate IRR via negative binomial regression univariabe for prac region
x <- glm.nb(chlamydia_test_n ~ as.factor(prac_region) + offset(log(pyears_ey)), data = srh_annual_counts_final_extra)
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
pyears_imd_overall <- aggregate(pyears_ey ~ imd, srh_annual_counts_final_extra, sum) 
pyears_imd_overall <- pyears_imd_overall %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_imd <- aggregate(chlamydia_test_n ~ imd, srh_annual_counts_final_extra, sum) 
IR_imd <- inner_join(chlamydia_testcount_imd,pyears_imd_overall, by= c("imd" = "imd"))
IR_imd_output <- pois.exact(IR_imd$chlamydia_test_n, IR_imd$pyears_ey, conf.level=0.95)
IR_imd_output <- IR_imd_output %>%
  mutate(incidence_rate100=rate*100)
IR_imd_output <- IR_imd_output %>%
  mutate(lower100=lower*100)
IR_imd_output <- IR_imd_output %>%
  mutate(upper100=upper*100)
imd <- IR_imd[,1]
IR_imd_output <- cbind(IR_imd_output, imd)
IR_imd_output <- IR_imd_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_imd_output <- IR_imd_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_imd_output$ci <- paste(IR_imd_output$lower100, IR_imd_output$upper100, sep ="-")
IR_imd_output$ir_ci <- paste(IR_imd_output$incidence_rate100, IR_imd_output$ci, sep =",")
IR_imd_output_table <-  dplyr::select(IR_imd_output, imd, events, person_years, ir_ci)

## Generate IRR via negative binomial regression univariabe for imd
x <- glm.nb(chlamydia_test_n ~ as.factor(imd) + offset(log(pyears_ey)), data = srh_annual_counts_final_extra)
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
pyears_annual <- aggregate(pyears_ey ~ eventyear, srh_annual_counts_final_extra, sum) 
pyears_annual <- pyears_annual %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_test_count_annual <- aggregate(chlamydia_test_n ~ eventyear, srh_annual_counts_final_extra, sum) 
IR_annual <- inner_join(chlamydia_test_count_annual,pyears_annual, by= c("eventyear" = "eventyear"))
IR_annual_output <- pois.exact(IR_annual$chlamydia_test_n, IR_annual$pyears_ey, conf.level=0.95)
IR_annual_output <- IR_annual_output %>%
  mutate(rate100=rate*100) %>%
  mutate(lower100=lower*100) %>%
  mutate(upper100=upper*100)
eventyear <- IR_annual[,1]
IR_annual_output <- cbind(IR_annual_output, eventyear) 
IR_annual_output <- IR_annual_output %>% 
  relocate(eventyear) %>%
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) %>% 
  rename(incidence_rate_100 = rate100)
IR_annual_output <- IR_annual_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_annual_output$ci <- paste(IR_annual_output$lower100, IR_annual_output$upper100, sep ="-")
IR_annual_output$ir_ci <- paste(IR_annual_output$incidence_rate_100, IR_annual_output$ci, sep =",")
IR_annual_output_table <-  dplyr::select(IR_annual_output, eventyear, events, person_years, ir_ci)
IR_annual_output_table$eventyear <- factor(IR_annual_output$eventyear, c("2009", "2010", "2011", "2012", 
                                                                         "2013", "2014", "2015", "2016", "2017", "2018"))

## Generate IRR via negative binomial regression univariabe for  year 
x <- glm.nb(chlamydia_test_n ~ as.factor(eventyear) + offset(log(pyears_ey)), data = srh_annual_counts_final_extra)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
            data = srh_annual_counts_final_extra)
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

# FP of adjusted and fully unadjusted IRR
glm_all_mvnm_fp <- glm_all_mvnm[1:2,]
glm_all_mvnm_fp$names <- recode(glm_all_mvnm_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Migrant(adj)")
glm_adj_unadj <- full_join(glm_mig, glm_all_mvnm_fp, by = c("names" = "names", "estimate" = "estimate",
                                                            "lower" = "lower", "upper"= "upper",
                                                            "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci"))

chlamydia_test_glm_adj_unadj <- glm_adj_unadj
save(chlamydia_test_glm_adj_unadj, file = "filepath")

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
                                       title = "Chlamydia Testing",
                                       graphwidth = unit(80, 'mm'))
dev.off()


## SUBGROUP ANALYSES ----------------------------------------------------------------------

## Certainty of migration status -----------------------------------------------------------------------------

## IR & univariable IRR - migcertainy ---

## IR by migcertainty - whole cohort
pyears_dp_overall <- aggregate(pyears_ey ~ migcertainty, srh_annual_counts_final_extra, sum) 
pyears_dp_overall <- pyears_dp_overall %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_dp <- aggregate(chlamydia_test_n ~ migcertainty, srh_annual_counts_final_extra, sum) 
IR_dp <- inner_join(chlamydia_testcount_dp,pyears_dp_overall, by= c("migcertainty" = "migcertainty"))
IR_dp_output <- pois.exact(IR_dp$chlamydia_test_n, IR_dp$pyears_ey, conf.level=0.95)
IR_dp_output <- IR_dp_output %>%
  mutate(incidence_rate100=rate*100)
IR_dp_output <- IR_dp_output %>%
  mutate(lower100=lower*100)
IR_dp_output <- IR_dp_output %>%
  mutate(upper100=upper*100)
migcertainty <- IR_dp[,1]
IR_dp_output <- cbind(IR_dp_output, migcertainty)
IR_dp_output <- IR_dp_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_dp_output <- IR_dp_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_dp_output$ci <- paste(IR_dp_output$lower100, IR_dp_output$upper100, sep ="-")
IR_dp_output$ir_ci <- paste(IR_dp_output$incidence_rate100, IR_dp_output$ci, sep =",")
IR_dp_output_table <-  dplyr::select(IR_dp_output, migcertainty, events, person_years, ir_ci)


##  generate IRR by negative binomial regression
srh_annual_counts_final_extra$migcertainty <- relevel(srh_annual_counts_final_extra$migcertainty, "Non-migrant")
x <- glm.nb(chlamydia_test_n ~ as.factor(migcertainty) + offset(log(pyears_ey)), data = srh_annual_counts_final_extra)
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
srh_annual_counts_final_extra$migcertainty <- relevel(srh_annual_counts_final_extra$migcertainty, "Non-migrant")
x <- glm.nb(chlamydia_test_n ~ as.factor(migcertainty) + as.factor(eventyear) + as.factor(eventyear_agecat) +  as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)), 
            data = srh_annual_counts_final_extra)
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
glm_all_dp_fp$names <- recode(glm_all_dp_fp$names, "Non-migrants" = "Non-migrants", "Definite Migrants" = "Definite Migrants(adj)",
                              "Probable Migrants" = "Probable Migrants(adj)")
glm_dp_adj_unadj <- full_join(glm_dp, glm_all_dp_fp, by = c("names" = "names", "estimate" = "estimate",
                                                            "lower" = "lower", "upper"= "upper",
                                                            "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci"))


chlamydia_test_glm_dp_adj_unadj <- glm_dp_adj_unadj
save(chlamydia_test_glm_dp_adj_unadj, file = "filepath")

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
                                title = "Chlamydia Testing",
                                graphwidth = unit(80, 'mm'))
dev.off()


## Ethnicity -----------------------------------------------------------------------------


## white_british ---

white_british <- srh_annual_counts_final_extra %>% filter(ethnicat6 == "White British")

# IR by migrant status 
pyears_mvnm_white_british <- aggregate(pyears_ey ~ migrant_status, white_british, sum) 
pyears_mvnm_white_british <- pyears_mvnm_white_british %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_white_british<- aggregate(chlamydia_test_n ~ migrant_status, white_british, sum) 
IR_mvnm_white_british <- inner_join(chlamydia_testcount_mvnm_white_british,pyears_mvnm_white_british, by= c("migrant_status" = "migrant_status"))
IR_mvnm_white_british_output <- pois.exact(IR_mvnm_white_british$chlamydia_test_n, IR_mvnm_white_british$pyears_ey, conf.level=0.95)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>%
  mutate(lower100=lower*100)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_white_british[,1]
IR_mvnm_white_british_output <- cbind(IR_mvnm_white_british_output, migrant_status)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_white_british_output$ci <- paste(IR_mvnm_white_british_output$lower100, IR_mvnm_white_british_output$upper100, sep ="-")
IR_mvnm_white_british_output$ir_ci <- paste(IR_mvnm_white_british_output$incidence_rate100, IR_mvnm_white_british_output$ci, sep =",")
IR_mvnm_white_british_output_table <-  dplyr::select(IR_mvnm_white_british_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = white_british)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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



## white_nonbritish ---

white_nonbritish <- srh_annual_counts_final_extra %>% filter(ethnicat6 == "White Non-British")

# IR by migrant status 
pyears_mvnm_white_nonbritish <- aggregate(pyears_ey ~ migrant_status, white_nonbritish, sum) 
pyears_mvnm_white_nonbritish <- pyears_mvnm_white_nonbritish %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_white_nonbritish<- aggregate(chlamydia_test_n ~ migrant_status, white_nonbritish, sum) 
IR_mvnm_white_nonbritish <- inner_join(chlamydia_testcount_mvnm_white_nonbritish,pyears_mvnm_white_nonbritish, by= c("migrant_status" = "migrant_status"))
IR_mvnm_white_nonbritish_output <- pois.exact(IR_mvnm_white_nonbritish$chlamydia_test_n, IR_mvnm_white_nonbritish$pyears_ey, conf.level=0.95)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>%
  mutate(lower100=lower*100)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_white_nonbritish[,1]
IR_mvnm_white_nonbritish_output <- cbind(IR_mvnm_white_nonbritish_output, migrant_status)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_white_nonbritish_output$ci <- paste(IR_mvnm_white_nonbritish_output$lower100, IR_mvnm_white_nonbritish_output$upper100, sep ="-")
IR_mvnm_white_nonbritish_output$ir_ci <- paste(IR_mvnm_white_nonbritish_output$incidence_rate100, IR_mvnm_white_nonbritish_output$ci, sep =",")
IR_mvnm_white_nonbritish_output_table <-  dplyr::select(IR_mvnm_white_nonbritish_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = white_nonbritish)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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

mixed <- srh_annual_counts_final_extra %>% filter(ethnicat6 == "Mixed")

# IR by migrant status 
pyears_mvnm_mixed <- aggregate(pyears_ey ~ migrant_status, mixed, sum) 
pyears_mvnm_mixed <- pyears_mvnm_mixed %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_mixed<- aggregate(chlamydia_test_n ~ migrant_status, mixed, sum) 
IR_mvnm_mixed <- inner_join(chlamydia_testcount_mvnm_mixed,pyears_mvnm_mixed, by= c("migrant_status" = "migrant_status"))
IR_mvnm_mixed_output <- pois.exact(IR_mvnm_mixed$chlamydia_test_n, IR_mvnm_mixed$pyears_ey, conf.level=0.95)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>%
  mutate(lower100=lower*100)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_mixed[,1]
IR_mvnm_mixed_output <- cbind(IR_mvnm_mixed_output, migrant_status)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_mixed_output$ci <- paste(IR_mvnm_mixed_output$lower100, IR_mvnm_mixed_output$upper100, sep ="-")
IR_mvnm_mixed_output$ir_ci <- paste(IR_mvnm_mixed_output$incidence_rate100, IR_mvnm_mixed_output$ci, sep =",")
IR_mvnm_mixed_output_table <-  dplyr::select(IR_mvnm_mixed_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = mixed)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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

asian <- srh_annual_counts_final_extra %>% filter(ethnicat6 == "Asian/Asian British")

# IR by migrant status 
pyears_mvnm_asian <- aggregate(pyears_ey ~ migrant_status, asian, sum) 
pyears_mvnm_asian <- pyears_mvnm_asian %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_asian<- aggregate(chlamydia_test_n ~ migrant_status, asian, sum) 
IR_mvnm_asian <- inner_join(chlamydia_testcount_mvnm_asian,pyears_mvnm_asian, by= c("migrant_status" = "migrant_status"))
IR_mvnm_asian_output <- pois.exact(IR_mvnm_asian$chlamydia_test_n, IR_mvnm_asian$pyears_ey, conf.level=0.95)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>%
  mutate(lower100=lower*100)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_asian[,1]
IR_mvnm_asian_output <- cbind(IR_mvnm_asian_output, migrant_status)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_asian_output <- IR_mvnm_asian_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_asian_output$ci <- paste(IR_mvnm_asian_output$lower100, IR_mvnm_asian_output$upper100, sep ="-")
IR_mvnm_asian_output$ir_ci <- paste(IR_mvnm_asian_output$incidence_rate100, IR_mvnm_asian_output$ci, sep =",")
IR_mvnm_asian_output_table <-  dplyr::select(IR_mvnm_asian_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = asian)
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
write_csv(univariable_mig_asian, "filepath")

## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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
write_csv(glm_mvnm_asian_table, "filepath")



## black ---

black <- srh_annual_counts_final_extra %>% filter(ethnicat6 == "Black/Black British")

# IR by migrant status 
pyears_mvnm_black <- aggregate(pyears_ey ~ migrant_status, black, sum) 
pyears_mvnm_black <- pyears_mvnm_black %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_black<- aggregate(chlamydia_test_n ~ migrant_status, black, sum) 
IR_mvnm_black <- inner_join(chlamydia_testcount_mvnm_black,pyears_mvnm_black, by= c("migrant_status" = "migrant_status"))
IR_mvnm_black_output <- pois.exact(IR_mvnm_black$chlamydia_test_n, IR_mvnm_black$pyears_ey, conf.level=0.95)
IR_mvnm_black_output <- IR_mvnm_black_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_black_output <- IR_mvnm_black_output %>%
  mutate(lower100=lower*100)
IR_mvnm_black_output <- IR_mvnm_black_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_black[,1]
IR_mvnm_black_output <- cbind(IR_mvnm_black_output, migrant_status)
IR_mvnm_black_output <- IR_mvnm_black_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_black_output <- IR_mvnm_black_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_black_output$ci <- paste(IR_mvnm_black_output$lower100, IR_mvnm_black_output$upper100, sep ="-")
IR_mvnm_black_output$ir_ci <- paste(IR_mvnm_black_output$incidence_rate100, IR_mvnm_black_output$ci, sep =",")
IR_mvnm_black_output_table <-  dplyr::select(IR_mvnm_black_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = black)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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

other <- srh_annual_counts_final_extra %>% filter(ethnicat6 == "Other ethnic group")

# IR by migrant status 
pyears_mvnm_other <- aggregate(pyears_ey ~ migrant_status, other, sum) 
pyears_mvnm_other <- pyears_mvnm_other %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_other<- aggregate(chlamydia_test_n ~ migrant_status, other, sum) 
IR_mvnm_other <- inner_join(chlamydia_testcount_mvnm_other,pyears_mvnm_other, by= c("migrant_status" = "migrant_status"))
IR_mvnm_other_output <- pois.exact(IR_mvnm_other$chlamydia_test_n, IR_mvnm_other$pyears_ey, conf.level=0.95)
IR_mvnm_other_output <- IR_mvnm_other_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_other_output <- IR_mvnm_other_output %>%
  mutate(lower100=lower*100)
IR_mvnm_other_output <- IR_mvnm_other_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_other[,1]
IR_mvnm_other_output <- cbind(IR_mvnm_other_output, migrant_status)
IR_mvnm_other_output <- IR_mvnm_other_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_other_output <- IR_mvnm_other_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_other_output$ci <- paste(IR_mvnm_other_output$lower100, IR_mvnm_other_output$upper100, sep ="-")
IR_mvnm_other_output$ir_ci <- paste(IR_mvnm_other_output$incidence_rate100, IR_mvnm_other_output$ci, sep =",")
IR_mvnm_other_output_table <-  dplyr::select(IR_mvnm_other_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = other)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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
glm_mvnm_white_british_fp <- dplyr::select(glm_mvnm_white_british_fp, -c(p))
glm_mvnm_white_british_fp$names <- recode(glm_mvnm_white_british_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "White British ethnicity - Migrant(adj)")
glm_mvnm_white_nonbritish_fp <- glm_mvnm_white_nonbritish[1:2,]
glm_mvnm_white_nonbritish_fp <- dplyr::select(glm_mvnm_white_nonbritish_fp, -c(p))
glm_mvnm_white_nonbritish_fp$names <- recode(glm_mvnm_white_nonbritish_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "White Non-British ethnicity - Migrant(adj)")
glm_mvnm_mixed_fp <- glm_mvnm_mixed[1:2,]
glm_mvnm_mixed_fp <- dplyr::select(glm_mvnm_mixed_fp, -c(p))
glm_mvnm_mixed_fp$names <- recode(glm_mvnm_mixed_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Mixed ethnicity - Migrant(adj)")
glm_mvnm_asian_fp <- glm_mvnm_asian[1:2,]
glm_mvnm_asian_fp <- dplyr::select(glm_mvnm_asian_fp, -c(p))
glm_mvnm_asian_fp$names <- recode(glm_mvnm_asian_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Asian ethnicity - Migrant(adj)")
glm_mvnm_black_fp <- glm_mvnm_black[1:2,]
glm_mvnm_black_fp <- dplyr::select(glm_mvnm_black_fp, -c(p))
glm_mvnm_black_fp$names <- recode(glm_mvnm_black_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Black ethnicity - Migrant(adj)")
glm_mvnm_other_fp <- glm_mvnm_other[1:2,]
glm_mvnm_other_fp <- dplyr::select(glm_mvnm_other_fp, -c(p))
glm_mvnm_other_fp$names <- recode(glm_mvnm_other_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Other ethnicity - Migrant(adj)")
glm_mig_white_british <- dplyr::select(glm_mig_white_british, -c(p))
glm_mig_white_nonbritish <- dplyr::select(glm_mig_white_nonbritish, -c(p))
glm_mig_mixed <- dplyr::select(glm_mig_mixed, -c(p))
glm_mig_asian <- dplyr::select(glm_mig_asian, -c(p))
glm_mig_black <- dplyr::select(glm_mig_black, -c(p))
glm_mig_other <- dplyr::select(glm_mig_other, -c(p))


glm_adj_unadj<- full_join(glm_mig_white_british, glm_mvnm_white_british_fp, by = c("names" = "names", "estimate" = "estimate",
                                                                                   "lower" = "lower", "upper"= "upper",
                                                                                   "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_white_nonbritish, by = c("names" = "names", "estimate" = "estimate",
                                             "lower" = "lower", "upper"= "upper",
                                             "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_white_nonbritish_fp, by = c("names" = "names", "estimate" = "estimate",
                                                 "lower" = "lower", "upper"= "upper",
                                                 "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_mixed, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_mixed_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_asian, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_asian_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_black, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_black_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_other, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_other_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "ci" = "ci", "irr_ci" = "irr_ci")) 


chlamydia_test_glm_adj_unadj_ethnicity <- glm_adj_unadj
save(chlamydia_test_glm_adj_unadj_ethnicity, file = "filepath")


tabletext <- cbind(c("Non-migrants", "White British ethnicity - Migrants (unadjusted)", "White British ethnicity - Migrants (fully adjusted)",
                     "White Non-British ethnicity - Migrants (unadjusted)", "White Non-British ethnicity - Migrants (fully adjusted)",
                     "Mixed ethnicity - Migrants (unadjusted)", "Mixed ethnicity - Migrants (fully adjusted)",
                     "Asian/Asian British ethnicity - Migrants (unadjusted)", "Asian/Asian British ethnicity - Migrants (fully adjusted)",
                     "Black/Black British ethnicity - Migrants (unadjusted)", "Black/Black British ethnicity - Migrants (fully adjusted)",
                     "Other ethnicity - Migrants (unadjusted)", "Other ethnicity - Migrants (fully adjusted)"),
                   c(glm_adj_unadj$estimate),
                   c(glm_adj_unadj$ci))
dev.new()
png(file = "filepath", width = 800, height = 700)
irr_migrant_status_ethnicity <- forestplot(tabletext, mean = glm_adj_unadj$estimate, lower=glm_adj_unadj$lower, upper=glm_adj_unadj$upper,
                                           graph.pos = (ncol(tabletext)-1),
                                           hrzl_lines = TRUE, xlab = "IRR", zero = 1, 
                                           col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                           xticks = 0:2,
                                           ci.vertices = TRUE,
                                           boxsize = 0.05,
                                           title = "Chlamydia Testing",
                                           graphwidth = unit(80, 'mm'))
dev.off()


## SENSITIVITY ANALYSIS ----------------------

## Matched cohort 4:1 with 365D washout from data start --------------

# Load final datasets for analysis .Rdata files 

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
exact_match_srh_annual_counts_final_4to1$prac_region <- relevel(exact_match_srh_annual_counts_final_4to1$prac_region, "London")
exact_match_srh_annual_counts_final_extra_4to1$prac_region <- relevel(exact_match_srh_annual_counts_final_extra_4to1$prac_region, "London")



# IR by migrant status - whole cohort
pyears_mvnm_overall <- aggregate(pyears_ey ~ migrant_status, exact_match_srh_annual_counts_final_extra_4to1, sum) 
pyears_mvnm_overall <- pyears_mvnm_overall %>% mutate(across(is.numeric, ~ round(.,0)))
chlamydia_testcount_mvnm <- aggregate(chlamydia_test_n ~ migrant_status, exact_match_srh_annual_counts_final_extra_4to1, sum) 
IR_mvnm <- inner_join(chlamydia_testcount_mvnm,pyears_mvnm_overall, by= c("migrant_status" = "migrant_status"))
IR_mvnm_output <- pois.exact(IR_mvnm$chlamydia_test_n, IR_mvnm$pyears_ey, conf.level=0.95)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(lower100=lower*100)
IR_mvnm_output <- IR_mvnm_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm[,1]
IR_mvnm_output <- cbind(IR_mvnm_output, migrant_status)
IR_mvnm_output <- IR_mvnm_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_output <- IR_mvnm_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_output$ci <- paste(IR_mvnm_output$lower100, IR_mvnm_output$upper100, sep ="-")
IR_mvnm_output$ir_ci <- paste(IR_mvnm_output$incidence_rate100, IR_mvnm_output$ci, sep =",")
IR_mvnm_output_table <-  dplyr::select(IR_mvnm_output, migrant_status, events, person_years, ir_ci)




##  generate IRR by negative binomial regression for mig status ---

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = exact_match_srh_annual_counts_final_extra_4to1)
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
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
            data = exact_match_srh_annual_counts_final_extra_4to1)
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


## Forest plots ---

# FP of adjusted and fully unadjusted IRR
glm_all_mvnm_fp <- glm_all_mvnm[1:2,]
glm_all_mvnm_fp$names <- recode(glm_all_mvnm_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Migrant(adj)")
glm_adj_unadj <- full_join(glm_mig, glm_all_mvnm_fp, by = c("names" = "names", "estimate" = "estimate",
                                                            "lower" = "lower", "upper"= "upper",
                                                            "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci"))


chlamydia_test_glm_adj_unadj_em <- glm_adj_unadj
save(chlamydia_test_glm_adj_unadj_em, file = "filepath")

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
                                       title = "chlamydia_test (Exact Matched Cohort)",
                                       graphwidth = unit(80, 'mm'))
dev.off()


## Matched cohort- Certainty of migration status -----------------------------------------------------------------------------

## IR & univariable IRR - migcertainy ---

## IR by migcertainty - whole cohort
pyears_dp_overall <- aggregate(pyears_ey ~ migcertainty, exact_match_srh_annual_counts_final_extra_4to1, sum) 
pyears_dp_overall <- pyears_dp_overall %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_dp <- aggregate(chlamydia_test_n ~ migcertainty, exact_match_srh_annual_counts_final_extra_4to1, sum) 
IR_dp <- inner_join(chlamydia_testcount_dp,pyears_dp_overall, by= c("migcertainty" = "migcertainty"))
IR_dp_output <- pois.exact(IR_dp$chlamydia_test_n, IR_dp$pyears_ey, conf.level=0.95)
IR_dp_output <- IR_dp_output %>%
  mutate(incidence_rate100=rate*100)
IR_dp_output <- IR_dp_output %>%
  mutate(lower100=lower*100)
IR_dp_output <- IR_dp_output %>%
  mutate(upper100=upper*100)
migcertainty <- IR_dp[,1]
IR_dp_output <- cbind(IR_dp_output, migcertainty)
IR_dp_output <- IR_dp_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_dp_output <- IR_dp_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_dp_output$ci <- paste(IR_dp_output$lower100, IR_dp_output$upper100, sep ="-")
IR_dp_output$ir_ci <- paste(IR_dp_output$incidence_rate100, IR_dp_output$ci, sep =",")
IR_dp_output_table <-  dplyr::select(IR_dp_output, migcertainty, events, person_years, ir_ci)


##  generate IRR by negative binomial regression
exact_match_srh_annual_counts_final_extra_4to1$migcertainty <- relevel(exact_match_srh_annual_counts_final_extra_4to1$migcertainty, "Non-migrant")
x <- glm.nb(chlamydia_test_n ~ as.factor(migcertainty) + offset(log(pyears_ey)), data = exact_match_srh_annual_counts_final_extra_4to1)
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
exact_match_srh_annual_counts_final_extra_4to1$migcertainty <- relevel(exact_match_srh_annual_counts_final_extra_4to1$migcertainty, "Non-migrant")
x <- glm.nb(chlamydia_test_n ~ as.factor(migcertainty) + as.factor(eventyear) + as.factor(eventyear_agecat) +  as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)), 
            data = exact_match_srh_annual_counts_final_extra_4to1)
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
glm_all_dp_fp$names <- recode(glm_all_dp_fp$names, "Non-migrants" = "Non-migrants", "Definite Migrants" = "Definite Migrants(adj)",
                              "Probable Migrants" = "Probable Migrants(adj)")
glm_dp_adj_unadj <- full_join(glm_dp, glm_all_dp_fp, by = c("names" = "names", "estimate" = "estimate",
                                                            "lower" = "lower", "upper"= "upper",
                                                            "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci"))

chlamydia_test_glm_dp_adj_unadj_em <- glm_dp_adj_unadj
save(chlamydia_test_glm_dp_adj_unadj_em, file = "filepath")


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
                                title = "chlamydia_test Overall",
                                graphwidth = unit(80, 'mm'))
dev.off()







## Matched cohort - Ethnicity -----------------------------------------------------------------------------



## white_british ---

white_british <- exact_match_srh_annual_counts_final_extra_4to1 %>% filter(ethnicat6 == "White British")

# IR by migrant status 
pyears_mvnm_white_british <- aggregate(pyears_ey ~ migrant_status, white_british, sum) 
pyears_mvnm_white_british <- pyears_mvnm_white_british %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_white_british<- aggregate(chlamydia_test_n ~ migrant_status, white_british, sum) 
IR_mvnm_white_british <- inner_join(chlamydia_testcount_mvnm_white_british,pyears_mvnm_white_british, by= c("migrant_status" = "migrant_status"))
IR_mvnm_white_british_output <- pois.exact(IR_mvnm_white_british$chlamydia_test_n, IR_mvnm_white_british$pyears_ey, conf.level=0.95)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>%
  mutate(lower100=lower*100)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_white_british[,1]
IR_mvnm_white_british_output <- cbind(IR_mvnm_white_british_output, migrant_status)
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_white_british_output <- IR_mvnm_white_british_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_white_british_output$ci <- paste(IR_mvnm_white_british_output$lower100, IR_mvnm_white_british_output$upper100, sep ="-")
IR_mvnm_white_british_output$ir_ci <- paste(IR_mvnm_white_british_output$incidence_rate100, IR_mvnm_white_british_output$ci, sep =",")
IR_mvnm_white_british_output_table <-  dplyr::select(IR_mvnm_white_british_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = white_british)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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



## white_nonbritish ---

white_nonbritish <- exact_match_srh_annual_counts_final_extra_4to1 %>% filter(ethnicat6 == "White Non-British")

# IR by migrant status 
pyears_mvnm_white_nonbritish <- aggregate(pyears_ey ~ migrant_status, white_nonbritish, sum) 
pyears_mvnm_white_nonbritish <- pyears_mvnm_white_nonbritish %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_white_nonbritish<- aggregate(chlamydia_test_n ~ migrant_status, white_nonbritish, sum) 
IR_mvnm_white_nonbritish <- inner_join(chlamydia_testcount_mvnm_white_nonbritish,pyears_mvnm_white_nonbritish, by= c("migrant_status" = "migrant_status"))
IR_mvnm_white_nonbritish_output <- pois.exact(IR_mvnm_white_nonbritish$chlamydia_test_n, IR_mvnm_white_nonbritish$pyears_ey, conf.level=0.95)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>%
  mutate(lower100=lower*100)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_white_nonbritish[,1]
IR_mvnm_white_nonbritish_output <- cbind(IR_mvnm_white_nonbritish_output, migrant_status)
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_white_nonbritish_output <- IR_mvnm_white_nonbritish_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_white_nonbritish_output$ci <- paste(IR_mvnm_white_nonbritish_output$lower100, IR_mvnm_white_nonbritish_output$upper100, sep ="-")
IR_mvnm_white_nonbritish_output$ir_ci <- paste(IR_mvnm_white_nonbritish_output$incidence_rate100, IR_mvnm_white_nonbritish_output$ci, sep =",")
IR_mvnm_white_nonbritish_output_table <-  dplyr::select(IR_mvnm_white_nonbritish_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = white_nonbritish)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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

mixed <- exact_match_srh_annual_counts_final_extra_4to1 %>% filter(ethnicat6 == "Mixed")

# IR by migrant status 
pyears_mvnm_mixed <- aggregate(pyears_ey ~ migrant_status, mixed, sum) 
pyears_mvnm_mixed <- pyears_mvnm_mixed %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_mixed<- aggregate(chlamydia_test_n ~ migrant_status, mixed, sum) 
IR_mvnm_mixed <- inner_join(chlamydia_testcount_mvnm_mixed,pyears_mvnm_mixed, by= c("migrant_status" = "migrant_status"))
IR_mvnm_mixed_output <- pois.exact(IR_mvnm_mixed$chlamydia_test_n, IR_mvnm_mixed$pyears_ey, conf.level=0.95)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>%
  mutate(lower100=lower*100)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_mixed[,1]
IR_mvnm_mixed_output <- cbind(IR_mvnm_mixed_output, migrant_status)
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_mixed_output <- IR_mvnm_mixed_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_mixed_output$ci <- paste(IR_mvnm_mixed_output$lower100, IR_mvnm_mixed_output$upper100, sep ="-")
IR_mvnm_mixed_output$ir_ci <- paste(IR_mvnm_mixed_output$incidence_rate100, IR_mvnm_mixed_output$ci, sep =",")
IR_mvnm_mixed_output_table <-  dplyr::select(IR_mvnm_mixed_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = mixed)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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

asian <- exact_match_srh_annual_counts_final_extra_4to1 %>% filter(ethnicat6 == "Asian/Asian British")

# IR by migrant status 
pyears_mvnm_asian <- aggregate(pyears_ey ~ migrant_status, asian, sum) 
pyears_mvnm_asian <- pyears_mvnm_asian %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_asian<- aggregate(chlamydia_test_n ~ migrant_status, asian, sum) 
IR_mvnm_asian <- inner_join(chlamydia_testcount_mvnm_asian,pyears_mvnm_asian, by= c("migrant_status" = "migrant_status"))
IR_mvnm_asian_output <- pois.exact(IR_mvnm_asian$chlamydia_test_n, IR_mvnm_asian$pyears_ey, conf.level=0.95)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>%
  mutate(lower100=lower*100)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_asian[,1]
IR_mvnm_asian_output <- cbind(IR_mvnm_asian_output, migrant_status)
IR_mvnm_asian_output <- IR_mvnm_asian_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_asian_output <- IR_mvnm_asian_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_asian_output$ci <- paste(IR_mvnm_asian_output$lower100, IR_mvnm_asian_output$upper100, sep ="-")
IR_mvnm_asian_output$ir_ci <- paste(IR_mvnm_asian_output$incidence_rate100, IR_mvnm_asian_output$ci, sep =",")
IR_mvnm_asian_output_table <-  dplyr::select(IR_mvnm_asian_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = asian)
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
write_csv(univariable_mig_asian, "filepath")

## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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
write_csv(glm_mvnm_asian_table, "filepath")



## black ---

black <- exact_match_srh_annual_counts_final_extra_4to1 %>% filter(ethnicat6 == "Black/Black British")

# IR by migrant status 
pyears_mvnm_black <- aggregate(pyears_ey ~ migrant_status, black, sum) 
pyears_mvnm_black <- pyears_mvnm_black %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_black<- aggregate(chlamydia_test_n ~ migrant_status, black, sum) 
IR_mvnm_black <- inner_join(chlamydia_testcount_mvnm_black,pyears_mvnm_black, by= c("migrant_status" = "migrant_status"))
IR_mvnm_black_output <- pois.exact(IR_mvnm_black$chlamydia_test_n, IR_mvnm_black$pyears_ey, conf.level=0.95)
IR_mvnm_black_output <- IR_mvnm_black_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_black_output <- IR_mvnm_black_output %>%
  mutate(lower100=lower*100)
IR_mvnm_black_output <- IR_mvnm_black_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_black[,1]
IR_mvnm_black_output <- cbind(IR_mvnm_black_output, migrant_status)
IR_mvnm_black_output <- IR_mvnm_black_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_black_output <- IR_mvnm_black_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_black_output$ci <- paste(IR_mvnm_black_output$lower100, IR_mvnm_black_output$upper100, sep ="-")
IR_mvnm_black_output$ir_ci <- paste(IR_mvnm_black_output$incidence_rate100, IR_mvnm_black_output$ci, sep =",")
IR_mvnm_black_output_table <-  dplyr::select(IR_mvnm_black_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = black)
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
write_csv(univariable_mig_black,"filepath")

## Multivariable IRR by migrant status  ---
## controlling for year + agecat + imd + prac_region

## Generate IRR via negative binomial regression
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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

other <- exact_match_srh_annual_counts_final_extra_4to1 %>% filter(ethnicat6 == "Other ethnic group")

# IR by migrant status 
pyears_mvnm_other <- aggregate(pyears_ey ~ migrant_status, other, sum) 
pyears_mvnm_other <- pyears_mvnm_other %>% mutate(across(is.numeric, ~ round(.,)))
chlamydia_testcount_mvnm_other<- aggregate(chlamydia_test_n ~ migrant_status, other, sum) 
IR_mvnm_other <- inner_join(chlamydia_testcount_mvnm_other,pyears_mvnm_other, by= c("migrant_status" = "migrant_status"))
IR_mvnm_other_output <- pois.exact(IR_mvnm_other$chlamydia_test_n, IR_mvnm_other$pyears_ey, conf.level=0.95)
IR_mvnm_other_output <- IR_mvnm_other_output %>%
  mutate(incidence_rate100=rate*100)
IR_mvnm_other_output <- IR_mvnm_other_output %>%
  mutate(lower100=lower*100)
IR_mvnm_other_output <- IR_mvnm_other_output %>%
  mutate(upper100=upper*100)
migrant_status <- IR_mvnm_other[,1]
IR_mvnm_other_output <- cbind(IR_mvnm_other_output, migrant_status)
IR_mvnm_other_output <- IR_mvnm_other_output %>% 
  rename(events = x) %>%
  rename(person_years = pt) %>% 
  rename(incidence_rate = rate) 
IR_mvnm_other_output <- IR_mvnm_other_output %>% mutate(across(is.numeric, ~ round(.,2)))
IR_mvnm_other_output$ci <- paste(IR_mvnm_other_output$lower100, IR_mvnm_other_output$upper100, sep ="-")
IR_mvnm_other_output$ir_ci <- paste(IR_mvnm_other_output$incidence_rate100, IR_mvnm_other_output$ci, sep =",")
IR_mvnm_other_output_table <-  dplyr::select(IR_mvnm_other_output, migrant_status, events, person_years, ir_ci)

##  generate IRR by negative binomial regression

x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + offset(log(pyears_ey)), data = other)
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
x <- glm.nb(chlamydia_test_n ~ as.factor(migrant_status) + as.factor(eventyear) + as.factor(eventyear_agecat) + as.factor(imd) + as.factor(prac_region) + offset(log(pyears_ey)) , 
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
glm_mvnm_white_british_fp <- dplyr::select(glm_mvnm_white_british_fp, -c(p))
glm_mvnm_white_british_fp$names <- recode(glm_mvnm_white_british_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "White British ethnicity - Migrant(adj)")
glm_mvnm_white_nonbritish_fp <- glm_mvnm_white_nonbritish[1:2,]
glm_mvnm_white_nonbritish_fp <- dplyr::select(glm_mvnm_white_nonbritish_fp, -c(p))
glm_mvnm_white_nonbritish_fp$names <- recode(glm_mvnm_white_nonbritish_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "White Non-British ethnicity - Migrant(adj)")
glm_mvnm_mixed_fp <- glm_mvnm_mixed[1:2,]
glm_mvnm_mixed_fp <- dplyr::select(glm_mvnm_mixed_fp, -c(p))
glm_mvnm_mixed_fp$names <- recode(glm_mvnm_mixed_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Mixed ethnicity - Migrant(adj)")
glm_mvnm_asian_fp <- glm_mvnm_asian[1:2,]
glm_mvnm_asian_fp <- dplyr::select(glm_mvnm_asian_fp, -c(p))
glm_mvnm_asian_fp$names <- recode(glm_mvnm_asian_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Asian ethnicity - Migrant(adj)")
glm_mvnm_black_fp <- glm_mvnm_black[1:2,]
glm_mvnm_black_fp <- dplyr::select(glm_mvnm_black_fp, -c(p))
glm_mvnm_black_fp$names <- recode(glm_mvnm_black_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Black ethnicity - Migrant(adj)")
glm_mvnm_other_fp <- glm_mvnm_other[1:2,]
glm_mvnm_other_fp <- dplyr::select(glm_mvnm_other_fp, -c(p))
glm_mvnm_other_fp$names <- recode(glm_mvnm_other_fp$names, "Non-migrant" = "Non-migrant", "Migrant" = "Other ethnicity - Migrant(adj)")
glm_mig_white_british <- dplyr::select(glm_mig_white_british, -c(p))
glm_mig_white_nonbritish <- dplyr::select(glm_mig_white_nonbritish, -c(p))
glm_mig_mixed <- dplyr::select(glm_mig_mixed, -c(p))
glm_mig_asian <- dplyr::select(glm_mig_asian, -c(p))
glm_mig_black <- dplyr::select(glm_mig_black, -c(p))
glm_mig_other <- dplyr::select(glm_mig_other, -c(p))


glm_adj_unadj<- full_join(glm_mig_white_british, glm_mvnm_white_british_fp, by = c("names" = "names", "estimate" = "estimate",
                                                                                   "lower" = "lower", "upper"= "upper",
                                                                                   "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_white_nonbritish, by = c("names" = "names", "estimate" = "estimate",
                                             "lower" = "lower", "upper"= "upper",
                                             "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_white_nonbritish_fp, by = c("names" = "names", "estimate" = "estimate",
                                                 "lower" = "lower", "upper"= "upper",
                                                 "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_mixed, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_mixed_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_asian, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_asian_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_black, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_black_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(glm_mig_other, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(glm_mvnm_other_fp, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "ci" = "ci", "irr_ci" = "irr_ci")) 


chlamydia_test_glm_adj_unadj_ethnicity_em <- glm_adj_unadj
save(chlamydia_test_glm_adj_unadj_ethnicity_em, file = "filepath")

tabletext <- cbind(c("Non-migrants", "White British ethnicity - Migrants (unadjusted)", "White British ethnicity - Migrants (fully adjusted)",
                     "White Non-British ethnicity - Migrants (unadjusted)", "White Non-British ethnicity - Migrants (fully adjusted)",
                     "Mixed ethnicity - Migrants (unadjusted)", "Mixed ethnicity - Migrants (fully adjusted)",
                     "Asian/Asian British ethnicity - Migrants (unadjusted)", "Asian/Asian British ethnicity - Migrants (fully adjusted)",
                     "Black/Black British ethnicity - Migrants (unadjusted)", "Black/Black British ethnicity - Migrants (fully adjusted)",
                     "Other ethnicity - Migrants (unadjusted)", "Other ethnicity - Migrants (fully adjusted)"),
                   c(glm_adj_unadj$estimate),
                   c(glm_adj_unadj$ci))
dev.new()
png(file = "filepath", width = 800, height = 700)
irr_migrant_status_ethnicity <- forestplot(tabletext, mean = glm_adj_unadj$estimate, lower=glm_adj_unadj$lower, upper=glm_adj_unadj$upper,
                                           graph.pos = (ncol(tabletext)-1),
                                           hrzl_lines = TRUE, xlab = "IRR", zero = 1, 
                                           col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                           xticks = 0:2,
                                           ci.vertices = TRUE,
                                           boxsize = 0.05,
                                           title = "chlamydia_test Overall",
                                           graphwidth = unit(80, 'mm'))
dev.off()











