####---- Description -------------------------------------------------------------------------

## Cleaning data for Neha's SRHR overview chapter in females of reproductive age (15-49yo) 
## Date started: 30/09/2020
## Author: Neha Pathak


## Load packages -------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Hmisc)

## Set working directory ------------------------------------------------------------------

setwd("filepath")

## Import data -----------------------------------------------------------------------

## Cohort files 
cohort_15_49 <- read_ts("filepath")
all_patients_1 <- read_csv("filepath")
all_patients_2 <- read_csv("filepath")

## Exposure events file
migration <- read_tsv("filepath")

## SRHR domains events files - abortion, contraception, gender-based violence, HIV & STIs, infertility, 
## maternal & neonatal health (not done because CPRD pregnancy register for this), reproductive cancers, 
induced_abortion <- read_tsv("filepath")
ec <- read_tsv("filepath")
dva <- read_tsv("filepath")
chlamydia_test <- read_csv("filepath")
infertility_management <- read_tsv("filepath")
cervical_screening <- read_csv("filepath")
consultations <- read_tsv("filepath")


## Covariates events files
ethnicity <- read_csv("filepath")

## Linked data 
patient_imd <- read_tsv("filepath")
practice_imd <- read_tsv("filepath")

## Explore raw data -------------------------------------------------------------------


## Cohort 
glimpse(cohort_15_49)
str(cohort_15_49)
names(cohort_15_49)
summary(cohort_15_49)
head(cohort_15_49)
tail(cohort_15_49)

## All patients 1
glimpse(all_patients_1)
names(all_patients_1)
summary(all_patients_1)
head(all_patients_1)
tail(all_patients_1)

## All patients 2
glimpse(all_patients_2)
names(all_patients_2)
summary(all_patients_2)
head(all_patients_2)
tail(all_patients_2)

## Migration
glimpse(migration)
str(migration)
names(migration)
summary(migration)
head(migration)
tail(migration)

## induced_abortion
glimpse(induced_abortion)
str(induced_abortion)
names(induced_abortion)
summary(induced_abortion)
head(induced_abortion)
tail(induced_abortion)


## ec
glimpse(ec)
str(ec)
names(ec)
summary(ec)
head(ec)
tail(ec)

## ec_presc 
glimpse(ec_presc)
str(ec_presc)
names(ec_presc)
summary(ec_presc)
head(ec_presc)
tail(ec_presc)

## dva 
glimpse(dva)
str(dva)
names(dva)
summary(dva)
head(dva)
tail(dva)

## chlamydia 
glimpse(chlamydia)
str(chlamydia)
names(chlamydia)
summary(chlamydia)
head(chlamydia)
tail(chlamydia)

## infertility_management
glimpse(infertility_management)
str(infertility_management)
names(infertility_management)
summary(infertility_management)
head(infertility_management)
tail(infertility_management)

## cin_cervical
glimpse(cin_cervical)
str(cin_cervical)
names(cin_cervical)
summary(cin_cervical)
head(cin_cervical)
tail(cin_cervical)

## ca_cervical
glimpse(ca_cervical)
str(ca_cervical)
names(ca_cervical)
summary(ca_cervical)
head(ca_cervical)
tail(ca_cervical)

## ethnicity
glimpse(ethnicity)
str(ethnicity)
names(ethnicity)
summary(ethnicity)
head(ethnicity)
tail(ethnicity)

## smoking
glimpse(smoking)
str(smoking)
names(smoking)
summary(smoking)
head(smoking)
tail(smoking)

## bmi
glimpse(bmi)
str(bmi)
names(bmi)
summary(bmi)
head(bmi)
tail(bmi)

## patient_imd
glimpse(patient_imd)
str(patient_imd)
names(patient_imd)
summary(patient_imd)
head(patient_imd)
tail(patient_imd)

## practice_imd
glimpse(practice_imd)
str(practice_imd)
names(practice_imd)
summary(practice_imd)
head(practice_imd)
tail(practice_imd)

## Visualise numeric variables as histogram

## Cohort 
par(mar = rep(2, 4))
hist(cohort_15_49$patid)
hist(cohort_15_49$pracid)
hist(cohort_15_49$toreason)
hist(cohort_15_49$data_in)
hist(cohort_15_49$hes_e_17)
hist(cohort_15_49$death_e_17)
hist(cohort_15_49$lsoa_e_17)
hist(cohort_15_49$age1)
hist(cohort_15_49$age2)
hist(cohort_15_49$age_15_49)

## All patients 1
hist(all_patients_1$patid)
hist(all_patients_1$pracid)
hist(all_patients_1$prac_region)
hist(all_patients_1$gender)
hist(all_patients_1$toreason) 
hist(all_patients_1$data_in)
hist(all_patients_1$eligible)
hist(all_patients_1$hes16)
hist(all_patients_1$death16)
hist(all_patients_1$cancer16)
hist(all_patients_1$lsoa16)
hist(all_patients_1$mh16)
hist(all_patients_1$hes_e_17)
hist(all_patients_1$death_e_17)
hist(all_patients_1$cr_e_17)
hist(all_patients_1$lsoa_e_17)
hist(all_patients_1$mh_e_17)

## All patients 2
hist(all_patients_2$patid)
hist(all_patients_2$pracid)
hist(all_patients_2$vmid)
hist(all_patients_2$gender)
hist(all_patients_2$yob)
hist(all_patients_2$mob)
hist(all_patients_2$marital)
hist(all_patients_2$famnum)
hist(all_patients_2$chsreg)
hist(all_patients_2$prescr)
hist(all_patients_2$capsup)
hist(all_patients_2$regstat)
hist(all_patients_2$reggap)
hist(all_patients_2$internal)
hist(all_patients_2$toreason)
hist(all_patients_2$accept)

## Migration 
hist(migration$patid)
hist(migration$constype)
hist(migration$consid)
hist(migration$medcode)
hist(migration$staffid)
hist(migration$episode)
hist(migration$enttype)
hist(migration$adid)
hist(migration$category)

## induced_abortion
hist(induced_abortion$patid)
hist(induced_abortion$constype)
hist(induced_abortion$consid)
hist(induced_abortion$medcode)
hist(induced_abortion$staffid)
hist(induced_abortion$episode)
hist(induced_abortion$enttype)
hist(induced_abortion$adid)
hist(induced_abortion$category)

## ec
hist(ec$patid)
hist(ec$constype)
hist(ec$consid)
hist(ec$medcode)
hist(ec$staffid)
hist(ec$episode)
hist(ec$enttype)
hist(ec$adid)
hist(ec$category)

## ec_presc 
hist(ec_presc$patid)
hist(ec_presc$consid)
hist(ec_presc$prodcode)
hist(ec_presc$staffid)
hist(ec_presc$bnfcode)
hist(ec_presc$qty)
hist(ec_presc$numdays)
hist(ec_presc$numpacks)
hist(ec_presc$packtype)
hist(ec_presc$issueseq)
hist(ec_presc$category)

## dva 
hist(dva$patid)
hist(dva$constype)
hist(dva$consid)
hist(dva$medcode)
hist(dva$staffid)
hist(dva$episode)
hist(dva$enttype)
hist(dva$adid)
hist(dva$category)

## chlamydia
hist(chlamydia$patid)
hist(chlamydia$constype)
hist(chlamydia$consid)
hist(chlamydia$medcode)
hist(chlamydia$staffid)
hist(chlamydia$episode)
hist(chlamydia$enttype)
hist(chlamydia$adid)
hist(chlamydia$category)

## infertility_management
hist(infertility_management$patid)
hist(infertility_management$constype)
hist(infertility_management$consid)
hist(infertility_management$medcode)
hist(infertility_management$staffid)
hist(infertility_management$episode)
hist(infertility_management$enttype)
hist(infertility_management$adid)
hist(infertility_management$category)

## cin_cervical
hist(cin_cervical$patid)
hist(cin_cervical$constype)
hist(cin_cervical$consid)
hist(cin_cervical$medcode)
hist(cin_cervical$staffid)
hist(cin_cervical$episode)
hist(cin_cervical$enttype)
hist(cin_cervical$adid)
hist(cin_cervical$category)

## ca_cervical
hist(ca_cervical$patid)
hist(ca_cervical$constype)
hist(ca_cervical$consid)
hist(ca_cervical$medcode)
hist(ca_cervical$staffid)
hist(ca_cervical$episode)
hist(ca_cervical$enttype)
hist(ca_cervical$adid)
hist(ca_cervical$category)

## ethnicity
hist(ethnicity$patid)
hist(ethnicity$constype)
hist(ethnicity$consid)
hist(ethnicity$medcode)
hist(ethnicity$staffid)
hist(ethnicity$episode)
hist(ethnicity$enttype)
hist(ethnicity$adid)
hist(ethnicity$category)

## patient_imd
hist(patient_imd$patid)
hist(patient_imd$pracid)
hist(patient_imd$imd2015_5)

## practice_imd
hist(practice_imd$pracid)
hist(practice_imd$e2015_imd_5)

## Tidy data  -----------------------------------------------------------------------

## Cohort 15_49 ---

## Check for duplicate observations (whole row and patid) 
n_distinct(cohort_15_49) == count(cohort_15_49)
n_distinct(cohort_15_49) == n_distinct(distinct(cohort_15_49, patid, .keep_all = TRUE)) 

## Change variables to the correct data type based on CPRD GOLD data specification & mapping
str(cohort_15_49)
tidy_cohort_15_49 <-  cohort_15_49 %>% 
  mutate_at(vars(c(patid, pracid, toreason, data_in, hes_e_17, death_e_17, lsoa_e_17, age_15_49)), 
            funs(as.integer))
tidy_cohort_15_49$tod <- as_date(tidy_cohort_15_49$tod)
tidy_cohort_15_49$deathdate <- as_date(tidy_cohort_15_49$deathdate)
tidy_cohort_15_49$gender <- factor(tidy_cohort_15_49$gender, levels = c(0,1,2,3,4), 
                                   labels = c("Data Not Entered", "Male", "Female", "Indeterminate", "Unknown"))
tidy_cohort_15_49$toreason <- factor(tidy_cohort_15_49$toreason, levels = c(0:34), labels = c("Data Not Entered",
                                                                                              "Death", "Removal to new TP/HB/CSA", "Internal Transfer", "Mental Hospital", "Embarkation", 
                                                                                              "New TP/HB/CSA/Same GP", "Adopted Child", "Services", "Deduction at GP's Request", 
                                                                                              "Registration Cancelled","Service Dependant",  "Deduction at Patient's Request", "Other reason",
                                                                                              "Enlistment", "Institution", "Transfer within Practice", "Linkage", "Untraced - Miscellaneous", 
                                                                                              "Untraced - Immig", "Untraced - GP Resign", "Untraced - College", "Untraced - outwith Practice", 
                                                                                              "Untraced - outwith HB", "Multiple Transfer", "Intra-consortium transfer", "Returned Undelivered",
                                                                                              "Internal Transfer - Address Change", "Internal Transfer within Partnership", 
                                                                                              "Correspondence states gone away", "Practice advise outside their area", 
                                                                                              "Practice advise patient no longer resident", "Practive advise removal via screening system",
                                                                                              "Practice advise removal via vaccination data", "Removal from Residential Institute"))


str(tidy_cohort_15_49)
glimpse(tidy_cohort_15_49)
levels(tidy_cohort_15_49$gender)
levels(tidy_cohort_15_49$toreason)

## Find missing values
any(is.na(tidy_cohort_15_49))
sum(is.na(tidy_cohort_15_49))
summary(is.na(tidy_cohort_15_49)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_cohort_15_49)
summary(tidy_cohort_15_49$toreason)

## Visualise numeric variables for outliers and obvious errors with boxplots and histograms
boxplot(tidy_cohort_15_49$patid, xlab = "patid")
boxplot(tidy_cohort_15_49$pracid, xlab = "pracid")
boxplot(tidy_cohort_15_49$data_in, xlab = "data_in")
boxplot(tidy_cohort_15_49$hes_e_17, xlab = "hes_e_17")
boxplot(tidy_cohort_15_49$death_e_17, xlab = "death_e_17")
boxplot(tidy_cohort_15_49$lsoa_e_17, xlab = "lsoa_e_17")
boxplot(tidy_cohort_15_49$age1, xlab = "age1")
boxplot(tidy_cohort_15_49$age2, xlab = "age2")
boxplot(tidy_cohort_15_49$age_15_49, xlab = "age_15_49")
hist(tidy_cohort_15_49$patid, xlab = "patid")
hist(tidy_cohort_15_49$pracid, xlab = "pracid")
hist(tidy_cohort_15_49$data_in, xlab = "data_in")
hist(tidy_cohort_15_49$hes_e_17, xlab = "hes_e_17")
hist(tidy_cohort_15_49$death_e_17, xlab = "death_e_17")
hist(tidy_cohort_15_49$lsoa_e_17, xlab = "lsoa_e_17")
hist(tidy_cohort_15_49$age1, xlab = "age1")
hist(tidy_cohort_15_49$age2, xlab = "age2")
hist(tidy_cohort_15_49$age_15_49, xlab = "age_15_49")


## All patients ---

## Join both all patients files
all_patients <- inner_join(all_patients_1, all_patients_2, by = c("patid" = "patid", "pracid" = "pracid", "gender" = "gender", 
                                                                  "frd" = "frd", "crd" = "crd", "tod" = "tod", "toreason" = "toreason", "deathdate" = "deathdate"))

## Check for duplicates (whole row and patid only)
n_distinct(all_patients) == count(all_patients) 
n_distinct(distinct(all_patients, patid)) == count(all_patients)

## Check new variables and classes
glimpse(all_patients)

## Change variables to the correct data type based on CPRD GOLD data specification & mapping
tidy_all_patients <-  all_patients %>% mutate_if(is.numeric, as.integer) 
tidy_all_patients$prac_region <- factor(tidy_all_patients$prac_region, levels = c(1:13), labels = c("North East",
                                                                                                    "North West", "Yorkshire & The Humber", "East Midlands", "West Midlands", "East of England", 
                                                                                                    "South West", "South Central", "London", "South East Coast", "Northern Ireland", "Scotland", "Wales"))
tidy_all_patients$gender <- factor(tidy_all_patients$gender, levels = c(0,1,2,3,4), 
                                   labels = c("Data Not Entered", "Male", "Female", "Indeterminate", "Unknown"))
tidy_all_patients$toreason <- factor(tidy_all_patients$toreason, levels = c(0:34), labels = c("Data Not Entered",
                                                                                              "Death", "Removal to new TP/HB/CSA", "Internal Transfer", "Mental Hospital", "Embarkation", 
                                                                                              "New TP/HB/CSA/Same GP", "Adopted Child", "Services", "Deduction at GP's Request", 
                                                                                              "Registration Cancelled","Service Dependant",  "Deduction at Patient's Request", "Other reason",
                                                                                              "Enlistment", "Institution", "Transfer within Practice", "Linkage", "Untraced - Miscellaneous", 
                                                                                              "Untraced - Immig", "Untraced - GP Resign", "Untraced - College", "Untraced - outwith Practice", 
                                                                                              "Untraced - outwith HB", "Multiple Transfer", "Intra-consortium transfer", "Returned Undelivered",
                                                                                              "Internal Transfer - Address Change", "Internal Transfer within Partnership", 
                                                                                              "Correspondence states gone away", "Practice advise outside their area", 
                                                                                              "Practice advise patient no longer resident", "Practive advise removal via screening system",
                                                                                              "Practice advise removal via vaccination data", "Removal from Residential Institute"))
tidy_all_patients$data_in <- factor(tidy_all_patients$data_in, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$eligible <- factor(tidy_all_patients$eligible, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$hes16<- factor(tidy_all_patients$hes16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$death16 <- factor(tidy_all_patients$death16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$cancer16 <- factor(tidy_all_patients$cancer16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$lsoa16 <- factor(tidy_all_patients$lsoa16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$mh16 <- factor(tidy_all_patients$mh16, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$hes_e_17 <- factor(tidy_all_patients$hes_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$death_e_17 <- factor(tidy_all_patients$death_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$cr_e_17 <- factor(tidy_all_patients$cr_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$lsoa_e_17 <- factor(tidy_all_patients$lsoa_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$mh_e_17 <- factor(tidy_all_patients$mh_e_17, levels = c(0, 1), labels = c("No", "Yes"))
tidy_all_patients$mob <- factor(tidy_all_patients$mob, levels = c(0:12), labels = c("Data Not Entered",
                                                                                    "January", "February", "March", "April", "May", "June", "July", "August", "September", 
                                                                                    "October", "November", "December"))
tidy_all_patients$marital <- factor(tidy_all_patients$marital, levels = c(0:11), labels = c("Data Not Entered",
                                                                                            "Single", "Married", "Widowed", "Divorced", "Separated", "Unknown", "Engaged", "Co-habiting", "Remarried", 
                                                                                            "Stable relationship", "Civil partnership"))
tidy_all_patients$chsreg <- factor(tidy_all_patients$chsreg, levels = c(0,1, 2), labels = c("Data Not Entered", "Yes", "No"))
tidy_all_patients$prescr <- factor(tidy_all_patients$prescr, levels = c(0:21), labels = c("Data Not Entered",
                                                                                          "Under 16 years of age", "16, 17 or 18 and in full-time education", "Woman aged 60 or over", "Man aged 60 or over",
                                                                                          "Has a maternity/medical exemption certificate", "Has a prescription prepayment certificate", 
                                                                                          "Receives Income Support/Family credit et", "Has a War Pension exemption certificate", "Not Exempt",
                                                                                          "Get Disability Working Allowance", "Receives Income-based Jobseeker's Allowance", 
                                                                                          "Is named on a current HC2 charges certificate", "Was prescribed a free-of-charge contraceptive", 
                                                                                          "Has a maternity exemption certificate", "Has a medical exemption certificate", "Receives Income Support", 
                                                                                          "Has WTFC exemption or gets full or reduced WFTC", "Has DPTC exemption or get full or reduced DPTC", 
                                                                                          "Aged 60 or over", "Entitled to/has a valid NHS Tax Credit Exemption Certificate", 
                                                                                          "Has a partner who gets Pension Credit guarantee credit PCGC"))
tidy_all_patients$capsup<- factor(tidy_all_patients$capsup, levels = c(0,1,2,3,4), 
                                  labels = c("Data Not Entered", "Low", "Medium", "High", "Not Applicable"))
tidy_all_patients$accept<- factor(tidy_all_patients$accept, levels = c(0,1), 
                                  labels = c("unacceptable", "acceptable"))

## Check correct coercion of variables
glimpse(tidy_all_patients)

## Check factors correctly labelled
levels(tidy_all_patients$prac_region)
levels(tidy_all_patients$gender)
levels(tidy_all_patients$toreason)
levels(tidy_all_patients$data_in)
levels(tidy_all_patients$eligible)
levels(tidy_all_patients$hes16)
levels(tidy_all_patients$death16)
levels(tidy_all_patients$cancer16)
levels(tidy_all_patients$lsoa16)
levels(tidy_all_patients$mh16)
levels(tidy_all_patients$hes_e_17)
levels(tidy_all_patients$death_e_17)
levels(tidy_all_patients$cr_e_17)
levels(tidy_all_patients$lsoa_e_17)
levels(tidy_all_patients$mh_e_17)
levels(tidy_all_patients$mob)
levels(tidy_all_patients$marital)
levels(tidy_all_patients$chsreg)
levels(tidy_all_patients$prescr)
levels(tidy_all_patients$capsup)
levels(tidy_all_patients$toreason)
levels(tidy_all_patients$accept)

## Find missing values
any(is.na(tidy_all_patients))
sum(is.na(tidy_all_patients))
summary(is.na(tidy_all_patients))

## Look at summary of all variables for outliers and obvious errors
summary(tidy_all_patients)

## Look at summary of categorical variables for outliers and obvious errors where did not print in whole summary
summary(tidy_all_patients$mob)
summary(tidy_all_patients$marital)
summary(tidy_all_patients$prescr)
summary(tidy_all_patients$toreason)
summary(tidy_all_patients$prac_region)

## Visualise numeric ariables for outliers and obvious errors with boxplots
boxplot(tidy_all_patients$patid, xlab = "patid")
boxplot(tidy_all_patients$pracid, xlab = "pracid")
boxplot(tidy_all_patients$dob, xlab = "dob")
boxplot(tidy_all_patients$yob, xlab = "yob")
boxplot(tidy_all_patients$famnum, xlab = "famnum")
boxplot(tidy_all_patients$regstat, xlab = "regstat")
boxplot(tidy_all_patients$reggap, xlab = "reggap")
boxplot(tidy_all_patients$internal, xlab = "internal")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_all_patients$patid)
hist(tidy_all_patients$pracid)
hist(tidy_all_patients$vmid)
hist(tidy_all_patients$yob)
hist(tidy_all_patients$famnum)
hist(tidy_all_patients$regstat)
hist(tidy_all_patients$reggap)
hist(tidy_all_patients$internal)

## Migration ---

## Remove duplicates

## Remove whole row duplicates
n_distinct(migration) == count(migration)
distinct_migration <- migration %>% distinct()

## Remove duplicates based on patid + medcode + date as likely clinical coding errors
n_distinct(distinct_migration) == count(distinct(distinct_migration, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_migration_2 <- distinct_migration %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)

## dplyr::select index event for duplicates based on patid and medcode (i.e. same details but coded > 1 time) 
distinct_migration_3 <- distinct_migration_2 %>% arrange(eventdate) %>% distinct(patid, medcode, .keep_all = TRUE)

## Drop unneeded variables
tidy_migration <-  dplyr::select(distinct_migration_3, -c(sysdate, constype, episode, enttype, 
                                                   adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping and check
tidy_migration <-  tidy_migration %>% 
  mutate_if(is.numeric, as.integer) 
tidy_migration$category <- factor(tidy_migration$category, levels = c(1,2,3,4), 
                                  labels = c("Non-UK origin", "Born outside of the UK", "First/main language not English", 
                                             "Visa status indicating migration"))
glimpse(tidy_migration)
levels(tidy_migration$category)

## Find missing values - nil
any(is.na(tidy_migration))
sum(is.na(tidy_migration))
summary(is.na(tidy_migration)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_migration)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_migration$patid, xlab = "patid")
boxplot(tidy_migration$consid, xlab = "consid")
boxplot(tidy_migration$medcode, xlab = "medcode")
boxplot(tidy_migration$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_migration$patid)
hist(tidy_migration$consid)
hist(tidy_migration$medcode)
hist(tidy_migration$staffid)

## SRHR outcomes ---

## Induced abortion ---

## Remove duplicates
n_distinct(induced_abortion) == count(induced_abortion)
distinct_induced_abortion <- induced_abortion %>% distinct()
n_distinct(distinct_induced_abortion) == count(distinct(distinct_induced_abortion, patid, eventdate, medcode, constype, consid, category,  .keep_all = TRUE)) 
distinct_induced_abortion_2 <- distinct_induced_abortion %>% distinct(patid, eventdate, medcode,constype, consid,  category,  .keep_all = TRUE)

## Drop unneeded variables
str(distinct_induced_abortion_2)
tidy_induced_abortion <-  dplyr::select(distinct_induced_abortion_2, -c(sysdate,episode, enttype, 
                                                                 adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification and check
str(tidy_induced_abortion)
tidy_induced_abortion <-  tidy_induced_abortion %>% 
  mutate_if(is.numeric, as.integer) 
tidy_induced_abortion$category <- factor(tidy_induced_abortion$category, levels = c(1,2,3), 
                                         labels = c("Definite", "Probable", "Possible"))
tidy_induced_abortion$constype <- factor(tidy_induced_abortion$constype, levels = c(0:7),
                                         labels = c("Missing", "Symptom", "Examination", "Diagnosis", "Intervention", 
                                                    "Management", "Administration", "Presenting complaint"))
str(tidy_induced_abortion)
levels(tidy_induced_abortion$category)
levels(tidy_induced_abortion$constype)

## Find missing values
any(is.na(tidy_induced_abortion))
sum(is.na(tidy_induced_abortion))
summary(is.na(tidy_induced_abortion))

## Drop records with missing event dates
tidy_induced_abortion <- tidy_induced_abortion %>% filter(!is.na(eventdate)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_induced_abortion)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_induced_abortion$patid, xlab = "patid")
boxplot(tidy_induced_abortion$constype, xlab = "constype")
boxplot(tidy_induced_abortion$consid, xlab = "consid")
boxplot(tidy_induced_abortion$medcode, xlab = "medcode")
boxplot(tidy_induced_abortion$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_induced_abortion$patid)
hist(tidy_induced_abortion$constype)
hist(tidy_induced_abortion$consid)
hist(tidy_induced_abortion$medcode)
hist(tidy_induced_abortion$medcode)

## ec ---

## Remove duplicates
n_distinct(ec) == count(ec)
distinct_ec <- ec %>% distinct()
n_distinct(distinct_ec) == count(distinct(distinct_ec, patid, eventdate, medcode, constype, consid, category,  .keep_all = TRUE)) 
distinct_ec_2 <- distinct_ec %>% distinct(patid, eventdate, medcode, constype, consid,  category,  .keep_all = TRUE)

## Drop unneeded variables
str(distinct_ec_2)
tidy_ec <-  dplyr::select(distinct_ec_2, -c(sysdate,episode, enttype, 
                                     adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
str(tidy_ec)
tidy_ec <-  tidy_ec %>% 
  mutate_if(is.numeric, as.integer) 
tidy_ec$category <- factor(tidy_ec$category, levels = c(1,2,3,4,5), 
                           labels = c("H/O", "Oral emergency contraception consultation", "Intra-uterine emergency contraception consultation", 
                                      "Emergency contraception consultation(not specified or general advice only)", 
                                      "Emergency contraception failure consultation" ))
tidy_ec$constype <- factor(tidy_ec$constype, levels = c(0:7),
                           labels = c("Missing", "Symptom", "Examination", "Diagnosis", "Intervention", 
                                      "Management", "Administration", "Presenting complaint"))

## Check correct coercion of variable classes
str(tidy_ec)
levels(tidy_ec$category)
levels(tidy_ec$constype)

## Find missing values
any(is.na(tidy_ec))
sum(is.na(tidy_ec))
summary(is.na(tidy_ec)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_ec)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_ec$patid, xlab = "patid")
boxplot(tidy_ec$constype, xlab = "constype")
boxplot(tidy_ec$consid, xlab = "consid")
boxplot(tidy_ec$medcode, xlab = "medcode")
boxplot(tidy_ec$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_ec$patid)
hist(tidy_ec$constype)
hist(tidy_ec$consid)
hist(tidy_ec$medcode)
hist(tidy_ec$medcode)


## dva ---

## Remove duplicates
n_distinct(dva) == count(dva)
distinct_dva <- dva %>% distinct()
n_distinct(distinct_dva) == count(distinct(distinct_dva, patid, eventdate, medcode, constype, consid, category,  .keep_all = TRUE)) 
distinct_dva_2 <- distinct_dva %>% distinct(patid, eventdate, medcode, constype, consid,  category,  .keep_all = TRUE)

## Drop unneeded variables
str(distinct_dva_2)
tidy_dva <-  dplyr::select(distinct_dva_2, -c(sysdate,episode, enttype, 
                                       adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
str(tidy_dva)
tidy_dva <-  tidy_dva %>% 
  mutate_if(is.numeric, as.integer) 
summary(tidy_dva$category)
dva_errors <- tidy_dva %>% filter(category == 2)
unique(dva_errors$medcode) 
tidy_dva <- tidy_dva %>% mutate(category = 1)
summary(tidy_dva$category)
tidy_dva$category <- factor(tidy_dva$category, levels = c(1), 
                            labels = c("Domestic violence and abuse"))
tidy_dva$constype <- factor(tidy_dva$constype, levels = c(0:7),
                            labels = c("Missing", "Symptom", "Examination", "Diagnosis", "Intervention", 
                                       "Management", "Administration", "Presenting complaint"))

## Check correct coercion of variable classes
str(tidy_dva)
levels(tidy_dva$category)
levels(tidy_dva$constype)

## Find missing values - none
any(is.na(tidy_dva))
sum(is.na(tidy_dva))
summary(is.na(tidy_dva))

## Look at summary of all variables for outliers and obvious errors
summary(tidy_dva)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_dva$patid, xlab = "patid")
boxplot(tidy_dva$constype, xlab = "constype")
boxplot(tidy_dva$consid, xlab = "consid")
boxplot(tidy_dva$medcode, xlab = "medcode")
boxplot(tidy_dva$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_dva$patid)
hist(tidy_dva$constype)
hist(tidy_dva$consid)
hist(tidy_dva$medcode)
hist(tidy_dva$medcode)

## chlamydia_test ---

## Remove duplicates
n_distinct(chlamydia_test) == count(chlamydia_test)
distinct_chlamydia_test <- chlamydia_test %>% distinct()
n_distinct(distinct_chlamydia_test) == count(distinct(distinct_chlamydia_test, patid, eventdate, medcode, constype, consid, category,  .keep_all = TRUE)) 
distinct_chlamydia_test_2 <- distinct_chlamydia_test %>% distinct(patid, eventdate, medcode, constype, consid,  category,  .keep_all = TRUE)

## Drop unneeded variables
str(distinct_chlamydia_test_2)
tidy_chlamydia_test <-  dplyr::select(distinct_chlamydia_test_2, -c(sysdate,episode, enttype, 
                                                   adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
str(tidy_chlamydia_test)
tidy_chlamydia_test <-  tidy_chlamydia_test %>% 
  mutate_if(is.numeric, as.integer) 
tidy_chlamydia_test$category <- factor(tidy_chlamydia_test$category, levels = c(1:5), 
                                  labels = c("Chlamydial infection/test positive", "Chlamydia test negative", "Chlamydia test NOS",
                                             "Possible chlamydia infection", "Chlamydia test declined"))
tidy_chlamydia_test$constype <- factor(tidy_chlamydia_test$constype, levels = c(0:7),
                                  labels = c("Missing", "Symptom", "Examination", "Diagnosis", "Intervention", 
                                             "Management", "Administration", "Presenting complaint"))

## Check correct coercion of variable classes
str(tidy_chlamydia_test)
levels(tidy_chlamydia_test$category)
levels(tidy_chlamydia_test$constype)

## Find missing values
any(is.na(tidy_chlamydia_test))  
sum(is.na(tidy_chlamydia_test)) 
summary(is.na(tidy_chlamydia_test)) 

## Drop records with missing event dates
tidy_chlamydia_test <- tidy_chlamydia_test %>% filter(!is.na(eventdate)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_chlamydia_test)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_chlamydia_test$patid, xlab = "patid")
boxplot(tidy_chlamydia_test$constype, xlab = "constype")
boxplot(tidy_chlamydia_test$consid, xlab = "consid")
boxplot(tidy_chlamydia_test$medcode, xlab = "medcode")
boxplot(tidy_chlamydia_test$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_chlamydia_test$patid)
hist(tidy_chlamydia_test$constype)
hist(tidy_chlamydia_test$consid)
hist(tidy_chlamydia_test$medcode)
hist(tidy_chlamydia_test$medcode)

## infertility_management ---

## Remove duplicates
n_distinct(infertility_management) == count(infertility_management)
distinct_infertility_management <- infertility_management %>% distinct()
n_distinct(distinct_infertility_management) == count(distinct(distinct_infertility_management, patid, eventdate, medcode, constype, consid,  category, .keep_all = TRUE)) 
distinct_infertility_management_2 <- distinct_infertility_management %>% distinct(patid, eventdate, medcode, constype, consid,  category, .keep_all = TRUE)

## Drop unneeded variables
str(distinct_infertility_management_2)
tidy_infertility_management <-  dplyr::select(distinct_infertility_management_2, -c(sysdate,episode, enttype, 
                                                                             adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
str(tidy_infertility_management)
tidy_infertility_management <-  tidy_infertility_management %>% 
  mutate_if(is.numeric, as.integer) 
tidy_infertility_management$category <- factor(tidy_infertility_management$category, levels = c(1,2,3,4), 
                                               labels = c("In vitro fertilisation", "Other assisted reproductive treatments", 
                                                          "Infertility investigations", "Referral to infertility clinic"))
tidy_infertility_management$constype <- factor(tidy_infertility_management$constype, levels = c(0:7),
                                               labels = c("Missing", "Symptom", "Examination", "Diagnosis", "Intervention", 
                                                          "Management", "Administration", "Presenting complaint"))

## Check correct coercion of variable classes
str(tidy_infertility_management)
levels(tidy_infertility_management$category)
levels(tidy_infertility_management$constype)

## Find missing values
any(is.na(tidy_infertility_management))
sum(is.na(tidy_infertility_management))
summary(is.na(tidy_infertility_management)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_infertility_management)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_infertility_management$patid, xlab = "patid")
boxplot(tidy_infertility_management$constype, xlab = "constype")
boxplot(tidy_infertility_management$consid, xlab = "consid")
boxplot(tidy_infertility_management$medcode, xlab = "medcode")
boxplot(tidy_infertility_management$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_infertility_management$patid)
hist(tidy_infertility_management$constype)
hist(tidy_infertility_management$consid)
hist(tidy_infertility_management$medcode)
hist(tidy_infertility_management$medcode)

## cervical_screening ---

## Remove duplicates
n_distinct(cervical_screening) == count(cervical_screening)
distinct_cervical_screening <- cervical_screening %>% distinct()
n_distinct(distinct_cervical_screening) == count(distinct(distinct_cervical_screening, patid, eventdate, medcode, constype, consid,  category, .keep_all = TRUE)) 
distinct_cervical_screening_2 <- distinct_cervical_screening %>% distinct(patid, eventdate, medcode, constype, consid,  category, .keep_all = TRUE)

## Drop unneeded variables
str(distinct_cervical_screening_2)
tidy_cervical_screening <-  dplyr::select(distinct_cervical_screening_2, -c(sysdate,episode, enttype, 
                                                         adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
str(tidy_cervical_screening)
tidy_cervical_screening <-  tidy_cervical_screening %>% 
  mutate_if(is.numeric, as.integer) 
summary(tidy_cervical_screening$category)
cervical_screening_errors <- tidy_cervical_screening %>% filter(category == 3)
unique(cervical_screening_errors$medcode)
tidy_cervical_screening<- tidy_cervical_screening %>% mutate(category =  ifelse(category == 3 & medcode == 10478, 2, 
                                                      ifelse(category == 3 & medcode == 28220, 1, category)))
summary(tidy_cervical_screening$category)
tidy_cervical_screening$category <- factor(tidy_cervical_screening$category, levels = c(1,2), 
                                     labels = c("Definite", "Offered"))
tidy_cervical_screening$constype <- factor(tidy_cervical_screening$constype, levels = c(0:7),
                                     labels = c("Missing", "Symptom", "Examination", "Diagnosis", "Intervention", 
                                                "Management", "Administration", "Presenting complaint"))

## Check correct coercion of variable classes
str(tidy_cervical_screening)
levels(tidy_cervical_screening$category)
levels(tidy_cervical_screening$constype)

## Find missing values
any(is.na(tidy_cervical_screening))
sum(is.na(tidy_cervical_screening)) 
summary(is.na(tidy_cervical_screening)) 

## Drop records with missing event dates
tidy_cervical_screening <- tidy_cervical_screening %>% filter(!is.na(eventdate)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_cervical_screening)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_cervical_screening$patid, xlab = "patid")
boxplot(tidy_cervical_screening$constype, xlab = "constype")
boxplot(tidy_cervical_screening$consid, xlab = "consid")
boxplot(tidy_cervical_screening$medcode, xlab = "medcode")
boxplot(tidy_cervical_screening$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_cervical_screening$patid)
hist(tidy_cervical_screening$constype)
hist(tidy_cervical_screening$consid)
hist(tidy_cervical_screening$medcode)
hist(tidy_cervical_screening$medcode)

## consultations ---
consultations$eventdate <- as_date(consultations$eventdate)

# restrict to 2009 onwards early to reduce size of file 
consultations_2009 <- consultations %>% filter(eventdate >= "2009-01-01")

## Remove duplicates
n_distinct(consultations_2009) == count(consultations_2009) 
n_distinct(consultations_2009) == count(distinct(consultations_2009, patid, eventdate, constype, staffid, consid,  .keep_all = TRUE)) 

## Drop unneeded variables
str(consultations_2009)
tidy_consultations <-  dplyr::select(consultations_2009,-c(sysdate, duration))

# Change variables to the correct data type based on CPRD GOLD data specification & mapping
str(tidy_consultations)
tidy_consultations <-  consultations %>%
  mutate_if(is.numeric, as.integer)
summary(tidy_consultations$constype)
cons_errors <- tidy_consultations %>% filter(constype == 61)
count(cons_errors)
tidy_consultations$constype <- factor(tidy_consultations$constype, levels = c(0:60),
                                 labels = c("Data not entered", "Clinic", "Night visit, deputising service", "Follow-up/routine visit", "Night visit, local rota", "Mail from patient", "Night visit, practice",
                                            "Out of hours, practice", "Out of hours, non-practice", "Surgery consultation", "Telephone call from a patient", "Acute visit", "Discharge details",
                                            "Letter from outpatients", "Repeat issue", "Other", "Results recording", "Mail to patient", "Emergency consultation",
                                            "Administration", "Casualty attendance", "Telephone call to a patient", "Third party consultation", "Hospital admission", "Children's home visit",
                                            "Day case report", "GOS18 report", "Home visit", "Hotel visit", "NHS direct report", "Nursing home visit",
                                            "Residential home visit", "Twilight visit", "Triage", "Walk-in centre", "Co-op telephone advice", "Co-op surgery consultation",
                                            "Co-op home visit", "Minor injury service", "Medicine management", "Community clinic", "Community nursing note", "Community nursing report",
                                            "Data transferred from other system", "Health authority entry", "Health visitor note", "Health visitor report", "Hospital inpatient report", "Initial post discharge review",
                                            "Laboratory request", "Night visit", "Radiology request", "Radiology result", "Referral letter", "Social services report",
                                            "Telephone consultation", "Template entry", "GP to GP communication transaction", "Non-consultation medication data", "Non-consultation data", "ePharmacy message"))

## Check correct coercion of variable classes
str(tidy_consultations)
levels(tidy_consultations$constype)

## Find missing values
any(is.na(tidy_consultations))
sum(is.na(tidy_consultations))
summary(is.na(tidy_consultations))

## Drop records with missing event dates and constypes
tidy_consultations <- tidy_consultations %>% filter(!is.na(eventdate)) 
tidy_consultations <- tidy_consultations %>% filter(!is.na(constype)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_consultations)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_consultations$patid, xlab = "patid")
boxplot(tidy_consultations$constype, xlab = "constype")
boxplot(tidy_consultations$consid, xlab = "consid")
boxplot(tidy_consultations$medcode, xlab = "medcode")
boxplot(tidy_consultations$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_consultations$patid)
hist(tidy_consultations$constype)
hist(tidy_consultations$consid)
hist(tidy_consultations$medcode)
hist(tidy_consultations$medcode)

## ethnicity ---

# Remove duplicate rows
# n_distinct(ethnicity) == count(ethnicity)
distinct_ethnicity <- ethnicity %>% distinct()

# Remove duplicates of combined patid + medcode + date (likely clinical coding errors)
# n_distinct(distinct_ethnicity) == count(distinct(distinct_ethnicity, patid, eventdate, medcode, category,  .keep_all = TRUE))
distinct_ethnicity_2 <- distinct_ethnicity %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)

# Remove duplicate medcodes for unique patids by dplyr::selecting first recorded medcode event
distinct_ethnicity_3 <- distinct_ethnicity_2 %>% arrange(eventdate) %>% distinct(patid, medcode, .keep_all = TRUE)

# Drop unneeded variables
tidy_ethnicity <-  dplyr::select(distinct_ethnicity_3, -c(sysdate, constype, episode, enttype,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_ethnicity <-  tidy_ethnicity %>% 
  mutate_if(is.numeric, as.integer) 
tidy_ethnicity$category <- factor(tidy_ethnicity$category, levels = c(1:19), 
                                  labels = c("White British", "White Irish", "Gypsy or Irish Traveller", "Other White", 
                                             "Mixed White and Black Caribbean", "Mixed White and Black African","Mixed White and Asian","Other Mixed",
                                              "Indian", "Pakistani", "Bangladeshi", "Chinese", "Other Asian", 
                                             "Black African", "Black Caribbean", "Other Black",  "Arab", "Any other ethnic group", 
                                             "Ethnic group not specified"))

## Check correct coercion of variable classes
glimpse(tidy_ethnicity)
levels(tidy_ethnicity$category)

## Find missing values
any(is.na(tidy_ethnicity))
sum(is.na(tidy_ethnicity))
summary(is.na(tidy_ethnicity))

## Look at summary of all fields for outliers and obvious errors
summary(tidy_ethnicity)

## Visualise numeric and date fields for outliers and obvious errors with boxplots
boxplot(tidy_ethnicity$patid, xlab = "patid")
boxplot(tidy_ethnicity$consid, xlab = "consid")
boxplot(tidy_ethnicity$medcode, xlab = "medcode")
boxplot(tidy_ethnicity$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_ethnicity$patid)
hist(tidy_ethnicity$consid)
hist(tidy_ethnicity$medcode)
hist(tidy_ethnicity$staffid)

## patient_imd ---

## Check for any duplicate based on whole row
n_distinct(patient_imd) == count(patient_imd) 

## Check for any duplicates based on combined patid and pracid
n_distinct(patient_imd) == count(distinct(patient_imd, patid,pracid,  .keep_all = TRUE)) 

## all variables need so no need to drop any 

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_patient_imd <-  patient_imd %>% 
  mutate_if(is.numeric, as.integer) 
tidy_patient_imd$imd2015_5 <- factor(tidy_patient_imd$imd2015_5, levels = c(1:5), 
                                     labels = c("IMD 1", "IMD 2", "IMD 3", "IMD 4","IMD 5" ))

## Check correct coercion of variable classes
glimpse(tidy_patient_imd)

## Check factors correctly labelled
levels(tidy_patient_imd$imd2015_5)

## Find missing values
any(is.na(tidy_patient_imd))
sum(is.na(tidy_patient_imd))
summary(is.na(tidy_patient_imd))

## Look at summary of all fields for outliers and obvious errors
summary(tidy_patient_imd)

## Visualise numeric fields for outliers and obvious errors with boxplots
boxplot(tidy_patient_imd$patid, xlab = "patid")
boxplot(tidy_patient_imd$pracid, xlab = "pracid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_patient_imd$patid)
hist(tidy_patient_imd$pracid)

## practice_imd ---

## Check for any duplicate based on whole row
n_distinct(practice_imd) == count(practice_imd) 

## Check for any duplicates based on practice id
n_distinct(practice_imd) == count(distinct(practice_imd, pracid,  .keep_all = TRUE)) 

## drop unneeded variables: imd for NI, scotland, walers (beacuse not provided by CPRD)
tidy_practice_imd <- dplyr::select(practice_imd, -c(country, ni2017_imd_5,s2016_imd_5, w2014_imd_5))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_practice_imd$pracid <-  as.integer(tidy_practice_imd$pracid)
tidy_practice_imd$e2015_imd_5 <- factor(tidy_practice_imd$e2015_imd_5, levels = c(1:5), 
                                        labels = c("IMD 1", "IMD 2", "IMD 3", "IMD 4","IMD 5" ))

## Check correct coercion of variable classes
glimpse(tidy_practice_imd)

## Check factors correctly labelled
levels(tidy_practice_imd$e2015_imd_5)

## Find missing values - none
any(is.na(tidy_practice_imd))
sum(is.na(tidy_practice_imd))
summary(is.na(tidy_practice_imd)) 

## Look at summary of all fields for outliers and obvious errors
summary(tidy_practice_imd)

## Visualise numeric fields for outliers and obvious errors with boxplots
boxplot(tidy_practice_imd$pracid, xlab = "pracid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_practice_imd)

## dplyr::select relevant variables -----------------------------------------------

cohort_15_49_clean <- dplyr::select(tidy_cohort_15_49, c(patid, pracid, prac_lcd, prac_uts, gender, dob, frd, crd, tod, deathdate, data_start, data_end, age1, age2))
all_patients_clean <- dplyr::select(tidy_all_patients, c(patid, pracid, prac_region, gender, dob, yob, data_start, data_end, frd, crd, marital, prac_uts, prac_lcd, deathdate, tod))
migration_clean <- dplyr::select(tidy_migration, c(patid, category, medcode))
induced_abortion_clean <- tidy_induced_abortion
ec_clean <- tidy_ec
dva_clean <- tidy_dva
chlamydia_test_clean <- tidy_chlamydia_test
infertility_management_clean <- tidy_infertility_management
cervical_screening_clean <- tidy_cervical_screening
consultations_clean <- dplyr::select(tidy_consultations, c(patid, eventdate, constype))
ethnicity_clean <- dplyr::select(tidy_ethnicity, c(patid, eventdate, category))
patient_imd_clean <- tidy_patient_imd
practice_imd_clean <- tidy_practice_imd

## Cohort creation -------------------------------------------------------------------
## Define cohort entry and exit dates, limit study period to 01-01-2009 to 31/12/2018, limit age 15-49 years during active data

## cohort_15_49_clean ---

## Drop patients whose data_end is before 01/01/2009 and data_start after 31/12/2018 - to restrict to one decade of analysis 
cohort_15_49_clean  <- cohort_15_49_clean  %>%
  filter(data_end >= as.Date("2009-01-01") & data_start <= as.Date("2018-12-31"))

## create cohort with 365 day washout and then use this data set after this section to create cohort_15_49_washout_clean
cohort_washout <- cohort_15_49_clean %>%  mutate(data_start_2 = data_start+365)

## Calc age at study period start, age turned 15m age turned 50 and age at data start/end 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}
cohort_15_49_clean_age_study_start_end <- cohort_15_49_clean %>%
  mutate(age_study_start = calc_age(dob, as.Date("2009-01-01"))) %>% 
  mutate(age_study_end = calc_age(dob, as.Date("2018-12-31")))
cohort_15_49_clean_age_study_start_end_15_50 <- cohort_15_49_clean_age_study_start_end %>% 
  mutate(age_15 = dob+years(15)) %>% 
  mutate(age_50 = dob+years(50))
cohort_15_49_clean_age_study_start_end_15_50_age_data_start_end <- cohort_15_49_clean_age_study_start_end_15_50 %>% 
  mutate(age_data_start = calc_age(dob, data_start)) %>% 
  mutate(age_data_end = calc_age(dob, data_end))

## Cohort entry - restricting to 1/1/2009 - 31/12/2018 & age anywhere from 15 to 49yo during the study period
cohort_15_49_clean_cohort_entry <- cohort_15_49_clean_age_study_start_end_15_50_age_data_start_end %>% 
  mutate(cohort_entry = ifelse(data_start <= as.Date("2009-01-01") & data_end > as.Date("2009-01-01") & age_study_start >= 15 & age_study_start < 50, as.Date("2009-01-01"),
                               ifelse( data_start > as.Date("2009-01-01") & age_data_start >= 15 & age_data_start < 50 , data_start,
                                       ifelse(data_start > as.Date("2009-01-01") & age_data_start < 15 & age_15 < data_end, age_15,
                                              ifelse(data_start <= as.Date("2009-01-01") & data_end >= as.Date("2009-01-01") & age_study_start < 15 & age_15 < data_end, age_15, NA)))))
cohort_15_49_clean_cohort_entry$cohort_entry <- as_date(cohort_15_49_clean_cohort_entry$cohort_entry)
summary(cohort_15_49_clean_cohort_entry$cohort_entry)
## Drop patients that never enter the cohort
cohort_15_49_clean_cohort_entry_nonas <- cohort_15_49_clean_cohort_entry %>% filter(!is.na(cohort_entry))
summary(cohort_15_49_clean_cohort_entry_nonas$cohort_entry)

## Cohort exit- restricting to 1/1/2009 - 31/12/2018 & age anywhere from 15 to 49yo during the study period
cohort_15_49_clean_cohort_entry_nonas_exit <- cohort_15_49_clean_cohort_entry_nonas %>% 
  mutate(cohort_exit = ifelse(data_end <=  as.Date("2018-12-31") & age_data_end < 50, data_end, 
                              ifelse(data_end <=  as.Date("2018-12-31") & (age_50-1) < data_end, (age_50-1),
                              ifelse(data_end > as.Date("2018-12-31") & age_study_end < 50, as.Date("2018-12-31"),
                                     ifelse(data_end > as.Date("2018-12-31") & (age_50-1) < as.Date("2018-12-31"), (age_50-1), NA)))))
cohort_15_49_clean_cohort_entry_nonas_exit$cohort_exit <- as_date(cohort_15_49_clean_cohort_entry_nonas_exit$cohort_exit)
summary(cohort_15_49_clean_cohort_entry_nonas_exit$cohort_exit) 

## Calc cohort person years/days 

## Adding person year and person days
cohort_15_49_clean_cohort_entry_nonas_exit_pyrs_pdays <- cohort_15_49_clean_cohort_entry_nonas_exit %>%
  mutate(pdays = (cohort_exit-cohort_entry)+1) %>%
  mutate(pyears = pdays/365.25)
cohort_15_49_clean_cohort_entry_nonas_exit_pyrs_pdays$pyears <- as.numeric(cohort_15_49_clean_cohort_entry_nonas_exit_pyrs_pdays$pyears)
cohort_15_49_clean_cohort_entry_nonas_exit_pyrs_pdays$pdays <- as.numeric(cohort_15_49_clean_cohort_entry_nonas_exit_pyrs_pdays$pdays)

cohort_15_49_clean <- cohort_15_49_clean_cohort_entry_nonas_exit_pyrs_pdays 

## Cohort creation with 12 month (365 day) washout ---------------------------------

## Drop all patients that have a data_start_2 after data_end due to addition of washout 
cohort_washout_eligible <- cohort_washout %>% filter(data_start_2 < data_end)

## Calc age at study period start, age turned 15m age turned 50 and age at data start/end 
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}
cohort_washout_age_study_start_end <- cohort_washout_eligible %>%
  mutate(age_study_start = calc_age(dob, as.Date("2009-01-01"))) %>% 
  mutate(age_study_end = calc_age(dob, as.Date("2018-12-31")))
cohort_washout_age_study_start_end_15_50 <- cohort_washout_age_study_start_end %>% 
  mutate(age_15 = dob+years(15)) %>% 
  mutate(age_50 = dob+years(50))
cohort_washout_age_study_start_end_15_50_age_data_start_end <- cohort_washout_age_study_start_end_15_50 %>% 
  mutate(age_data_start_2 = calc_age(dob, data_start_2)) %>% 
  mutate(age_data_end = calc_age(dob, data_end))

## Cohort entry - restricting to 1/1/2009 - 31/12/2018 & age anywhere from 15 to 49yo during the study period
cohort_washout_clean_cohort_entry <- cohort_washout_age_study_start_end_15_50_age_data_start_end %>% 
  mutate(cohort_entry = ifelse(data_start_2 <= as.Date("2009-01-01") & data_end > as.Date("2009-01-01") & age_study_start >= 15 & age_study_start < 50, as.Date("2009-01-01"),
                               ifelse( data_start_2 > as.Date("2009-01-01") & age_data_start_2 >= 15 & age_data_start_2 < 50 , data_start_2,
                                       ifelse(data_start_2 > as.Date("2009-01-01") & age_data_start_2 < 15 & age_15 < data_end, age_15,
                                              ifelse(data_start_2 <= as.Date("2009-01-01") & data_end >= as.Date("2009-01-01") & age_study_start < 15 & age_15 < data_end, age_15, NA)))))
cohort_washout_clean_cohort_entry$cohort_entry <- as_date(cohort_washout_clean_cohort_entry$cohort_entry)
summary(cohort_washout_clean_cohort_entry$cohort_entry) 
## Drop patients that never enter the cohort
cohort_washout_clean_cohort_entry_nonas <- cohort_washout_clean_cohort_entry %>% filter(!is.na(cohort_entry))
summary(cohort_washout_clean_cohort_entry_nonas$cohort_entry)

## Cohort exit- restricting to 1/1/2009 - 31/12/2018 & age anywhere from 15 to 49yo during the study period
cohort_washout_clean_cohort_entry_nonas_exit <- cohort_washout_clean_cohort_entry_nonas %>% 
  mutate(cohort_exit = ifelse(data_end <=  as.Date("2018-12-31") & age_data_end < 50, data_end, 
                              ifelse(data_end <=  as.Date("2018-12-31") & (age_50-1) < data_end, (age_50-1),
                                     ifelse(data_end > as.Date("2018-12-31") & age_study_end < 50, as.Date("2018-12-31"),
                                            ifelse(data_end > as.Date("2018-12-31") & (age_50-1) < as.Date("2018-12-31"), (age_50-1), NA)))))
cohort_washout_clean_cohort_entry_nonas_exit$cohort_exit <- as_date(cohort_washout_clean_cohort_entry_nonas_exit$cohort_exit)
summary(cohort_washout_clean_cohort_entry_nonas_exit$cohort_exit)

## Calc cohort person years/days 

## Adding person year and person days
cohort_washout_clean_cohort_entry_nonas_exit_pyrs_pdays <- cohort_washout_clean_cohort_entry_nonas_exit %>%
  mutate(pdays = (cohort_exit-cohort_entry)+1) %>%
  mutate(pyears = pdays/365.25)
cohort_washout_clean_cohort_entry_nonas_exit_pyrs_pdays$pyears <- as.numeric(cohort_washout_clean_cohort_entry_nonas_exit_pyrs_pdays$pyears)
cohort_washout_clean_cohort_entry_nonas_exit_pyrs_pdays$pdays <- as.numeric(cohort_washout_clean_cohort_entry_nonas_exit_pyrs_pdays$pdays)

cohort_15_49_washout_clean <- cohort_washout_clean_cohort_entry_nonas_exit_pyrs_pdays

## Recoding, renaming variables -------------------------------------------------------------------

# migration_clean ---

# Rename 'category' to avoid confusion with ethnicity 'category'
migration_clean <- rename(migration_clean, migcat = category)

# Create certainty of migration variable with only 1 record per patient - Display results as Definite (1) = category 2 (birth) & 4 (visa); Probable migrant (2) = category 3 (language); Possible migrants (3)= category 1 (origin)
n_distinct(migration_clean$patid)
migration_clean <- migration_clean %>%
  mutate(migcertainty = as.integer(migcat))

migration_clean <- migration_clean %>%
  mutate(migcertainty=replace(migcertainty, migcertainty==2, 4)) %>%
  mutate(migcertainty=replace(migcertainty, migcertainty==4, 4))

migration_clean <- migration_clean %>%
  group_by(patid) %>%
  summarise(migcertainty=max(migcertainty))
n_distinct(migration_clean)

migration_clean$migcertainty <- factor(migration_clean$migcertainty, levels = c(4,3,1),
                                       labels = c("Definite", "Probable", "Possible"))

## SRHR outcomes ---

## Rename 'category' to avoid confusion between category variable in different datasets
induced_abortion_clean <- rename(induced_abortion_clean, abortioncat = category)
ec_clean <- rename(ec_clean, eccat = category)
dva_clean <- rename(dva_clean, dvacat = category)
chlamydia_test_clean <- rename(chlamydia_test_clean, chlamytestcat = category)
infertility_management_clean <- rename(infertility_management_clean, infertmxcat = category)
cervical_screening_clean <- rename(cervical_screening_clean, cervscreencat = category)

## Drop events before 2009
induced_abortion_clean <- induced_abortion_clean %>% filter(eventdate >= "2009-01-01")
ec_clean <- ec_clean %>% filter(eventdate >= "2009-01-01")
dva_clean <- dva_clean %>% filter(eventdate >= "2009-01-01")
chlamydia_test_clean <- chlamydia_test_clean %>% filter(eventdate >= "2009-01-01")
infertility_management_clean <- infertility_management_clean %>% filter(eventdate >= "2009-01-01")
cervical_screening_clean <- cervical_screening_clean %>% filter(eventdate >= "2009-01-01")
consultations_clean <- consultations_clean %>% filter(eventdate >= "2009-01-01" & eventdate < "2019-01-01") 

# create 4 categories from 61 consultation type
consultations_clean <- consultations_clean %>%
  mutate(cons4type= constype)
levels(consultations_clean$cons4type) <-  c(levels(consultations_clean$cons4type),"Non-consultation", "Scheduled consultation", "Unscheduled/out-of-practice consultation", "Phone consultation")
consultations_clean$cons4type[consultations_clean$cons4type == "Clinic"] <- "Scheduled consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Follow-up/routine visit"] <- "Scheduled consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Surgery consultation"] <- "Scheduled consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Repeat issue"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Medicine management"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Community clinic"] <- "Scheduled consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Initial post discharge review"] <- "Scheduled consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Telephone call to a patient"] <- "Phone consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Telephone call from a patient"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Co-op telephone advice"] <- "Phone consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Telephone consultation"] <- "Phone consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Night visit, local rota"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Night visit, practice"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Night visit, deputising service"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Out of hours, practice"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Out of hours, non-practice"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Acute visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Emergency consultation"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Casualty attendance"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Third party consultation"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Hospital admission"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Children's home visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Home visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Hotel visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Nursing home visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Residential home visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Nursing home visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Twilight visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Walk-in centre"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Co-op surgery consultation"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Co-op home visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Minor injury service"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Night visit"] <- "Unscheduled/out-of-practice consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Data not entered"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Mail from patient"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Discharge details"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Letter from outpatients"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Other"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Results recording"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Mail to patient"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Administration"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Day case report"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "GOS18 report"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "NHS direct report"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Triage"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Community nursing note"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Community nursing report"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Data transferred from other system"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Health authority entry"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Health visitor note"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Health visitor report"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Hospital inpatient report"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Laboratory request"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Radiology request"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Radiology result"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Referral letter"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Social services report"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Template entry"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "GP to GP communication transaction"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Non-consultation medication data"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "Non-consultation data"] <- "Non-consultation"
consultations_clean$cons4type[consultations_clean$cons4type == "ePharmacy message"] <- "Non-consultation"

# mutate4 into 2 categories for consultation type
consultations_clean <- consultations_clean %>%
  mutate(cons2type= cons4type)
levels(consultations_clean$cons2type) <-  c(levels(consultations_clean$cons2type),"Direct", "Indirect")
consultations_clean$cons2type[consultations_clean$cons2type == "Non-consultation"] <- "Indirect"
consultations_clean$cons2type[consultations_clean$cons2type == "Scheduled consultation"] <- "Direct"
consultations_clean$cons2type[consultations_clean$cons2type == "Unscheduled/out-of-practice consultation"] <- "Direct"
consultations_clean$cons2type[consultations_clean$cons2type == "Phone consultation"] <- "Direct" 

# Keep Direct only for cons2type
consultations_clean <- consultations_clean %>% filter(cons2type == "Direct")

## Ethnicity ---

## remove records for patients with the same ethnicat 18 category coded > 1 
count(tidy_ethnicity)
n_distinct(distinct(tidy_ethnicity, patid, category))
ethnicity_clean <- tidy_ethnicity %>% distinct(patid, category, .keep_all = TRUE)
count(ethnicity_clean)

## Drop records with ethnicity recorded as ethnic group not specified or NA (i.e. no ethnicity data available for that patient) - note: later after joining to all_patient file, ethnicity N/A's retained as 'Unknown' category
sum(ethnicity_clean$category == "Ethnic group not specified")
sum(is.na(ethnicity_clean$category))
ethnicity_NOS <- ethnicity_clean%>%
  filter(category == "Ethnic group not specified") 
ethnicity_clean <- ethnicity_clean%>%
  filter(category != "Ethnic group not specified") %>%
  filter(!is.na(category))

n_distinct(ethnicity_clean$patid)

## Keep the most recent ethnic code for those with more than one ethnicity (for those with eventdate)
recent_ethnicity_clean <- ethnicity_clean %>%
  filter(!is.na(eventdate))
recent_ethnicity_clean <- recent_ethnicity_clean %>%
  group_by(patid) %>%
  slice(which.max(eventdate))
n_distinct(recent_ethnicity_clean$patid)
nrow(recent_ethnicity_clean) == n_distinct(recent_ethnicity_clean$patid)

## dplyr::select most frequently occuring ethnicity for those without eventdates
no_eventdate <- ethnicity_clean %>% 
  filter(is.na(eventdate))
n_distinct(no_eventdate$patid)

mode <- function(x) {
  ux <- unique(x)
  ux [which.max(tabulate(match(x,ux)))]
}

no_eventdate <- no_eventdate %>% 
  group_by(patid) %>%
  mutate(category=mode(category))

no_eventdate <- no_eventdate %>%
  distinct(patid, category) 

nrow(no_eventdate) == n_distinct(no_eventdate$patid) 

ethnicity_clean <- bind_rows(recent_ethnicity_clean, no_eventdate) 
nrow(ethnicity_clean) == n_distinct(ethnicity_clean$patid) 

## dplyr::select eventdate ethnicity and remove no eventdate ethnicity code for patients who have both
ethnicity_clean <- ethnicity_clean %>%
  mutate(eventdate=replace(eventdate, is.na(eventdate), as.Date("1900-01-01")))

ethnicity_clean <- ethnicity_clean %>%
  group_by(patid) %>%
  slice(which.max(eventdate)) 

n_distinct(ethnicity_clean$patid) 
nrow(ethnicity_clean) == n_distinct(ethnicity_clean$patid) 

ethnicity_clean <- ethnicity_clean %>%
  mutate(eventdate=replace(eventdate, eventdate == "1900-01-01", NA))

## Rename ethnicity category label
ethnicity_clean <- rename(ethnicity_clean, ethnicat = category)

## Create 6 group ethnicity variable 
ethnicity_clean <- ethnicity_clean %>%
  mutate(ethnicat6 = ethnicat)
levels(ethnicity_clean$ethnicat6) <-  c(levels(ethnicity_clean$ethnicat6),"White British", "White Non-British", "Mixed", 
                                        "Asian/Asian British", "Black/Black British", "Other ethnic group" )
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "White British"] <- "White British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "White Irish"] <- "White Non-British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Gypsy or Irish Traveller"] <- "White Non-British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Other White"] <- "White Non-British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Mixed White and Black Caribbean"] <- "Mixed"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Mixed White and Black African"] <- "Mixed"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Mixed White and Asian"] <- "Mixed"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Other Mixed"] <- "Mixed"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Indian"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Pakistani"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Bangladeshi"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Chinese"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Other Asian"] <- "Asian/Asian British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Black Caribbean"] <- "Black/Black British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Black African"] <- "Black/Black British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Other Black"] <- "Black/Black British"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Arab"] <- "Other ethnic group"
ethnicity_clean$ethnicat6[ethnicity_clean$ethnicat6 == "Any other ethnic group"] <- "Other ethnic group"

## Relevel ethnicat6
ethnicity_clean <- as.data.frame(ethnicity_clean)
ethnicity_clean <- within(ethnicity_clean, ethnicat6 <- relevel (ethnicat6, ref="White British") )
ethnicity_clean$ethnicat6 <- droplevels(ethnicity_clean$ethnicat6)
levels(ethnicity_clean$ethnicat6)

## check if need to Remove duplicates based on patid and category for 6 group classification and check number of patients with conflicting events i.e. more than one of 5 group classification
count(ethnicity_clean)
n_distinct(distinct(ethnicity_clean, patid, ethnicat))

# Remove eventdate variable
ethnicity_clean <-  ethnicity_clean %>% dplyr::select(-c(eventdate, consid, medcode, staffid))

## imd ---
patient_imd_clean <- rename(patient_imd_clean, patimd = imd2015_5)
practice_imd_clean <- rename(practice_imd_clean, pracimd = e2015_imd_5)

## Clean .Rdata files ------------------------------------------------------------------------------------

## Drop unused factor levels
cohort_15_49_clean <- droplevels(cohort_15_49_clean)
cohort_15_49_washout_clean <- droplevels(cohort_15_49_washout_clean)
all_patients_clean <- droplevels(all_patients_clean)
migration_clean <- droplevels(migration_clean)
induced_abortion_clean <- droplevels(induced_abortion_clean)
ec_clean <- droplevels(ec_clean)
dva_clean <- droplevels(dva_clean)
chlamydia_test_clean <- droplevels(chlamydia_test_clean)
infertility_management_clean <- droplevels(infertility_management_clean)
cervical_screening_clean <- droplevels(cervical_screening_clean)
consultations_clean <- droplevels(consultations_clean)
ethnicity_clean <- droplevels(ethnicity_clean) 
patient_imd_clean <- droplevels(patient_imd_clean)
practice_imd_clean <- droplevels(practice_imd_clean)

## Check factors correctly labelled
levels(cohort_15_49_clean$gender)
levels(cohort_15_49_washout_clean$gender)
levels(all_patients_clean$prac_region)
levels(all_patients_clean$gender)
levels(all_patients_clean$marital)
levels(migration_clean$migcertainty)
levels(induced_abortion_clean$constype)
levels(induced_abortion_clean$abortioncat)
levels(ec_clean$constype)
levels(ec_clean$eccat)
levels(dva_clean$constype)
levels(dva_clean$dvacat)
levels(chlamydia_test_clean$constype)
levels(chlamydia_test_clean$chlamytestcat)
levels(infertility_management_clean$constype)
levels(infertility_management_clean$infertmxcat)
levels(cervical_screening_clean$constype)
levels(cervical_screening_clean$cervscreencat)
levels(consultations_clean$constype)
levels(consultations_clean$cons2type)
levels(ethnicity_clean$ethnicat)
levels(ethnicity_clean$ethnicat6)
levels(patient_imd_clean$patimd)
levels(practice_imd_clean$pracimd)

## Save & load cleaned .Rdata ------------------------------------------------------------------------

## save
save(cohort_15_49_clean, file = "filepath")
save(cohort_15_49_washout_clean, file = "filepath")
save(all_patients_clean, file = "filepath")
save(migration_clean, file = "filepath")
save(induced_abortion_clean, file = "filepath")
save(ec_clean, file = "filepath")
save(dva_clean, file = "filepath")
save(chlamydia_test_clean, file = "filepath")
save(infertility_management_clean, file = "filepath")
save(cervical_screening_clean, file = "cleaned_files/cervical_screening_clean.Rdata")
save(consultations_clean, file = "filepath")
save(ethnicity_clean, file = "filepath")
save(patient_imd_clean, file = "filepath")
save(practice_imd_clean, file = "filepath")

## Load clean .Rdata files 
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")

## MAIN ANALYSIS + MIGCERTAINTY + ETHNICITY DATASETS ------------------------

## Join cohort/migration/ethnicity/imd ------------------------------------------------------------------------


## Join cohort and all patients to fill cohort with additional variables (marital status, yob, prac_country etc)all_patients_alldata <- left_join(all_patients_clean, imd_clean, by = c("patid" = "patid", "pracid" = "pracid"))
cohort_15_49_alldata <- left_join(cohort_15_49_clean, all_patients_clean, 
                                  by = c("patid" = "patid", "pracid" = "pracid", "prac_lcd" = "prac_lcd",
                                         "prac_uts" = "prac_uts", "gender" = "gender", "dob" = "dob",
                                         "frd" = "frd", "crd" = "crd", "tod" = "tod", "deathdate" = "deathdate",
                                         "data_start" = "data_start", "data_end" = "data_end"))
str(cohort_15_49_alldata)

## Join patient_imd_clean and practice_imd_clean 
imd_clean <- inner_join(patient_imd_clean, practice_imd_clean, by = c("pracid" = "pracid"))
glimpse(imd_clean)
sum(is.na(imd_clean))
summary(is.na(imd_clean))

## Join all patient cohort file to ethnicity, migration, imd_clean 
cohort_15_49_alldata1 <- left_join(cohort_15_49_alldata, imd_clean, by = c("patid" = "patid", "pracid" = "pracid"))
count(cohort_15_49_alldata1) == count(cohort_15_49_alldata)
cohort_15_49_alldata2 <- left_join(cohort_15_49_alldata1, migration_clean, by = c("patid" = "patid"))
count(cohort_15_49_alldata2) == count(cohort_15_49_alldata1)
cohort_15_49_alldata3 <- left_join(cohort_15_49_alldata2, ethnicity_clean, by = c("patid" = "patid"))
count(cohort_15_49_alldata3) == count(cohort_15_49_alldata2)
sum(is.na(cohort_15_49_alldata3$ethnicat))
sum(is.na(cohort_15_49_alldata3$ethnicat6)) 
n_distinct(cohort_15_49_alldata3$patid) == n_distinct(cohort_15_49_alldata3)
glimpse(cohort_15_49_alldata3)
sum(is.na(cohort_15_49_alldata3))
summary(is.na(cohort_15_49_alldata3)) 
cohort_15_49_alldata <- cohort_15_49_alldata3

## create migrant vs non-migrant variable
sum(is.na(cohort_15_49_alldata$migcertainty)) 
cohort_15_49_alldata <- cohort_15_49_alldata %>%
  mutate(migrant_status= migcertainty)

levels(cohort_15_49_alldata$migrant_status) <-  c(levels(cohort_15_49_alldata$migrant_status),"Non-migrant", "Migrant")
cohort_15_49_alldata$migrant_status[is.na(cohort_15_49_alldata$migrant_status)] <- "Non-migrant"
cohort_15_49_alldata$migrant_status[cohort_15_49_alldata$migrant_status == "Definite"] <- "Migrant"
cohort_15_49_alldata$migrant_status[cohort_15_49_alldata$migrant_status == "Probable"] <- "Migrant"
cohort_15_49_alldata$migrant_status[cohort_15_49_alldata$migrant_status == "Possible"] <- "Migrant"
str(cohort_15_49_alldata)
cohort_15_49_alldata %>% group_by(migrant_status) %>% count() 

## Replace N/As (i.e. non-migrants) for migcertainty variable with "Non-migrant" certainty category
levels(cohort_15_49_alldata$migcertainty)  <- c(levels(cohort_15_49_alldata$migcertainty), "Non-migrant")
cohort_15_49_alldata <- cohort_15_49_alldata %>%
  mutate(migcertainty=replace(migcertainty, is.na(migcertainty), "Non-migrant"))
str(cohort_15_49_alldata)
cohort_15_49_alldata %>% group_by(migcertainty) %>% count()

# Drop records for possible migrants
cohort_15_49_alldata <- cohort_15_49_alldata %>% filter(migcertainty != "Possible" )

## Merge pat and prac IMD into overall IMD where value is patient imd if available otherwise practice IMD

cohort_15_49_alldata <- cohort_15_49_alldata %>%
  mutate(imd = ifelse(!is.na(patimd), patimd, pracimd))
cohort_15_49_alldata$imd <- factor(cohort_15_49_alldata$imd, levels = c(1:5),
                                   labels = c("IMD 1", "IMD 2", "IMD 3", "IMD 4","IMD 5" ))
str(cohort_15_49_alldata)



## Save & load final cohort .Rdata ------------------------------------------------------------------

cohort_15_49_final <- cohort_15_49_alldata
save(cohort_15_49_final, file = "filepath") 
load(file = "filepath")

## Join srhr outcomes/cohort ---------------------------------------------------------------------------------------

## Add patient data to all SRHR events
induced_abortion_alldata <- left_join(induced_abortion_clean, cohort_15_49_final, by = c("patid" = "patid"))
ec_alldata <- left_join(ec_clean, cohort_15_49_final, by = c("patid" = "patid"))
dva_alldata <- left_join(dva_clean, cohort_15_49_final, by = c("patid" = "patid"))
chlamydia_test_alldata <- left_join(chlamydia_test_clean, cohort_15_49_final, by = c("patid" = "patid"))
infertility_management_alldata <- left_join(infertility_management_clean, cohort_15_49_final, by = c("patid" = "patid"))
cervical_screening_alldata <- left_join(cervical_screening_clean, cohort_15_49_final, by = c("patid" = "patid"))
consultations_alldata <- left_join(consultations_clean, cohort_15_49_final, by = c("patid" = "patid"))


## Check retained all SRHR events
count(induced_abortion_alldata) == count(induced_abortion_clean)
count(ec_alldata) == count(ec_clean)
count(dva_alldata) == count(dva_clean)
count(chlamydia_test_alldata) == count(chlamydia_test_clean)
count(infertility_management_alldata) == count(infertility_management_clean)
count(cervical_screening_alldata) == count(cervical_screening_clean)
count(consultations_alldata) == count(consultations_clean)

## Drop ineligible SRHR records   ------------------------------------------

## ie. pt not age 15-49 or not during patient's data start/data end

## Create age at time of event variable for SRHR outcomes
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

induced_abortion_alldata_age <- induced_abortion_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
ec_alldata_age <- ec_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
dva_alldata_age <- dva_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
chlamydia_test_alldata_age <- chlamydia_test_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
infertility_management_alldata_age <- infertility_management_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
cervical_screening_alldata_age <- cervical_screening_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
consultations_alldata_age <- consultations_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))


## Filter for records if age 15-49 and data during valid data period and data during study period 
induced_abortion_alldata_15_49 <- induced_abortion_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49) %>% 
  filter(eventdate >= data_start & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
ec_alldata_15_49 <- ec_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
dva_alldata_15_49 <- dva_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
chlamydia_test_alldata_15_49 <- chlamydia_test_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
infertility_management_alldata_15_49 <- infertility_management_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
cervical_screening_alldata_15_49 <- cervical_screening_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
consultations_alldata_15_49 <- consultations_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)

## Check filtered correct ages
summary(induced_abortion_alldata_15_49$event_age)
summary(ec_alldata_15_49$event_age)
summary(dva_alldata_15_49$event_age)
summary(chlamydia_test_alldata_15_49$event_age)
summary(infertility_management_alldata_15_49$event_age)
summary(cervical_screening_alldata_15_49$event_age)
summary(consultations_alldata_15_49$event_age)


## Create annual cohort records ----------------------------------------------

## create variable with repeated sequence of years
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(cohort_15_49_final)))
colnames(year_variable) <- 'eventyear'
cohort_15_49_final_2 <- cohort_15_49_final %>%
  slice(rep(1:n(), each=10))
cohort_15_49_final_2$eventyear <- year_variable$eventyear
cohort_15_49_annual_records <- filter(cohort_15_49_final_2, eventyear >= year(cohort_entry) & eventyear <= year(cohort_exit))


## Create annual srhr counts -----------------------------------------------------------------------

## Induced abortion ---
induced_abortion_annual_counts <- induced_abortion_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(abortion_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, abortion_n) %>%
  distinct()

## ec ---
ec_annual_counts <- ec_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(ec_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, ec_n) %>%
  distinct()

## dva ---
dva_annual_counts <- dva_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(dva_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, dva_n) %>%
  distinct()

## chlamydia testing ---
chlamydia_test_annual_counts <- chlamydia_test_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(chlamydia_test_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, chlamydia_test_n) %>%
  distinct()

## infertility_management ---
infertility_management_annual_counts <- infertility_management_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(infertility_management_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, infertility_management_n) %>%
  distinct()

## cervical screening  ---
cervical_screening_annual_counts <- cervical_screening_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(cervical_screening_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, cervical_screening_n) %>%
  distinct()

## consultations  ---
consultations_annual_counts <- consultations_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(consultations_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, consultations_n) %>%
  distinct()


## Join annual cohort records/annual srhr counts  --------------------------------------------------------------

cohort_15_49_annual_counts <- cohort_15_49_annual_records %>%
  left_join(induced_abortion_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(ec_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(dva_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(chlamydia_test_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(infertility_management_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(cervical_screening_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(consultations_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) 

## Replace NAs in SRH counts with 0 

cohort_15_49_annual_counts$abortion_n <- cohort_15_49_annual_counts$abortion_n %>% replace_na(0)
cohort_15_49_annual_counts$ec_n <- cohort_15_49_annual_counts$ec_n %>% replace_na(0)
cohort_15_49_annual_counts$dva_n <- cohort_15_49_annual_counts$dva_n %>% replace_na(0)
cohort_15_49_annual_counts$chlamydia_test_n <- cohort_15_49_annual_counts$chlamydia_test_n %>% replace_na(0)
cohort_15_49_annual_counts$infertility_management_n <- cohort_15_49_annual_counts$infertility_management_n %>% replace_na(0)
cohort_15_49_annual_counts$cervical_screening_n <- cohort_15_49_annual_counts$cervical_screening_n %>% replace_na(0)
cohort_15_49_annual_counts$consultations_n <- cohort_15_49_annual_counts$consultations_n %>% replace_na(0)

## Add age during event year and age category in event year ----------------------------------------------------

cohort_15_49_annual_counts_age <- cohort_15_49_annual_counts %>% mutate(age_date= eventyear) 
cohort_15_49_annual_counts_age$age_date <- as.character(cohort_15_49_annual_counts_age$age_date)
cohort_15_49_annual_counts_age$age_date <- paste("01-01-", cohort_15_49_annual_counts_age$age_date, sep="")
head(cohort_15_49_annual_counts_age$age_date)
cohort_15_49_annual_counts_age$age_date <- dmy(cohort_15_49_annual_counts_age$age_date)
head(cohort_15_49_annual_counts_age$age_date)
class(cohort_15_49_annual_counts_age$age_date)
cohort_15_49_annual_counts_age <- cohort_15_49_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(cohort_15_49_annual_counts_age$eventyear_age) # 15
max(cohort_15_49_annual_counts_age$eventyear_age) # 49
cohort_15_49_annual_counts_age_final <- cohort_15_49_annual_counts_age %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
cohort_15_49_annual_counts_age_final$eventyear_agecat <- factor(cohort_15_49_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                                labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

## dplyr::select relevant variables for final datasets -----------------------------------------------------------
reproductive_cohort_final <- dplyr::select(cohort_15_49_final, c(patid, pracid, prac_region, 
                                                          gender, dob, yob, patimd, pracimd, imd, 
                                                          migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                          data_start, data_end, cohort_entry, cohort_exit, 
                                                          pdays, pyears))
srh_annual_counts_final <- dplyr::select(cohort_15_49_annual_counts_age_final, c(patid, eventyear,pracid, prac_region, 
                                                                          gender, dob, yob, patimd, pracimd, imd, 
                                                                          migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                          data_start, data_end, cohort_entry, cohort_exit, 
                                                                          pdays, pyears, eventyear_age, eventyear_agecat, abortion_n, 
                                                                       ec_n,  
                                                                          dva_n,  chlamydia_test_n, 
                                                                          infertility_management_n, 
                                                                          cervical_screening_n, consultations_n))

## Save & load  final datasets -------

save(reproductive_cohort_final, file = "filepath")
save(srh_annual_counts_final, file = "filepath")
load(file = "filepath")
load(file = "filepath")

## Set up data for stratifying by time variant variable (year) - eventually move to cleaning script
srh_annual_counts_final_extra <- srh_annual_counts_final %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
srh_annual_counts_final_extra$start_eventyear <- as_date(srh_annual_counts_final_extra$start_eventyear)
srh_annual_counts_final_extra$end_eventyear <- as_date(srh_annual_counts_final_extra$end_eventyear)
srh_annual_counts_final_extra <- srh_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(cohort_entry <= start_eventyear, start_eventyear,cohort_entry))
srh_annual_counts_final_extra$t0_ey <- as_date(srh_annual_counts_final_extra$t0_ey)
srh_annual_counts_final_extra <- srh_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(cohort_exit >= end_eventyear, end_eventyear,cohort_exit))
srh_annual_counts_final_extra$t1_ey <- as_date(srh_annual_counts_final_extra$t1_ey)
srh_annual_counts_final_extra <- srh_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) %>%
  mutate(pyears_ey=pdays_ey/365.25)
srh_annual_counts_final_extra$pdays_ey <- as.numeric(srh_annual_counts_final_extra$pdays_ey)
srh_annual_counts_final_extra$pyears_ey <- as.numeric(srh_annual_counts_final_extra$pyears_ey)

a <- reproductive_cohort_final %>% group_by(patid) %>% tally(pdays)
b <- srh_annual_counts_final_extra %>% group_by(patid) %>% tally(pdays_ey)
c <- anti_join(b,a)
d <- anti_join(a,b) 

a <- reproductive_cohort_final %>% group_by(patid) %>% tally(pyears)
b <- srh_annual_counts_final_extra %>% group_by(patid) %>% tally(pyears_ey)
c <- anti_join(b,a)
d <- anti_join(a,b)
e <- full_join(c,d, "patid" = "patid") %>% arrange(patid)
a.rounded <- round(a, 2)
b.rounded <- round(b, 2)
c.rounded <- anti_join(b.rounded,a.rounded) 

save(srh_annual_counts_final_extra, file = "filepath")
load(file = "filepath")

## EXACT MATCHED DATASETS WITH 365 DAY WASHOUT FROM DATA START-----------------------------------------

## Load clean .Rdata files 
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")

## Join cohort/migration/ethnicity/imd ------------------------------------------------------------------------

## Join cohort and all patients to fill cohort with additional variables (marital status, yob, prac_country etc)all_patients_alldata <- left_join(all_patients_clean, imd_clean, by = c("patid" = "patid", "pracid" = "pracid"))
cohort_15_49_washout_alldata <- left_join(cohort_15_49_washout_clean, all_patients_clean, 
                                  by = c("patid" = "patid", "pracid" = "pracid", "prac_lcd" = "prac_lcd",
                                         "prac_uts" = "prac_uts", "gender" = "gender", "dob" = "dob",
                                         "frd" = "frd", "crd" = "crd", "tod" = "tod", "deathdate" = "deathdate",
                                         "data_start" = "data_start", "data_end" = "data_end"))
str(cohort_15_49_washout_alldata)


## Join patient_imd_clean and practice_imd_clean 
imd_clean <- inner_join(patient_imd_clean, practice_imd_clean, by = c("pracid" = "pracid"))
glimpse(imd_clean)
sum(is.na(imd_clean))
summary(is.na(imd_clean))

## Join all patient cohort file to ethnicity, migration, imd_clean 
cohort_15_49_washout_alldata1 <- left_join(cohort_15_49_washout_alldata, imd_clean, by = c("patid" = "patid", "pracid" = "pracid"))
count(cohort_15_49_washout_alldata1) == count(cohort_15_49_washout_alldata)
cohort_15_49_washout_alldata2 <- left_join(cohort_15_49_washout_alldata1, migration_clean, by = c("patid" = "patid"))
count(cohort_15_49_washout_alldata2) == count(cohort_15_49_washout_alldata1)
cohort_15_49_washout_alldata3 <- left_join(cohort_15_49_washout_alldata2, ethnicity_clean, by = c("patid" = "patid"))
count(cohort_15_49_washout_alldata3) == count(cohort_15_49_washout_alldata2)
sum(is.na(cohort_15_49_washout_alldata3$ethnicat)) 
sum(is.na(cohort_15_49_washout_alldata3$ethnicat6)) 
n_distinct(cohort_15_49_washout_alldata3$patid) == n_distinct(cohort_15_49_washout_alldata3) 
glimpse(cohort_15_49_washout_alldata3)
sum(is.na(cohort_15_49_washout_alldata3))
summary(is.na(cohort_15_49_washout_alldata3)) 
cohort_15_49_washout_alldata <- cohort_15_49_washout_alldata3

## create migrant vs non-migrant variable
sum(is.na(cohort_15_49_washout_alldata$migcertainty)) 
cohort_15_49_washout_alldata <- cohort_15_49_washout_alldata %>%
  mutate(migrant_status= migcertainty)

levels(cohort_15_49_washout_alldata$migrant_status) <-  c(levels(cohort_15_49_washout_alldata$migrant_status),"Non-migrant", "Migrant")
cohort_15_49_washout_alldata$migrant_status[is.na(cohort_15_49_washout_alldata$migrant_status)] <- "Non-migrant"
cohort_15_49_washout_alldata$migrant_status[cohort_15_49_washout_alldata$migrant_status == "Definite"] <- "Migrant"
cohort_15_49_washout_alldata$migrant_status[cohort_15_49_washout_alldata$migrant_status == "Probable"] <- "Migrant"
cohort_15_49_washout_alldata$migrant_status[cohort_15_49_washout_alldata$migrant_status == "Possible"] <- "Migrant"
str(cohort_15_49_washout_alldata)
cohort_15_49_washout_alldata %>% group_by(migrant_status) %>% count() 

## Replace N/As (i.e. non-migrants) for migcertainty variable with "Non-migrant" certainty category
levels(cohort_15_49_washout_alldata$migcertainty)  <- c(levels(cohort_15_49_washout_alldata$migcertainty), "Non-migrant")
cohort_15_49_washout_alldata <- cohort_15_49_washout_alldata %>%
  mutate(migcertainty=replace(migcertainty, is.na(migcertainty), "Non-migrant"))
str(cohort_15_49_washout_alldata)
cohort_15_49_washout_alldata %>% group_by(migcertainty) %>% count()

# Drop records for possible migrants
cohort_15_49_washout_alldata <- cohort_15_49_washout_alldata %>% filter(migcertainty != "Possible" )

## Merge pat and prac IMD into overall IMD where value is patient imd if available otherwise practice IMD
cohort_15_49_washout_alldata <- cohort_15_49_washout_alldata %>%
  mutate(imd = ifelse(!is.na(patimd), patimd, pracimd))
cohort_15_49_washout_alldata$imd <- factor(cohort_15_49_washout_alldata$imd, levels = c(1:5),
                                   labels = c("IMD 1", "IMD 2", "IMD 3", "IMD 4","IMD 5" ))
str(cohort_15_49_washout_alldata)

## Save & load final cohort .Rdata ------------------------------------------------------------------

cohort_15_49_washout_final <- cohort_15_49_washout_alldata
save(cohort_15_49_washout_final, file = "filepath") 
load(file = "filepath")

## Join srhr outcomes/cohort ---------------------------------------------------------------------------------------

## Add patient data to all SRHR events
induced_abortion_alldata <- left_join(induced_abortion_clean, cohort_15_49_washout_final, by = c("patid" = "patid"))
ec_alldata <- left_join(ec_clean, cohort_15_49_washout_final, by = c("patid" = "patid"))
dva_alldata <- left_join(dva_clean, cohort_15_49_washout_final, by = c("patid" = "patid"))
chlamydia_test_alldata <- left_join(chlamydia_test_clean, cohort_15_49_washout_final, by = c("patid" = "patid"))
infertility_management_alldata <- left_join(infertility_management_clean, cohort_15_49_washout_final, by = c("patid" = "patid"))
cervical_screening_alldata <- left_join(cervical_screening_clean, cohort_15_49_washout_final, by = c("patid" = "patid"))
consultations_alldata <- left_join(consultations_clean, cohort_15_49_washout_final, by = c("patid" = "patid"))

## Check retained all SRHR events
count(induced_abortion_alldata) == count(induced_abortion_clean)
count(ec_alldata) == count(ec_clean)
count(dva_alldata) == count(dva_clean)
count(chlamydia_test_alldata) == count(chlamydia_test_clean)
count(infertility_management_alldata) == count(infertility_management_clean)
count(cervical_screening_alldata) == count(cervical_screening_clean)
count(consultations_alldata) == count(consultations_clean)

## Drop ineligible SRHR records   ------------------------------------------

## ie. pt not age 15-49 or not during patient's data start/data end

## Create age at time of event variable for SRHR outcomes
calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

induced_abortion_alldata_age <- induced_abortion_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
ec_alldata_age <- ec_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
dva_alldata_age <- dva_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
chlamydia_test_alldata_age <- chlamydia_test_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
infertility_management_alldata_age <- infertility_management_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
cervical_screening_alldata_age <- cervical_screening_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))
consultations_alldata_age <- consultations_alldata %>%
  mutate(event_age = calc_age(dob, eventdate))

## Filter for records if age 15-49 and data during valid data period and data during study period 
induced_abortion_alldata_15_49 <- induced_abortion_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49) %>% 
  filter(eventdate >= data_start_2 & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
ec_alldata_15_49 <- ec_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start_2 & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
dva_alldata_15_49 <- dva_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start_2 & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
chlamydia_test_alldata_15_49 <- chlamydia_test_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start_2 & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
infertility_management_alldata_15_49 <- infertility_management_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start_2 & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
cervical_screening_alldata_15_49 <- cervical_screening_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start_2 & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)
consultations_alldata_15_49 <- consultations_alldata_age %>%  
  filter(event_age >=15 & event_age <= 49)  %>% 
  filter(eventdate >= data_start_2 & eventdate <= data_end) %>% 
  filter(eventdate >= cohort_entry & eventdate <= cohort_exit)

## Check filtered correct ages
summary(induced_abortion_alldata_15_49$event_age)
summary(ec_alldata_15_49$event_age)
summary(dva_alldata_15_49$event_age)
summary(chlamydia_test_alldata_15_49$event_age)
summary(infertility_management_alldata_15_49$event_age)
summary(cervical_screening_alldata_15_49$event_age)
summary(consultations_alldata_15_49$event_age)

## Create annual cohort records ----------------------------------------------

## create variable with repeated sequence of years
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(cohort_15_49_washout_final)))
colnames(year_variable) <- 'eventyear'

## Replicate each patient 10 times and label by year 2009-2018
cohort_15_49_washout_final_2 <- cohort_15_49_washout_final %>%
  slice(rep(1:n(), each=10))

## add the repeated year variable
cohort_15_49_washout_final_2$eventyear <- year_variable$eventyear

## filter 
cohort_15_49_washout_annual_records <- filter(cohort_15_49_washout_final_2, eventyear >= year(cohort_entry) & eventyear <= year(cohort_exit))


## Create annual srhr counts -----------------------------------------------------------------------

## Induced abortion ---
induced_abortion_annual_counts <- induced_abortion_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(abortion_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, abortion_n) %>%
  distinct()

## ec ---
ec_annual_counts <- ec_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(ec_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, ec_n) %>%
  distinct()

## dva ---
dva_annual_counts <- dva_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(dva_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, dva_n) %>%
  distinct()

## chlamydia testing ---
chlamydia_test_annual_counts <- chlamydia_test_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(chlamydia_test_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, chlamydia_test_n) %>%
  distinct()

## infertility_management ---
infertility_management_annual_counts <- infertility_management_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(infertility_management_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, infertility_management_n) %>%
  distinct()

## cervical screening  ---
cervical_screening_annual_counts <- cervical_screening_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(cervical_screening_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, cervical_screening_n) %>%
  distinct()

## consultations  ---
consultations_annual_counts <- consultations_alldata_15_49 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(consultations_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, consultations_n) %>%
  distinct()

## Join annual cohort records/annual srhr counts  --------------------------------------------------------------

cohort_15_49_annual_counts_washout <- cohort_15_49_washout_annual_records %>%
  left_join(induced_abortion_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(ec_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(dva_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(chlamydia_test_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(infertility_management_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(cervical_screening_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(consultations_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) 

## Replace NAs in SRH counts with 0 

cohort_15_49_annual_counts_washout$abortion_n <- cohort_15_49_annual_counts_washout$abortion_n %>% replace_na(0)
cohort_15_49_annual_counts_washout$ec_n <- cohort_15_49_annual_counts_washout$ec_n %>% replace_na(0)
cohort_15_49_annual_counts_washout$dva_n <- cohort_15_49_annual_counts_washout$dva_n %>% replace_na(0)
cohort_15_49_annual_counts_washout$chlamydia_test_n <- cohort_15_49_annual_counts_washout$chlamydia_test_n %>% replace_na(0)
cohort_15_49_annual_counts_washout$infertility_management_n <- cohort_15_49_annual_counts_washout$infertility_management_n %>% replace_na(0)
cohort_15_49_annual_counts_washout$cervical_screening_n <- cohort_15_49_annual_counts_washout$cervical_screening_n %>% replace_na(0)
cohort_15_49_annual_counts_washout$consultations_n <- cohort_15_49_annual_counts_washout$consultations_n %>% replace_na(0)

## Add age during event year and age category in event year ----------------------------------------------------

cohort_15_49_annual_counts_washout_age <- cohort_15_49_annual_counts_washout %>% mutate(age_date= eventyear) 
cohort_15_49_annual_counts_washout_age$age_date <- as.character(cohort_15_49_annual_counts_washout_age$age_date)
cohort_15_49_annual_counts_washout_age$age_date <- paste("01-01-", cohort_15_49_annual_counts_washout_age$age_date, sep="")
head(cohort_15_49_annual_counts_washout_age$age_date)
cohort_15_49_annual_counts_washout_age$age_date <- dmy(cohort_15_49_annual_counts_washout_age$age_date)
head(cohort_15_49_annual_counts_washout_age$age_date)
class(cohort_15_49_annual_counts_washout_age$age_date)
cohort_15_49_annual_counts_washout_age <- cohort_15_49_annual_counts_washout_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(cohort_15_49_annual_counts_washout_age$eventyear_age)
max(cohort_15_49_annual_counts_washout_age$eventyear_age) 
cohort_15_49_annual_counts_age_washout_final <- cohort_15_49_annual_counts_washout_age %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
cohort_15_49_annual_counts_age_washout_final$eventyear_agecat <- factor(cohort_15_49_annual_counts_age_washout_final$eventyear_agecat, levels = 0:6,
                                                                labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

## dplyr::select relevant variables for final datasets -----------------------------------------------------------
reproductive_cohort_final_washout <- dplyr::select(cohort_15_49_washout_final, c(patid, pracid, prac_region, 
                                                                 gender, dob, yob, patimd, pracimd, imd, 
                                                                 migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                 data_start, data_start_2, data_end, cohort_entry, cohort_exit, 
                                                                 pdays, pyears))
srh_annual_counts_final_washout <- dplyr::select(cohort_15_49_annual_counts_age_washout_final, c(patid, eventyear,pracid, prac_region, 
                                                                                 gender, dob, yob, patimd, pracimd, imd, 
                                                                                 migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                 data_start, data_start_2, data_end, cohort_entry, cohort_exit, 
                                                                                 pdays, pyears, eventyear_age, eventyear_agecat, abortion_n, 
                                                                                 ec_n,  
                                                                                 dva_n,  chlamydia_test_n, 
                                                                                 infertility_management_n, 
                                                                                 cervical_screening_n, consultations_n))

## Save & load  final datasets -------

save(reproductive_cohort_final_washout, file = "filepath") 
save(srh_annual_counts_final_washout, file = "filepath") 
load(file = "filepath")
load(file = "filepath")

## Set up data for stratifying by time variant variable (year) - eventually move to cleaning script
srh_annual_counts_final_washout_extra <- srh_annual_counts_final_washout %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
srh_annual_counts_final_washout_extra$start_eventyear <- as_date(srh_annual_counts_final_washout_extra$start_eventyear)
srh_annual_counts_final_washout_extra$end_eventyear <- as_date(srh_annual_counts_final_washout_extra$end_eventyear)
srh_annual_counts_final_washout_extra <- srh_annual_counts_final_washout_extra %>% 
  mutate(t0_ey = ifelse(cohort_entry <= start_eventyear, start_eventyear,cohort_entry))
srh_annual_counts_final_washout_extra$t0_ey <- as_date(srh_annual_counts_final_washout_extra$t0_ey)
srh_annual_counts_final_washout_extra <- srh_annual_counts_final_washout_extra %>% 
  mutate(t1_ey = ifelse(cohort_exit >= end_eventyear, end_eventyear,cohort_exit))
srh_annual_counts_final_washout_extra$t1_ey <- as_date(srh_annual_counts_final_washout_extra$t1_ey)
srh_annual_counts_final_washout_extra <- srh_annual_counts_final_washout_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) %>%
  mutate(pyears_ey=pdays_ey/365.25)
srh_annual_counts_final_washout_extra$pdays_ey <- as.numeric(srh_annual_counts_final_washout_extra$pdays_ey)
srh_annual_counts_final_washout_extra$pyears_ey <- as.numeric(srh_annual_counts_final_washout_extra$pyears_ey)

a <- reproductive_cohort_final_washout %>% group_by(patid) %>% tally(pdays)
b <- srh_annual_counts_final_washout_extra %>% group_by(patid) %>% tally(pdays_ey)
c <- anti_join(b,a)
d <- anti_join(a,b)

a <- reproductive_cohort_final_washout %>% group_by(patid) %>% tally(pyears)
b <- srh_annual_counts_final_washout_extra %>% group_by(patid) %>% tally(pyears_ey)
c <- anti_join(b,a)
d <- anti_join(a,b)
e <- full_join(c,d, "patid" = "patid") %>% arrange(patid)
a.rounded <- round(a, 2)
b.rounded <- round(b, 2)
c.rounded <- anti_join(b.rounded,a.rounded) 

save(srh_annual_counts_final_washout_extra, file = "filepath") 
load(file = "filepath")

## Exact matched cohort 4:1 ----------

load(file = "filepath")

# Derive age at cohort entry
reproductive_cohort_final_washout <- reproductive_cohort_final_washout %>%
  mutate(dob = lubridate::ymd(yob,truncated=2L))

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

reproductive_cohort_final_washout <- reproductive_cohort_final_washout %>%
  mutate(age_cohort_entry= calc_age(dob, cohort_entry))

# Derive year of cohort entry
reproductive_cohort_final_washout <- reproductive_cohort_final_washout %>%
  mutate(year_cohort_entry = lubridate::year(ymd(cohort_entry)))

# Drop unused levels for migrant status - added earlier in script, delete after re-running rest of script
levels(reproductive_cohort_final_washout$migrant_status)
reproductive_cohort_final_washout$migrant_status <- droplevels(reproductive_cohort_final_washout$migrant_status)
levels(reproductive_cohort_final_washout$migrant_status)

# turn migrant status into binary integer
reproductive_cohort_final_washout <- reproductive_cohort_final_washout %>%
  mutate(migrant_status_binary = as.numeric(migrant_status)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 1, 0)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 2, 1))

# select variables needed
reproductive_cohort_final_washout_2 <- dplyr::select(reproductive_cohort_final_washout, c(patid, migrant_status_binary,year_cohort_entry, age_cohort_entry, prac_region))

# function requires data.table
library(data.table)

# Turn df into data table
setDT(reproductive_cohort_final_washout_2)

# stratifies dataset and then selects random observations within strata
# data = dataset containing:
# - treatment/exposure variable 'mvar' (a string specifying variable name).
# - matching variable 'mvar' (a string specifying variable name). If you want to match on multiple variables, concatenate them first.
# other inputs are:
# - ratio of cases:controls (an integer > 0). You can also set ratio between 0 and 1, e.g. to 0.5 to get 2 cases/control, but the results aren't perfect
# - seed for fixing random selection of cases/controls (an integer; default NULL means no seed). Choice of seed is arbitrary. Seed will be reset globally at the end of the function (i.e. the function will overwrite a previous seed)
# returns data.table of matched observations, with additional variable 'id' for use in paired/grouped analyses
# the speed of the function mostly depends on the number of strata (i.e. levels of the matching variable)
# there is no problem converting the results to a standard data frame with data.frame(matched_data) if you prefer working in this way

smatch <- function (data, treat, mvar, ratio = 4, seed = NULL) {
  targ <- data[, .(case = sum(get(treat)), control = sum(!get(treat))), mvar]
  targ[, cst := floor(pmin(control / ratio, case))]
  targ[, cnt := cst * ratio]
  targ <- targ[cst > 0]
  setnames(targ, mvar, 'mvar')
  l2 <- cumsum(targ$cst)
  ids <- mapply(':', c(0, l2[-nrow(targ)]), l2-1)
  names(ids) <- targ$mvar
  case <- NULL
  control <- NULL
  set.seed(seed)
  on.exit(set.seed(NULL))
  for(i in targ$mvar) {
    case[[i]] <- data[get(treat) == T & get(mvar) == i][sample(.N, targ$cst[targ$mvar == i])]
    case[[i]][, id := ids[[i]]]
    control[[i]] <- data[get(treat) == F & get(mvar) == i][sample(.N, targ$cnt[targ$mvar == i])]
    control[[i]][, id := rep(ids[[i]], each = ratio)]
  }
  rbindlist(c(case, control))
}

# concatenate multiple variables
reproductive_cohort_final_washout_2[, age_year_region := paste0(age_cohort_entry, '-', year_cohort_entry, '-', prac_region)]

# create matched dataset

matched_data_4to1 <- smatch(reproductive_cohort_final_washout_2, 'migrant_status_binary', 'age_year_region') 

# check balance
dcast(matched_data_4to1, age_year_region ~ migrant_status_binary, value.var = 'age_cohort_entry', fun.aggregate = length)

# Turn back into dataframe
matched_reproductive_cohort_final_4to1 <- as.data.frame(matched_data_4to1)

# Rejoin matched cohort to rest of dataset
matched_reproductive_cohort_final_4to1 <- dplyr::select(matched_reproductive_cohort_final_4to1, -c(prac_region, year_cohort_entry, age_cohort_entry,migrant_status_binary))
exact_match_reproductive_cohort_final_4to1 <- left_join(matched_reproductive_cohort_final_4to1, reproductive_cohort_final_washout, by = c("patid"="patid"))

# Check 4:1 for whole cohort
exact_match_reproductive_cohort_final_4to1 %>% group_by(migrant_status) %>% count()

#Save cohort
save(exact_match_reproductive_cohort_final_4to1, file = "filepath") 

## Exact matched 4:1 counts file ---------------------

load(file = "filepath")
load(file = "filepath")

# Filter for only patients in exact matched 4:1 cohort
exact_match_srh_annual_counts_final_4to1 <- srh_annual_counts_final_washout %>% filter(patid %in% exact_match_reproductive_cohort_final_4to1$patid)

#Save events file
save(exact_match_srh_annual_counts_final_4to1 , file = "filepath") 

## Set up data for stratifying by time variant variable (year) - eventually move to cleaning script
exact_match_srh_annual_counts_final_extra_4to1 <- exact_match_srh_annual_counts_final_4to1 %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
exact_match_srh_annual_counts_final_extra_4to1$start_eventyear <- as_date(exact_match_srh_annual_counts_final_extra_4to1$start_eventyear)
exact_match_srh_annual_counts_final_extra_4to1$end_eventyear <- as_date(exact_match_srh_annual_counts_final_extra_4to1$end_eventyear)
exact_match_srh_annual_counts_final_extra_4to1 <- exact_match_srh_annual_counts_final_extra_4to1 %>% 
  mutate(t0_ey = ifelse(cohort_entry <= start_eventyear, start_eventyear,cohort_entry))
exact_match_srh_annual_counts_final_extra_4to1$t0_ey <- as_date(exact_match_srh_annual_counts_final_extra_4to1$t0_ey)
exact_match_srh_annual_counts_final_extra_4to1 <- exact_match_srh_annual_counts_final_extra_4to1 %>% 
  mutate(t1_ey = ifelse(cohort_exit >= end_eventyear, end_eventyear,cohort_exit))
exact_match_srh_annual_counts_final_extra_4to1$t1_ey <- as_date(exact_match_srh_annual_counts_final_extra_4to1$t1_ey)
exact_match_srh_annual_counts_final_extra_4to1 <- exact_match_srh_annual_counts_final_extra_4to1 %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) %>%
  mutate(pyears_ey=pdays_ey/365.25)
exact_match_srh_annual_counts_final_extra_4to1$pdays_ey <- as.numeric(exact_match_srh_annual_counts_final_extra_4to1$pdays_ey)
exact_match_srh_annual_counts_final_extra_4to1$pyears_ey <- as.numeric(exact_match_srh_annual_counts_final_extra_4to1$pyears_ey)

save(exact_match_srh_annual_counts_final_extra_4to1, file = "filepath")
load(file = "filepath")
