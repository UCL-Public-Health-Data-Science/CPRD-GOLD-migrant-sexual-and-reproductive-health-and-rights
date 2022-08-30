####---- Description -------------------------------------------------------------------------

## Cleaning data for Neha's contraception prescribing chapter in females of reproductive age (15-49yo) 
## Date started: 30/09/2020
## Author: Neha Pathak


## Load packages -------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(Hmisc)

## Set working directory ------------------------------------------------------------------

setwd("filepath")

## Import data -----------------------------------------------------------------------

## Cohort, exposure, covariates 
# linked files already cleaned in script 01_srhr_overview_cleaning
# reproductive_cohort_final.Rdata = cohort_15_49, all patients, migration events, ethnicity, patient_imd, practice_imd
load(file = "filepath")

## Outcome phenotype
contr_presc <- read_tsv("filepath")

## Phenotypes required to define contraception episodes and person time at risk
pregnancy <- read_tsv("filepath")
hysterectomy <- read_tsv("filepath")
sterilisation <- read_tsv("filepath")
cervical_ca <- read_tsv("filepath") 
endometrial_ca <- read_tsv("filepath")
gtd <- read_tsv("filepath")
pid <- read_tsv("filepath")
chlamydia <- read_tsv("filepath")
gonorrhoea <- read_tsv("filepath")
pelvic_tb <- read_tsv("filepath")
breast_ca <- read_tsv("filepath")
decomp_liver_dx_jaundice <- read_tsv("filepath")
decomp_liver_dx_ascites <- read_tsv("filepath")
decomp_liver_dx_cirrhosis <- read_tsv("filepath")
decomp_liver_dx_varices <- read_tsv("filepath")
hep_adenoma <- read_tsv("filepath")
hep_ca <- read_tsv("filepath")
IHD_epicardial <- read_tsv("filepath")
IHD_chd <- read_tsv("filepath")
breast_ca <- read_tsv("filepath")
IHD_epicardial <- read_tsv("filepath")
IHD_chd <- read_tsv("filepath")
stroke_ich <- read_tsv("filepath") 
stroke_isch <- read_tsv("filepath")
stroke_nos <- read_tsv("filepath")
stroke_TIA <- read_tsv("filepath")
vasculardx <-  read_tsv("filepath") 

## Explore raw data ----------------------------------------------------------------

explore_function <- function(data_set) {
  print(glimpse(data_set))
  print(names(data_set))
  print(summary(data_set))
  print(head(data_set))
  print(tail(data_set))
}

explore_function(contr_presc)
explore_function(contraception)
explore_function(pregnancy)
explore_function(hysterectomy)
explore_function(sterilisation)
        
## Tidy data  -----------------------------------------------------------------------------

## contr_presc ---

# Check for and remove duplicates (whole row + by specific vars)
n_distinct(contr_presc) == count(contr_presc) 
distinct_contr_presc <- contr_presc %>% distinct()
n_distinct(distinct_contr_presc) == count(distinct(distinct_contr_presc, patid, eventdate, consid, prodcode, dosageid,qty, numdays, staffid,  .keep_all = TRUE))
distinct_contr_presc <- distinct_contr_presc %>% distinct(patid, eventdate, consid, prodcode, dosageid,qty, numdays, staffid,  .keep_all = TRUE)
count(distinct_contr_presc) 

# Drop unneeded variables
tidy_contr_presc <-  dplyr::select(distinct_contr_presc, -c(sysdate)) 

# Change variables to the correct data type based on CPRD GOLD data specification & mapping
tidy_contr_presc  <-  tidy_contr_presc %>%
  mutate_if(is.numeric, as.integer)
tidy_contr_presc$category <- factor(tidy_contr_presc$category, levels = c(1,2,3,4,5,6,7,8,9,10), 
                                                  labels = c("Condom", "Cervical cap/diaphragm", "Combined vaginal ring", 
                                                             "Combined transdermal patch", "Combined oral contraceptive",
                                                             "Progesterone-only oral contraceptive", "Progesterone-only injectable contraceptive",
                                                             "Implant", "Hormonal intra-uterine system", "Copper intra-uterine device"))
bnfcodes_lookup <- read_tsv("bnfcodes.txt")
tidy_contr_presc$bnfcode <- factor(tidy_contr_presc$bnfcode, levels = c(1:2323), 
                                                 labels = bnfcodes_lookup$bnf)

## Check correct coercion of variable classes
str(tidy_contr_presc)
levels(tidy_contr_presc$category)
levels(tidy_contr_presc$bnfcode)

## Find missing values
any(is.na(tidy_contr_presc))
sum(is.na(tidy_contr_presc)) 
summary(is.na(tidy_contr_presc)) 

## Drop records with missing event dates
tidy_contr_presc <- tidy_contr_presc %>% filter(!is.na(eventdate)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_contr_presc)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_contr_presc$patid, xlab = "patid")
boxplot(tidy_contr_presc$consid, xlab = "consid")
boxplot(tidy_contr_presc$prodcode, xlab = "prodcode")
boxplot(tidy_contr_presc$staffid, xlab = "staffid")
boxplot(tidy_contr_presc$bnfcode, xlab = "bnfcode")
boxplot(tidy_contr_presc$qty, xlab = "qty")
boxplot(tidy_contr_presc$numdays, xlab = "numdays")
boxplot(tidy_contr_presc$numpacks, xlab = "numpacks")
boxplot(tidy_contr_presc$packtype, xlab = "packtype")
boxplot(tidy_contr_presc$issueseq, xlab = "issueseq")
boxplot(tidy_contr_presc$category, xlab = "category")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_contr_presc$patid)
hist(tidy_contr_presc$consid)
hist(tidy_contr_presc$prodcode)
hist(tidy_contr_presc$staffid)
hist(tidy_contr_presc$bnfcode)
hist(tidy_contr_presc$qty)
hist(tidy_contr_presc$numdays)
hist(tidy_contr_presc$numpacks)
hist(tidy_contr_presc$packtype)
hist(tidy_contr_presc$issueseq)
hist(tidy_contr_presc$category)

## contraception ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(contraception) == count(contraception) 
distinct_contraception <- contraception %>% distinct()
n_distinct(distinct_contraception) == count(distinct(distinct_contraception, patid, eventdate, medcode, constype, consid, category,  .keep_all = TRUE))  
distinct_contraception <- distinct_contraception %>% distinct(patid, eventdate, medcode,constype, consid,  category,  .keep_all = TRUE)
count(distinct_contraception) 

## Drop unneeded variables
tidy_contraception <-  dplyr::select(distinct_contraception, -c(sysdate,episode, enttype, 
                                                           adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_contraception <-  tidy_contraception %>% 
  mutate_if(is.numeric, as.integer) 
tidy_contraception$category <- factor(tidy_contraception$category, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), 
                                      labels = c("Condom", "Cervical cap/diaphragm", "Combined vaginal ring", 
                                                 "Combined transdermal patch", "Combined oral contraceptive",
                                                 "Progesterone-only oral contraceptive", "Progesterone-only injectable contraceptive",
                                                 "Implant", "Hormonal intra-uterine system", "Copper intra-uterine device", 
                                                 "Intra-uterine contraception NOS", "Others methods", "Contraception NOS", 
                                                 "Oral contraceptive NOS"))
tidy_contraception$constype <- factor(tidy_contraception$constype, levels = c(0:7),
                                      labels = c("Missing", "Symptom", "Examination", "Diagnosis", "Intervention", 
                                                 "Management", "Administration", "Presenting complaint"))

## Check correct coercion of variable classes
str(tidy_contraception)
levels(tidy_contraception$category)
levels(tidy_contraception$constype)

## Find missing values
any(is.na(tidy_contraception))
sum(is.na(tidy_contraception)) #17,187
summary(is.na(tidy_contraception)) # 17,187 relate to event date 

## Drop records with missing event dates  (cannot know if during eligible period or not)
tidy_contraception <- tidy_contraception %>% filter(!is.na(eventdate)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_contraception)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_contraception$patid, xlab = "patid")
boxplot(tidy_contraception$constype, xlab = "constype")
boxplot(tidy_contraception$consid, xlab = "consid")
boxplot(tidy_contraception$medcode, xlab = "medcode")
boxplot(tidy_contraception$staffid, xlab = "staffid")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_contraception$patid)
hist(tidy_contraception$constype)
hist(tidy_contraception$consid)
hist(tidy_contraception$medcode)
hist(tidy_contraception$medcode)

## pregnancy ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(pregnancy) == count(pregnancy)
distinct_pregnancy<- pregnancy %>% distinct()
n_distinct(distinct_pregnancy) == count(distinct(distinct_pregnancy, patid, eventdate, medcode, .keep_all = TRUE))
count(distinct_pregnancy) 

## Drop unneeded variables 
tidy_pregnancy <- distinct_pregnancy 

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_pregnancy <-  tidy_pregnancy %>% 
  mutate_if(is.numeric, as.integer) 
tidy_pregnancy$pregnancy <- factor(tidy_pregnancy$pregnancy, levels = c(1,2,3,4,5,6,7,8,9,10,11), 
                                      labels = c( "Currently pregnant", "Abnormal product of conception", 
                                                  "Spontaneous abortion", "Termination of pregnancy", "Unspecified abortion",
                                                  "Labour, delivery or condition at birth", "Puerperium or neonatal(within 6 weeks after delivery)",
                                                  "Postnatal (after delivery)", "Pregnancy of childbirth-related terms with vague timing", 
                                                  "Ectopic pregnancy", "Stillbirth, intrauterine or perinatal death"))


## Check correct coercion of variable classes
str(tidy_pregnancy)
levels(tidy_pregnancy$pregnancy)

## Drop events that have same category but different medcodes on same date
count(tidy_pregnancy) == count(distinct(tidy_pregnancy, patid, eventdate, pregnancy)) 
tidy_pregnancy <- tidy_pregnancy %>% distinct(patid, eventdate, pregnancy)
count(tidy_pregnancy)

## Find missing values
any(is.na(tidy_pregnancy)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_pregnancy)

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_pregnancy$patid, xlab = "patid")
boxplot(tidy_pregnancy$medcode, xlab = "medcode")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_pregnancy$patid)
hist(tidy_pregnancy$medcode)

## hysterectomy ---


# Check for and remove duplicates (whole row and by specific vars)
n_distinct(hysterectomy) == count(hysterectomy) 
distinct_hysterectomy <- hysterectomy %>% distinct()
n_distinct(distinct_hysterectomy) == count(distinct(distinct_hysterectomy, patid, eventdate, medcode, constype, consid, category,  .keep_all = TRUE)) 
distinct_hysterectomy <- distinct_hysterectomy %>% distinct(patid, eventdate, category,  .keep_all = TRUE)
count(distinct_hysterectomy) 

## Drop unneeded variables
tidy_hysterectomy <-  dplyr::select(distinct_hysterectomy, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                         adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_hysterectomy <-  tidy_hysterectomy %>% 
  mutate_if(is.numeric, as.integer) 
tidy_hysterectomy$eventdate <- as_date(tidy_hysterectomy$eventdate)
tidy_hysterectomy$category <- factor(tidy_hysterectomy$category, levels = c(1), 
                                      labels = c("Hysterectomy"))

## Check correct coercion of variable classes
str(tidy_hysterectomy)
levels(tidy_hysterectomy$category)

## Find missing values
any(is.na(tidy_hysterectomy)) 
sum(is.na(tidy_hysterectomy)) 
summary(is.na(tidy_hysterectomy))

## Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_hysterectomy <- tidy_hysterectomy %>% filter(!is.na(eventdate)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_hysterectomy) 

## Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_hysterectomy$patid, xlab = "patid")
boxplot(tidy_hysterectomy$medcode, xlab = "medcode")

## Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_hysterectomy$patid)
hist(tidy_hysterectomy$medcode)

## sterilisation --- 

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(sterilisation) == count(sterilisation)
distinct_sterilisation <- sterilisation %>% distinct()
n_distinct(distinct_sterilisation) == count(distinct(distinct_sterilisation, patid, eventdate,  category,  .keep_all = TRUE))
distinct_sterilisation <- distinct_sterilisation %>% distinct(patid, eventdate,  category,  .keep_all = TRUE)
count(distinct_sterilisation) 

## Drop unneeded variables
tidy_sterilisation <-  dplyr::select(distinct_sterilisation, -c(sysdate,episode, enttype, staffid, medcode, constype, consid,
                                                       adid, data1, data2, data3, data4, data5, data6 , data7))

## Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_sterilisation <-  tidy_sterilisation %>% 
  mutate_if(is.numeric, as.integer) 
tidy_sterilisation$eventdate <- as_date(tidy_sterilisation$eventdate)
tidy_sterilisation$category <- factor(tidy_sterilisation$category, levels = c(1, 2, 3), 
                                     labels = c("Male sterilisation", "Female sterilisation",
                                                "Sterilisation NOS"))

## Check correct coercion of variable classes
str(tidy_sterilisation)
levels(tidy_sterilisation$category)

## Find missing values
any(is.na(tidy_sterilisation)) 
sum(is.na(tidy_sterilisation))
summary(is.na(tidy_sterilisation)) 

## Drop records with missing event dates  (cannot know when sterilisation is from therefore when to consider ineligible)
tidy_sterilisation <- tidy_sterilisation %>% filter(!is.na(eventdate)) 

## Look at summary of all variables for outliers and obvious errors
summary(tidy_sterilisation)

## Visualise numeric variables for outliers and obvious errors with boxplots
# boxplot(tidy_sterilisation$patid, xlab = "patid")
# boxplot(tidy_sterilisation$medcode, xlab = "medcode")

## Visualise numeric variables for outliers and obvious errors with histograms 
# hist(tidy_sterilisation$patid)
# hist(tidy_sterilisation$medcode)

# cervical_ca ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(cervical_ca) == count(cervical_ca)
distinct_cervical_ca <- cervical_ca %>% distinct()
n_distinct(distinct_cervical_ca) == count(distinct(distinct_cervical_ca, patid, eventdate, category,  .keep_all = TRUE))
distinct_cervical_ca <- distinct_cervical_ca %>% distinct(patid, eventdate, category,  .keep_all = TRUE)
count(distinct_cervical_ca)

# Drop unneeded variables
tidy_cervical_ca <-  dplyr::select(distinct_cervical_ca, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                       adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_cervical_ca <-  tidy_cervical_ca %>% 
  mutate_if(is.numeric, as.integer) 
tidy_cervical_ca$eventdate <- as_date(tidy_cervical_ca$eventdate)
tidy_cervical_ca$category <- factor(tidy_cervical_ca$category, levels = c(0:1), 
                                     labels = c("Diagnosis of Cervical cancer", "History of Cervical Cancer"))

# Check correct coercion of variable classes
str(tidy_cervical_ca)
levels(tidy_cervical_ca$category)

# Find missing values
any(is.na(tidy_cervical_ca))
sum(is.na(tidy_cervical_ca))
summary(is.na(tidy_cervical_ca)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_cervical_ca <- tidy_cervical_ca %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_cervical_ca)

# Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_cervical_ca$patid, xlab = "patid")

# Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_cervical_ca$patid)
hist(tidy_cervical_ca$category)

## endometrial_ca ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(endometrial_ca) == count(endometrial_ca)
distinct_endometrial_ca <- endometrial_ca %>% distinct()
n_distinct(distinct_endometrial_ca) == count(distinct(distinct_endometrial_ca, patid, eventdate, category,  .keep_all = TRUE))
distinct_endometrial_ca <- distinct_endometrial_ca %>% distinct(patid, eventdate, category,  .keep_all = TRUE)
count(distinct_endometrial_ca)

# Drop unneeded variables
tidy_endometrial_ca <-  dplyr::select(distinct_endometrial_ca, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                     adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_endometrial_ca <-  tidy_endometrial_ca %>% 
  mutate_if(is.numeric, as.integer) 
tidy_endometrial_ca$eventdate <- as_date(tidy_endometrial_ca$eventdate)
tidy_endometrial_ca$category <- factor(tidy_endometrial_ca$category, levels = c(0:1), 
                                    labels = c("Diagnosis of Endometrial Cancer", "History of Endometrial Cancer"))

# Check correct coercion of variable classes
str(tidy_endometrial_ca)
levels(tidy_endometrial_ca$category)

# Find missing values
any(is.na(tidy_endometrial_ca))
sum(is.na(tidy_endometrial_ca))
summary(is.na(tidy_endometrial_ca))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_endometrial_ca <- tidy_endometrial_ca %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_endometrial_ca) # 589

# Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_endometrial_ca$patid, xlab = "patid")
# Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_endometrial_ca$patid)
hist(tidy_endometrial_ca$category)


## gtd ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(gtd) == count(gtd)
distinct_gtd <- gtd %>% distinct()
n_distinct(distinct_gtd) == count(distinct(distinct_gtd, patid, eventdate, category,  .keep_all = TRUE)) 
distinct_gtd <- distinct_gtd %>% distinct(patid, eventdate, category,  .keep_all = TRUE)
count(distinct_gtd)

# Drop unneeded variables
tidy_gtd <-  dplyr::select(distinct_gtd, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                           adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_gtd <-  tidy_gtd %>% 
  mutate_if(is.numeric, as.integer) 
tidy_gtd$eventdate <- as_date(tidy_gtd$eventdate)
tidy_gtd$category <- factor(tidy_gtd$category, levels = c(1:3), 
                                       labels = c("Definite", "Probable", "Possible"))

# Check correct coercion of variable classes
str(tidy_gtd)
levels(tidy_gtd$category)

# Find missing values
any(is.na(tidy_gtd)) 
sum(is.na(tidy_gtd)) 
summary(is.na(tidy_gtd)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_gtd <- tidy_gtd %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_gtd)

# Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_endometrial_ca$patid, xlab = "patid")
# Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_endometrial_ca$patid)
hist(tidy_endometrial_ca$category)

## pid ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(pid) == count(pid)
distinct_pid <- pid %>% distinct()
n_distinct(distinct_pid) == count(distinct(distinct_pid, patid, eventdate, category,  .keep_all = TRUE)) 
distinct_pid <- distinct_pid %>% distinct(patid, eventdate, category,  .keep_all = TRUE)
count(distinct_pid)

# Drop unneeded variables
tidy_pid <-  dplyr::select(distinct_pid, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                     adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_pid <-  tidy_pid %>% 
  mutate_if(is.numeric, as.integer) 
tidy_pid$eventdate <- as_date(tidy_pid$eventdate)
tidy_pid$category <- factor(tidy_pid$category, levels = c(1), 
                            labels = c("Definite"))

# Check correct coercion of variable classes
str(tidy_pid)
levels(tidy_pid$category)

# Find missing values
any(is.na(tidy_pid))
sum(is.na(tidy_pid))
summary(is.na(tidy_pid))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_pid <- tidy_pid %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_pid)

# Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_pid$patid, xlab = "patid")
# Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_pid$patid)
hist(tidy_pid$category)

## chlamydia ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(chlamydia) == count(chlamydia)
distinct_chlamydia <- chlamydia %>% distinct()
n_distinct(distinct_chlamydia) == count(distinct(distinct_chlamydia, patid, eventdate, category,  .keep_all = TRUE))
distinct_chlamydia <- distinct_chlamydia %>% distinct(patid, eventdate, category,  .keep_all = TRUE)
count(distinct_chlamydia)

# Drop unneeded variables
tidy_chlamydia <-  dplyr::select(distinct_chlamydia, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                     adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_chlamydia <-  tidy_chlamydia %>% 
  mutate_if(is.numeric, as.integer) 
tidy_chlamydia$eventdate <- as_date(tidy_chlamydia$eventdate)
tidy_chlamydia$category <- factor(tidy_chlamydia$category, levels = c(1), 
                            labels = c("Chlamydia"))

# Check correct coercion of variable classes
str(tidy_chlamydia)
levels(tidy_chlamydia$category)

# Find missing values
any(is.na(tidy_chlamydia))
sum(is.na(tidy_chlamydia))
summary(is.na(tidy_chlamydia))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_chlamydia <- tidy_chlamydia %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_chlamydia)

# Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_chlamydia$patid, xlab = "patid")
# Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_chlamydia$patid)
hist(tidy_chlamydia$category)

## gonorrhoea ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(gonorrhoea) == count(gonorrhoea)
distinct_gonorrhoea <- gonorrhoea %>% distinct()
n_distinct(distinct_gonorrhoea) == count(distinct(distinct_gonorrhoea, patid, eventdate, category,  .keep_all = TRUE)) 
distinct_gonorrhoea <- distinct_gonorrhoea %>% distinct(patid, eventdate, category,  .keep_all = TRUE)
count(distinct_gonorrhoea) 

# Drop unneeded variables
tidy_gonorrhoea <-  dplyr::select(distinct_gonorrhoea, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                 adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_gonorrhoea <-  tidy_gonorrhoea %>% 
  mutate_if(is.numeric, as.integer) 
tidy_gonorrhoea$eventdate <- as_date(tidy_gonorrhoea$eventdate)
tidy_gonorrhoea$category <- factor(tidy_gonorrhoea$category, levels = c(1), 
                                  labels = c("gonorrhoea"))

# Check correct coercion of variable classes
str(tidy_gonorrhoea)
levels(tidy_gonorrhoea$category)

# Find missing values
any(is.na(tidy_gonorrhoea))
sum(is.na(tidy_gonorrhoea))
summary(is.na(tidy_gonorrhoea))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_gonorrhoea <- tidy_gonorrhoea %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_gonorrhoea)

# Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_gonorrhoea$patid, xlab = "patid")
# Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_gonorrhoea$patid)
hist(tidy_gonorrhoea$category)

## pelvic_tb ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(pelvic_tb) == count(pelvic_tb)
distinct_pelvic_tb <- pelvic_tb %>% distinct()
n_distinct(distinct_pelvic_tb) == count(distinct(distinct_pelvic_tb, patid, eventdate, category,  .keep_all = TRUE))
distinct_pelvic_tb <- distinct_pelvic_tb %>% distinct(patid, eventdate, category,  .keep_all = TRUE)
count(distinct_pelvic_tb)

# Drop unneeded variables
tidy_pelvic_tb <-  dplyr::select(distinct_pelvic_tb, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_pelvic_tb <-  tidy_pelvic_tb %>% 
  mutate_if(is.numeric, as.integer) 
tidy_pelvic_tb$eventdate <- as_date(tidy_pelvic_tb$eventdate)
tidy_pelvic_tb$category <- factor(tidy_pelvic_tb$category, levels = c(1), 
                                   labels = c("pelvic_tb"))

# Check correct coercion of variable classes
str(tidy_pelvic_tb)
levels(tidy_pelvic_tb$category)

# Find missing values
any(is.na(tidy_pelvic_tb))

# Look at summary of all variables for outliers and obvious errors
summary(tidy_pelvic_tb)

# Visualise numeric variables for outliers and obvious errors with boxplots
boxplot(tidy_pelvic_tb$patid, xlab = "patid")
# Visualise numeric variables for outliers and obvious errors with histograms 
hist(tidy_pelvic_tb$patid)
hist(tidy_pelvic_tb$category)


## breast_ca ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(breast_ca) == count(breast_ca)
distinct_breast_ca <- breast_ca %>% distinct()
n_distinct(distinct_breast_ca) == count(distinct(distinct_breast_ca, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_breast_ca <- distinct_breast_ca %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_breast_ca)

# Drop unneeded variables
tidy_breast_ca <-  dplyr::select(distinct_breast_ca, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                 adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_breast_ca <-  tidy_breast_ca %>% 
  mutate_if(is.numeric, as.integer) 
tidy_breast_ca$eventdate <- as_date(tidy_breast_ca$eventdate)
tidy_breast_ca$category <- factor(tidy_breast_ca$category, levels = c(0:1), 
                                  labels = c("Diagnosis of Breast Cancer", "History of Breast Cancer"))

# Check correct coercion of variable classes
str(tidy_breast_ca)
levels(tidy_breast_ca$category)

# Find missing values
any(is.na(tidy_breast_ca))
sum(is.na(tidy_breast_ca))
summary(is.na(tidy_breast_ca))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_breast_ca <- tidy_breast_ca %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_breast_ca)

## decomp_liver_dx_jaundice ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(decomp_liver_dx_jaundice) == count(decomp_liver_dx_jaundice)
distinct_decomp_liver_dx_jaundice <- decomp_liver_dx_jaundice %>% distinct()
n_distinct(distinct_decomp_liver_dx_jaundice) == count(distinct(distinct_decomp_liver_dx_jaundice, patid, eventdate, medcode, category,  .keep_all = TRUE))
distinct_decomp_liver_dx_jaundice <- distinct_decomp_liver_dx_jaundice %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_decomp_liver_dx_jaundice)

# Drop unneeded variables
tidy_decomp_liver_dx_jaundice <-  dplyr::select(distinct_decomp_liver_dx_jaundice, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                 adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_decomp_liver_dx_jaundice <-  tidy_decomp_liver_dx_jaundice %>% 
  mutate_if(is.numeric, as.integer) 
tidy_decomp_liver_dx_jaundice$eventdate <- as_date(tidy_decomp_liver_dx_jaundice$eventdate)
tidy_decomp_liver_dx_jaundice$category <- factor(tidy_decomp_liver_dx_jaundice$category, levels = c(1:3), 
                                  labels = c("History of jaundice", "Probable", "Definite"))

# Check correct coercion of variable classes
str(tidy_decomp_liver_dx_jaundice)
levels(tidy_decomp_liver_dx_jaundice$category)

# Find missing values
any(is.na(tidy_decomp_liver_dx_jaundice))
sum(is.na(tidy_decomp_liver_dx_jaundice))
summary(is.na(tidy_decomp_liver_dx_jaundice))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_decomp_liver_dx_jaundice <- tidy_decomp_liver_dx_jaundice %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_decomp_liver_dx_jaundice) 
count(tidy_decomp_liver_dx_jaundice)


## decomp_liver_dx_ascites ---


# Check for and remove duplicates (whole row and by specific vars)
n_distinct(decomp_liver_dx_ascites) == count(decomp_liver_dx_ascites)
distinct_decomp_liver_dx_ascites <- decomp_liver_dx_ascites %>% distinct()
n_distinct(distinct_decomp_liver_dx_ascites) == count(distinct(distinct_decomp_liver_dx_ascites, patid, eventdate, medcode, category,  .keep_all = TRUE))
distinct_decomp_liver_dx_ascites <- distinct_decomp_liver_dx_ascites %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_decomp_liver_dx_ascites)

# Drop unneeded variables
tidy_decomp_liver_dx_ascites <-  dplyr::select(distinct_decomp_liver_dx_ascites, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                       adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_decomp_liver_dx_ascites <-  tidy_decomp_liver_dx_ascites %>% 
  mutate_if(is.numeric, as.integer) 
tidy_decomp_liver_dx_ascites$eventdate <- as_date(tidy_decomp_liver_dx_ascites$eventdate)
tidy_decomp_liver_dx_ascites$category <- factor(tidy_decomp_liver_dx_ascites$category, levels = c(2:4), 
                                     labels = c("Probable Ascites", "Definite Ascites", "Spontaneous Bacterial Peritonitis"))

# Check correct coercion of variable classes
str(tidy_decomp_liver_dx_ascites)
levels(tidy_decomp_liver_dx_ascites$category)

# Find missing values
any(is.na(tidy_decomp_liver_dx_ascites))
sum(is.na(tidy_decomp_liver_dx_ascites))
summary(is.na(tidy_decomp_liver_dx_ascites))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_decomp_liver_dx_ascites <- tidy_decomp_liver_dx_ascites %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_decomp_liver_dx_ascites)
count(tidy_decomp_liver_dx_ascites)

## decomp_liver_dx_cirrhosis ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(decomp_liver_dx_cirrhosis) == count(decomp_liver_dx_cirrhosis) 
distinct_decomp_liver_dx_cirrhosis <- decomp_liver_dx_cirrhosis %>% distinct()
n_distinct(distinct_decomp_liver_dx_cirrhosis) == count(distinct(distinct_decomp_liver_dx_cirrhosis, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_decomp_liver_dx_cirrhosis <- distinct_decomp_liver_dx_cirrhosis %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_decomp_liver_dx_cirrhosis)

# Drop unneeded variables
tidy_decomp_liver_dx_cirrhosis <- dplyr::select(distinct_decomp_liver_dx_cirrhosis, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                                                  adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_decomp_liver_dx_cirrhosis <-  tidy_decomp_liver_dx_cirrhosis %>% 
  mutate_if(is.numeric, as.integer) 
tidy_decomp_liver_dx_cirrhosis$eventdate <- as_date(tidy_decomp_liver_dx_cirrhosis$eventdate)
tidy_decomp_liver_dx_cirrhosis$category <- factor(tidy_decomp_liver_dx_cirrhosis$category, levels = c(2:6), 
                                                labels = c("Possible alcoholic cirrhosis", "Possible non-alcoholic cirrhosis/unspecified cirrhosis",
                                                           "Alcoholic cirrhosis", "Non-alcoholic cirrhosis/unspecified cirrhosis",
                                                           "Congenital cirrhosis"))

# Check correct coercion of variable classes
str(tidy_decomp_liver_dx_cirrhosis)
levels(tidy_decomp_liver_dx_cirrhosis$category)

# Find missing values
any(is.na(tidy_decomp_liver_dx_cirrhosis))
sum(is.na(tidy_decomp_liver_dx_cirrhosis))
summary(is.na(tidy_decomp_liver_dx_cirrhosis)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_decomp_liver_dx_cirrhosis <- tidy_decomp_liver_dx_cirrhosis %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_decomp_liver_dx_cirrhosis)
count(tidy_decomp_liver_dx_cirrhosis)

## decomp_liver_dx_varices ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(decomp_liver_dx_varices) == count(decomp_liver_dx_varices)
distinct_decomp_liver_dx_varices <- decomp_liver_dx_varices %>% distinct()
n_distinct(distinct_decomp_liver_dx_varices) == count(distinct(distinct_decomp_liver_dx_varices, patid, eventdate, medcode, category,  .keep_all = TRUE))
distinct_decomp_liver_dx_varices <- distinct_decomp_liver_dx_varices %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_decomp_liver_dx_varices)

# Drop unneeded variables
tidy_decomp_liver_dx_varices <- dplyr::select(distinct_decomp_liver_dx_varices, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                                            adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_decomp_liver_dx_varices <-  tidy_decomp_liver_dx_varices %>% 
  mutate_if(is.numeric, as.integer) 
tidy_decomp_liver_dx_varices$eventdate <- as_date(tidy_decomp_liver_dx_varices$eventdate)
tidy_decomp_liver_dx_varices$category <- factor(tidy_decomp_liver_dx_varices$category, levels = c(3:4), 
                                                  labels = c("Definite without bleeding/unspecified", "Definite with bleeding"))

# Check correct coercion of variable classes
str(tidy_decomp_liver_dx_varices)
levels(tidy_decomp_liver_dx_varices$category)

# Find missing values
any(is.na(tidy_decomp_liver_dx_varices))
sum(is.na(tidy_decomp_liver_dx_varices))
summary(is.na(tidy_decomp_liver_dx_varices))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_decomp_liver_dx_varices <- tidy_decomp_liver_dx_varices %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_decomp_liver_dx_varices)
count(tidy_decomp_liver_dx_varices)

## hep_adenoma ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(hep_adenoma) == count(hep_adenoma)
distinct_hep_adenoma <- hep_adenoma %>% distinct()
n_distinct(distinct_hep_adenoma) == count(distinct(distinct_hep_adenoma, patid, eventdate, medcode,  category , .keep_all = TRUE))
distinct_hep_adenoma <- distinct_hep_adenoma %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_hep_adenoma)

# Drop unneeded variables
tidy_hep_adenoma <- dplyr::select(distinct_hep_adenoma, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                                                adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_hep_adenoma <-  tidy_hep_adenoma %>% 
  mutate_if(is.numeric, as.integer) 
tidy_hep_adenoma$eventdate <- as_date(tidy_hep_adenoma$eventdate)
tidy_hep_adenoma$category <- factor(tidy_hep_adenoma$category, levels = c(1:2), 
                                                  labels = c("Definite", "Probable"))

# Check correct coercion of variable classes
str(tidy_hep_adenoma)
levels(tidy_hep_adenoma$category)

# Find missing values
any(is.na(tidy_hep_adenoma)) # False

# Look at summary of all variables for outliers and obvious errors
summary(tidy_hep_adenoma)
count(tidy_hep_adenoma)


## hep_ca ---


# Check for and remove duplicates (whole row and by specific vars)
n_distinct(hep_ca) == count(hep_ca)
distinct_hep_ca <- hep_ca %>% distinct()
n_distinct(distinct_hep_ca) == count(distinct(distinct_hep_ca, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_hep_ca <- distinct_hep_ca %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_hep_ca) 

# Drop unneeded variables
tidy_hep_ca <- dplyr::select(distinct_hep_ca, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                                                adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_hep_ca <-  tidy_hep_ca %>% 
  mutate_if(is.numeric, as.integer) 
tidy_hep_ca$eventdate <- as_date(tidy_hep_ca$eventdate)
tidy_hep_ca$category <- factor(tidy_hep_ca$category, levels = c(0), 
                                                  labels = c("Diagnosis of Liver Ca"))

# Check correct coercion of variable classes
str(tidy_hep_ca)
levels(tidy_hep_ca$category)

# Find missing values
any(is.na(tidy_hep_ca))

# Look at summary of all variables for outliers and obvious errors
summary(tidy_hep_ca) 


## IHD_epicardial ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(IHD_epicardial) == count(IHD_epicardial)
distinct_IHD_epicardial <- IHD_epicardial %>% distinct()
n_distinct(distinct_IHD_epicardial) == count(distinct(distinct_IHD_epicardial, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_IHD_epicardial <- distinct_IHD_epicardial %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_IHD_epicardial) 

# Drop unneeded variables
tidy_IHD_epicardial <- dplyr::select(distinct_IHD_epicardial, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                          adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_IHD_epicardial <-  tidy_IHD_epicardial %>% 
  mutate_if(is.numeric, as.integer) 
tidy_IHD_epicardial$eventdate <- as_date(tidy_IHD_epicardial$eventdate)
tidy_IHD_epicardial$category <- factor(tidy_IHD_epicardial$category, levels = c(1), 
                               labels = c("Epicardial coronary heart disease"))

# Check correct coercion of variable classes
str(tidy_IHD_epicardial)
levels(tidy_IHD_epicardial$category)

# Find missing values
any(is.na(tidy_IHD_epicardial))
sum(is.na(tidy_IHD_epicardial)) 
summary(is.na(tidy_IHD_epicardial)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_IHD_epicardial <- tidy_IHD_epicardial %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_IHD_epicardial) 

## IHD_chd ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(IHD_chd) == count(IHD_chd)
distinct_IHD_chd <- IHD_chd %>% distinct()
n_distinct(distinct_IHD_chd) == count(distinct(distinct_IHD_chd, patid, eventdate, medcode, chd_nos_cprd,  .keep_all = TRUE)) 
distinct_IHD_chd <- distinct_IHD_chd %>% distinct(patid, eventdate, medcode, chd_nos_cprd,  .keep_all = TRUE)
count(distinct_IHD_chd) 

# Drop unneeded variables
tidy_IHD_chd <- dplyr::select(distinct_IHD_chd, -c(enttype))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_IHD_chd <-  tidy_IHD_chd %>% 
  mutate_if(is.numeric, as.integer) 
tidy_IHD_chd$eventdate <- as_date(tidy_IHD_chd$eventdate)
tidy_IHD_chd$chd_nos_cprd <- factor(tidy_IHD_chd$chd_nos_cprd, levels = c(1:3), 
                               labels = c("History of Coronary Heart Disease", "Possible coronary heart disease", "Diagnosed coronary heart disease"))

# Check correct coercion of variable classes
str(tidy_IHD_chd)
levels(tidy_IHD_chd$chd_nos_cprd)

# Find missing values
any(is.na(tidy_IHD_chd)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_IHD_chd)
count(tidy_IHD_chd) 

## stroke_ich ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(stroke_ich) == count(stroke_ich) 
distinct_stroke_ich <- stroke_ich %>% distinct()
n_distinct(distinct_stroke_ich) == count(distinct(distinct_stroke_ich, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_stroke_ich <- distinct_stroke_ich %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_stroke_ich)

# Drop unneeded variables
tidy_stroke_ich <- dplyr::select(distinct_stroke_ich, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                          adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_stroke_ich <-  tidy_stroke_ich %>% 
  mutate_if(is.numeric, as.integer) 
tidy_stroke_ich$eventdate <- as_date(tidy_stroke_ich$eventdate)
tidy_stroke_ich$category <- factor(tidy_stroke_ich$category, levels = c(0:1), 
                               labels = c("History of Intracerebral Haemorrhage", "Diagnosis of Intracerebral Haemorrhage"))

# Check correct coercion of variable classes
str(tidy_stroke_ich)
levels(tidy_stroke_ich$category)

# Find missing values
any(is.na(tidy_stroke_ich)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_stroke_ich)

## stroke_isch ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(stroke_isch) == count(stroke_isch) 
distinct_stroke_isch <- stroke_isch %>% distinct()
n_distinct(distinct_stroke_isch) == count(distinct(distinct_stroke_isch, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_stroke_isch <- distinct_stroke_isch %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_stroke_isch) 

# Drop unneeded variables
tidy_stroke_isch <- dplyr::select(distinct_stroke_isch, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                          adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_stroke_isch <-  tidy_stroke_isch %>% 
  mutate_if(is.numeric, as.integer) 
tidy_stroke_isch$eventdate <- as_date(tidy_stroke_isch$eventdate)
tidy_stroke_isch$category <- factor(tidy_stroke_isch$category, levels = c(0:1), 
                               labels = c("Diagnosis of Ischaemic Stroke", " History of Ischaemic Stroke"))

# Check correct coercion of variable classes
str(tidy_stroke_isch)
levels(tidy_stroke_isch$category)

# Find missing values
any(is.na(tidy_stroke_isch)) 
sum(is.na(tidy_stroke_isch)) 
summary(is.na(tidy_stroke_isch)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_stroke_isch <- tidy_stroke_isch %>% filter(!is.na(eventdate)) 


# Look at summary of all variables for outliers and obvious errors
summary(tidy_stroke_isch) 

## stroke_nos ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(stroke_nos) == count(stroke_nos) 
distinct_stroke_nos <- stroke_nos %>% distinct()
n_distinct(distinct_stroke_nos) == count(distinct(distinct_stroke_nos, patid, eventdate, medcode, category,  .keep_all = TRUE))
distinct_stroke_nos <- distinct_stroke_nos %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_stroke_nos)

# Drop unneeded variables
tidy_stroke_nos <- dplyr::select(distinct_stroke_nos, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                          adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_stroke_nos <-  tidy_stroke_nos %>% 
  mutate_if(is.numeric, as.integer) 
tidy_stroke_nos$eventdate <- as_date(tidy_stroke_nos$eventdate)
tidy_stroke_nos$category <- factor(tidy_stroke_nos$category, levels = c(0:1), 
                               labels = c("History of Stroke NOS", "Diagnosis of Stroke NOS"))

# Check correct coercion of variable classes
str(tidy_stroke_nos)
levels(tidy_stroke_nos$category)

# Find missing values
any(is.na(tidy_stroke_nos)) 
sum(is.na(tidy_stroke_nos)) 
summary(is.na(tidy_stroke_nos))  

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_stroke_nos <- tidy_stroke_nos %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_stroke_nos)
count(tidy_stroke_nos)

## stroke_TIA ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(stroke_TIA) == count(stroke_TIA) 
distinct_stroke_TIA <- stroke_TIA %>% distinct()
n_distinct(distinct_stroke_TIA) == count(distinct(distinct_stroke_TIA, patid, eventdate, medcode, category,  .keep_all = TRUE))  
distinct_stroke_TIA <- distinct_stroke_TIA %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_stroke_TIA) 

# Drop unneeded variables
tidy_stroke_TIA <- dplyr::select(distinct_stroke_TIA, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                  adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_stroke_TIA <-  tidy_stroke_TIA %>% 
  mutate_if(is.numeric, as.integer) 
tidy_stroke_TIA$eventdate <- as_date(tidy_stroke_TIA$eventdate)
tidy_stroke_TIA$category <- factor(tidy_stroke_TIA$category, levels = c(0:1), 
                                   labels = c("History of TIA", "Diagnosis of TIA"))

# Check correct coercion of variable classes
str(tidy_stroke_TIA)
levels(tidy_stroke_TIA$category)

# Find missing values
any(is.na(tidy_stroke_TIA)) 
sum(is.na(tidy_stroke_TIA)) 
summary(is.na(tidy_stroke_TIA)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_stroke_TIA <- tidy_stroke_TIA %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_stroke_TIA)
count(tidy_stroke_TIA)

## smoking ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(smoking) == count(smoking)
distinct_smoking <- smoking %>% distinct()
n_distinct(distinct_smoking) == count(distinct(distinct_smoking, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_smoking <- distinct_smoking %>% distinct(patid, eventdate, medcode, category,  .keep_all = TRUE)
count(distinct_smoking)

# Drop unneeded variables
tidy_smoking <- dplyr::select(distinct_smoking, -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                            adid, data1, data2, data3, data4, data5, data6 , data7))

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_smoking <-  tidy_smoking %>% 
  mutate_if(is.numeric, as.integer) 
tidy_smoking$eventdate <- as_date(tidy_smoking$eventdate)
tidy_smoking$category <- factor(tidy_smoking$category, levels = c(1:4), 
                                   labels = c("Non-smoker", "Ex-smoker", "Ex or current smoker", "Current smoker"))

# Check correct coercion of variable classes
str(tidy_smoking)
levels(tidy_smoking$category)

# Find missing values
any(is.na(tidy_smoking))
sum(is.na(tidy_smoking)) 
summary(is.na(tidy_smoking)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_smoking <- tidy_smoking %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_smoking)
count(tidy_smoking) 

## obesity --

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(obesity) == count(obesity)
distinct_obesity <- obesity %>% distinct()
n_distinct(distinct_obesity) == count(distinct(distinct_obesity, patid, eventdate, medcode, obesity_cat,  .keep_all = TRUE))
distinct_obesity <- distinct_obesity %>% distinct(patid, eventdate, medcode, obesity_cat,  .keep_all = TRUE)
count(distinct_obesity) 

# Drop unneeded variables
tidy_obesity <- distinct_obesity 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_obesity <-  tidy_obesity %>% 
  mutate_if(is.numeric, as.integer) 
tidy_obesity$obesity_cat <- factor(tidy_obesity$obesity_cat, levels = c(1:7), 
                                   labels = c("Underweight", "Normal weight", "Overweight", "History of obesity", 
                                              "POssibly overweight/obese", "Obese", "Morbidly/severely obese"))

# Check correct coercion of variable classes
str(tidy_obesity)
levels(tidy_obesity$obesity_cat)

# Find missing values
any(is.na(tidy_obesity)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_obesity)
count(tidy_obesity)

## diabetes_diag ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(diabetes_diag) == count(diabetes_diag) 
distinct_diabetes_diag <- diabetes_diag %>% distinct()
n_distinct(distinct_diabetes_diag) == count(distinct(distinct_diabetes_diag, patid, eventdate, medcode, diabdiag_cprd,  .keep_all = TRUE)) 
distinct_diabetes_diag <- distinct_diabetes_diag %>% distinct(patid, eventdate, medcode, diabdiag_cprd,  .keep_all = TRUE)
count(distinct_diabetes_diag) 

# Drop unneeded variables
tidy_diabetes_diag <- distinct_diabetes_diag

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_diabetes_diag <-  tidy_diabetes_diag %>% 
  mutate_if(is.numeric, as.integer) 
tidy_diabetes_diag$diabdiag_cprd <- factor(tidy_diabetes_diag$diabdiag_cprd, levels = c(3:4), 
                                labels = c("Type I Diabetes Mellitus", "Type II Diabetes Mellitus"))

# Check correct coercion of variable classes
str(tidy_diabetes_diag)
levels(tidy_diabetes_diag$diabdiag_cprd)

# Find missing values
any(is.na(tidy_diabetes_diag))

# Look at summary of all variables for outliers and obvious errors
summary(tidy_diabetes_diag)
count(tidy_diabetes_diag) 

## dm ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(dm) == count(dm) 
distinct_dm <- dm %>% distinct()
n_distinct(distinct_dm) == count(distinct(distinct_dm, patid, eventdate, medcode, dm_cprd,  .keep_all = TRUE)) 
distinct_dm <- distinct_dm %>% distinct(patid, eventdate, medcode, dm_cprd,  .keep_all = TRUE)
count(distinct_dm)

# Drop unneeded variables
tidy_dm <- distinct_dm 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_dm <-  tidy_dm %>% 
  mutate_if(is.numeric, as.integer) 
tidy_dm$dm_cprd <- factor(tidy_dm$dm_cprd, levels = c(1:8), 
                                      labels = c("History of diabetes", "Possible diabetes", "Insulin dependent diabetes",
                                                 "Non-insulin dependent diabetes", "Secondary diabetes", "Diabetes nos",
                                                 "Diabetes excluded", "Diabetes resolved"))

# Check correct coercion of variable classes
str(tidy_dm)
levels(tidy_dm$dm_cprd)

# Find missing values
any(is.na(tidy_dm))

# Look at summary of all variables for outliers and obvious errors
summary(tidy_dm)

## hypertension ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(hypertension) == count(hypertension) 
distinct_hypertension <- hypertension %>% distinct()
n_distinct(distinct_hypertension) == count(distinct(distinct_hypertension, patid, eventdate, medcode, bp_cprd,  .keep_all = TRUE)) 
distinct_hypertension <- distinct_hypertension %>% distinct(patid, eventdate, medcode, bp_cprd,  .keep_all = TRUE)
count(distinct_hypertension) 

# Drop unneeded variables
tidy_hypertension <- distinct_hypertension

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_hypertension <-  tidy_hypertension %>% 
  mutate_if(is.numeric, as.integer) 
tidy_hypertension$bp_cprd <- factor(tidy_hypertension$bp_cprd, levels = c(0:4), 
                                    labels = c("No cat", "Ordinary blood pressure measurement", "Home blood pressure measurement",
                                      "Ambulatory blood pressure measurement", "Target blood pressure measurement"))

# Check correct coercion of variable classes
str(tidy_hypertension)
levels(tidy_hypertension$bp_cprd)

# Find missing values
any(is.na(tidy_hypertension)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_hypertension)


## vasculardx ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(vasculardx) == count(vasculardx) 
distinct_vasculardx <- vasculardx %>% distinct()
n_distinct(distinct_vasculardx) == count(distinct(distinct_vasculardx, patid, eventdate, medcode, category,  .keep_all = TRUE))
distinct_vasculardx <- distinct_vasculardx %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_vasculardx) 

# Drop unneeded variables
tidy_vasculardx <- dplyr::select(distinct_vasculardx,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_vasculardx <-  tidy_vasculardx %>% 
  mutate_if(is.numeric, as.integer) 
tidy_vasculardx$eventdate <- as_date(tidy_vasculardx$eventdate)
tidy_vasculardx$category <- factor(tidy_vasculardx$category, levels = c(1:9), 
                                    labels = c("History of peripheral arterial disease", "Suspected peripheral arterial disease",
                                               "Non-specific arterial disease or atherosclerosis, excluding legs", 
                                               "Abdominal aortic aneurysm", "Aortic dissection +/- aneurysm",
                                               "Other arterial aneurysm or dissection (not coronary or cerebral)",
                                               "Peripheral vascular disease", "Leg or aortic embolism or thrombosis",
                                               "Raynaud phenomenon"))

# Check correct coercion of variable classes
str(tidy_vasculardx)
levels(tidy_vasculardx$category)

# Find missing values
any(is.na(tidy_vasculardx)) 
sum(is.na(tidy_vasculardx)) 
summary(is.na(tidy_vasculardx))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_vasculardx <- tidy_vasculardx %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_vasculardx)
count(tidy_vasculardx)

##vte_not_pe ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(vte_not_pe) == count(vte_not_pe) 
distinct_vte_not_pe <- vte_not_pe %>% distinct()
n_distinct(distinct_vte_not_pe) == count(distinct(distinct_vte_not_pe, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_vte_not_pe <- distinct_vte_not_pe %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_vte_not_pe) 

# Drop unneeded variables
tidy_vte_not_pe <- dplyr::select(distinct_vte_not_pe,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_vte_not_pe <-  tidy_vte_not_pe %>% 
  mutate_if(is.numeric, as.integer) 
tidy_vte_not_pe$eventdate <- as_date(tidy_vte_not_pe$eventdate)
tidy_vte_not_pe$category <- factor(tidy_vte_not_pe$category, levels = c(0:1), 
                                   labels = c("History of VTE not PE", "Diagnosis of VTE not PE"))

# Check correct coercion of variable classes
str(tidy_vte_not_pe)
levels(tidy_vte_not_pe$category)

# Find missing values
any(is.na(tidy_vte_not_pe))
sum(is.na(tidy_vte_not_pe)) 
summary(is.na(tidy_vte_not_pe)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_vte_not_pe <- tidy_vte_not_pe %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_vte_not_pe)
count(tidy_vte_not_pe) 

## pe ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(pe) == count(pe) 
distinct_pe <- pe %>% distinct()
n_distinct(distinct_pe) == count(distinct(distinct_pe, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_pe <- distinct_pe %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_pe) 

# Drop unneeded variables
tidy_pe <- dplyr::select(distinct_pe,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_pe <-  tidy_pe %>% 
  mutate_if(is.numeric, as.integer) 
tidy_pe$eventdate <- as_date(tidy_pe$eventdate)
tidy_pe$category <- factor(tidy_pe$category, levels = c(0:2), 
                                   labels = c("History of PE", "Procedure for PE", "Diagnosis of PE"))

# Check correct coercion of variable classes
str(tidy_pe)
levels(tidy_pe$category)

# Find missing values
any(is.na(tidy_pe)) 
sum(is.na(tidy_pe))
summary(is.na(tidy_pe))

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_pe <- tidy_pe %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_pe)
count(tidy_pe) 

## vte_tx ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(vte_tx) == count(vte_tx)
distinct_vte_tx <- vte_tx %>% distinct()
n_distinct(distinct_vte_tx) == count(distinct(distinct_vte_tx, patid, eventdate,anticoagulants_and_protamine_cprdprod,  .keep_all = TRUE))  
distinct_vte_tx <- distinct_vte_tx %>% distinct(patid, eventdate, anticoagulants_and_protamine_cprdprod,  .keep_all = TRUE)
count(distinct_vte_tx) 

# Drop unneeded variables
tidy_vte_tx <- dplyr::select(distinct_vte_tx, c(patid, eventdate, anticoagulants_and_protamine_cprdprod)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_vte_tx <-  tidy_vte_tx %>% 
  mutate_if(is.numeric, as.integer) 
tidy_vte_tx$eventdate <- as_date(tidy_vte_tx$eventdate)
tidy_vte_tx$anticoagulants_and_protamine_cprdprod <- factor(tidy_vte_tx$anticoagulants_and_protamine_cprdprod, levels = c(1:2), 
                                   labels = c("Oral anticoagulants", "Parenteral anticoagulants"))

# Check correct coercion of variable classes
str(tidy_vte_tx)
levels(tidy_vte_tx$anticoagulants_and_protamine_cprdprod)

# Find missing values
any(is.na(tidy_vte_tx)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_vte_tx)
count(tidy_vte_tx) 

## thrombophilias ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(thrombophilias) == count(thrombophilias) 
distinct_thrombophilias <- thrombophilias %>% distinct()
n_distinct(distinct_thrombophilias) == count(distinct(distinct_thrombophilias, patid, eventdate, medcode, category,  .keep_all = TRUE))  
distinct_thrombophilias <- distinct_thrombophilias %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_thrombophilias) 

# Drop unneeded variables
tidy_thrombophilias <- dplyr::select(distinct_thrombophilias,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_thrombophilias <-  tidy_thrombophilias %>% 
  mutate_if(is.numeric, as.integer) 
tidy_thrombophilias$eventdate <- as_date(tidy_thrombophilias$eventdate)
tidy_thrombophilias$category <- factor(tidy_thrombophilias$category, levels = c(0), 
                                   labels = c("Diagnosis of Thrombophilia"))

# Check correct coercion of variable classes
str(tidy_thrombophilias)
levels(tidy_thrombophilias$category)

# Find missing values
any(is.na(tidy_thrombophilias)) 
sum(is.na(tidy_thrombophilias)) 
summary(is.na(tidy_thrombophilias)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_thrombophilias <- tidy_thrombophilias %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_thrombophilias)

## valve_dx ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(valve_dx) == count(valve_dx)
distinct_valve_dx <- valve_dx %>% distinct()
n_distinct(distinct_valve_dx) == count(distinct(distinct_valve_dx, patid, eventdate, medcode, category,  .keep_all = TRUE))
distinct_valve_dx <- distinct_valve_dx %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_valve_dx) 

# Drop unneeded variables
tidy_valve_dx <- dplyr::select(distinct_valve_dx,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_valve_dx <-  tidy_valve_dx %>% 
  mutate_if(is.numeric, as.integer) 
tidy_valve_dx$eventdate <- as_date(tidy_valve_dx$eventdate)
tidy_valve_dx$category <- factor(tidy_valve_dx$category, levels = c(3), 
                                   labels = c("Diagnosed cardiac valve disorder"))

# Check correct coercion of variable classes
str(tidy_valve_dx)
levels(tidy_valve_dx$category)

# Find missing values
any(is.na(tidy_valve_dx)) 
sum(is.na(tidy_valve_dx)) 
summary(is.na(tidy_valve_dx)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_valve_dx <- tidy_valve_dx %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_valve_dx)

## congenhd ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(congenhd) == count(congenhd)
distinct_congenhd <- congenhd %>% distinct()
n_distinct(distinct_congenhd) == count(distinct(distinct_congenhd, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_congenhd <- distinct_congenhd %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_congenhd) 

# Drop unneeded variables
tidy_congenhd <- dplyr::select(distinct_congenhd,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_congenhd <-  tidy_congenhd %>% 
  mutate_if(is.numeric, as.integer) 
tidy_congenhd$eventdate <- as_date(tidy_congenhd$eventdate)
tidy_congenhd$category <- factor(tidy_congenhd$category, levels = c(1), 
                                   labels = c("Septum"))

# Check correct coercion of variable classes
str(tidy_congenhd)
levels(tidy_congenhd$category)

# Find missing values
any(is.na(tidy_congenhd))
sum(is.na(tidy_congenhd))
summary(is.na(tidy_congenhd)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_congenhd <- tidy_congenhd %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_congenhd) 

## cm_dilated ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(cm_dilated) == count(cm_dilated) 
distinct_cm_dilated <- cm_dilated %>% distinct()
n_distinct(distinct_cm_dilated) == count(distinct(distinct_cm_dilated, patid, eventdate, medcode, category,  .keep_all = TRUE))  
distinct_cm_dilated <- distinct_cm_dilated %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_cm_dilated) 

# Drop unneeded variables
tidy_cm_dilated <- dplyr::select(distinct_cm_dilated,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_cm_dilated <-  tidy_cm_dilated %>% 
  mutate_if(is.numeric, as.integer) 
tidy_cm_dilated$eventdate <- as_date(tidy_cm_dilated$eventdate)
tidy_cm_dilated$category <- factor(tidy_cm_dilated$category, levels = c(1:2), 
                                   labels = c("Primary dilated cardiomyopathy", "Secondary dilated cardiomyopathy"))

# Check correct coercion of variable classes
str(tidy_cm_dilated)
levels(tidy_cm_dilated$category)

# Find missing values
any(is.na(tidy_cm_dilated)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_cm_dilated) 

## hocm ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(hocm) == count(hocm) 
distinct_hocm <- hocm %>% distinct()
n_distinct(distinct_hocm) == count(distinct(distinct_hocm, patid, eventdate, medcode, category,  .keep_all = TRUE)) 
distinct_hocm <- distinct_hocm %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_hocm) 

# Drop unneeded variables
tidy_hocm <- dplyr::select(distinct_hocm,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                                   adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_hocm <-  tidy_hocm %>% 
  mutate_if(is.numeric, as.integer) 
tidy_hocm$eventdate <- as_date(tidy_hocm$eventdate)
tidy_hocm$category <- factor(tidy_hocm$category, levels = c(0), 
                                   labels = c("Hypertrophic cardiomyopathy"))

# Check correct coercion of variable classes
str(tidy_hocm)
levels(tidy_hocm$category)

# Find missing values
any(is.na(tidy_hocm)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_hocm) 

## af ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(af) == count(af) 
distinct_af <- af %>% distinct()
n_distinct(distinct_af) == count(distinct(distinct_af, patid, eventdate, medcode, af_cprd,  .keep_all = TRUE)) # true
distinct_af <- distinct_af %>% distinct(patid, eventdate, medcode,  af_cprd,  .keep_all = TRUE)
count(distinct_af) 

# Drop unneeded variables
tidy_af <- distinct_af

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_af <-  tidy_af %>% 
  mutate_if(is.numeric, as.integer) 
tidy_af$eventdate <- as_date(tidy_af$eventdate, -c(af_cprd))

# Check correct coercion of variable classes
str(tidy_af)

# Find missing values
any(is.na(tidy_af)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_af)

## migraine  ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(migraine) == count(migraine)
distinct_migraine <- migraine %>% distinct()
n_distinct(distinct_migraine) == count(distinct(distinct_migraine, patid, eventdate, medcode, category,  .keep_all = TRUE))  
distinct_migraine <- distinct_migraine %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_migraine) 

# Drop unneeded variables
tidy_migraine <- dplyr::select(distinct_migraine,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                       adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_migraine <-  tidy_migraine %>% 
  mutate_if(is.numeric, as.integer) 
tidy_migraine$eventdate <- as_date(tidy_migraine$eventdate)
tidy_migraine$category <- factor(tidy_migraine$category, levels = c(1:5), 
                             labels = c("History of migraine", "Diagnosis of migraine", 
                                        "History of migraine with aura", "Diagnosis of migraine with aura",
                                        "Contraception induced migraine"))

# Check correct coercion of variable classes
str(tidy_migraine)
levels(tidy_migraine$category)

# Find missing values
any(is.na(tidy_migraine)) 
sum(is.na(tidy_migraine)) 
summary(is.na(tidy_migraine)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_migraine <- tidy_migraine %>% filter(!is.na(eventdate)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_migraine)
count(tidy_migraine)

## bmi ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(bmi) == count(bmi)
distinct_bmi <- bmi %>% distinct()
n_distinct(distinct_bmi) == count(distinct(distinct_bmi, patid, eventdate, bmi,  .keep_all = TRUE)) 
distinct_bmi <- distinct_bmi %>% distinct(patid, eventdate, bmi,  .keep_all = TRUE)
count(distinct_bmi) 

# Drop unneeded variables
tidy_bmi <- distinct_bmi

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_bmi <-  tidy_bmi %>% 
  mutate_if(is.numeric, as.integer) 
tidy_bmi$bmi <- as.integer(tidy_bmi$bmi)

# Check correct coercion of variable classes
str(tidy_bmi)

# Find missing values
any(is.na(tidy_bmi))
sum(is.na(tidy_bmi))
summary(is.na(tidy_bmi)) 

# Drop records with missing event dates  (cannot know when hysterectomy is from therefore when to consider ineligible)
tidy_bmi <- tidy_bmi %>% filter(!is.na(bmi)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_bmi)

## bariatric_surgery ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(bariatric_surgery) == count(bariatric_surgery)
distinct_bariatric_surgery <- bariatric_surgery %>% distinct()
n_distinct(distinct_bariatric_surgery) == count(distinct(distinct_bariatric_surgery, patid, eventdate, medcode, bariatric_surgery,  .keep_all = TRUE)) 
distinct_bariatric_surgery <- distinct_bariatric_surgery %>% distinct(patid, eventdate, medcode,  bariatric_surgery,  .keep_all = TRUE)
count(distinct_bariatric_surgery)

# Drop unneeded variables
tidy_bariatric_surgery <- distinct_bariatric_surgery

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_bariatric_surgery <-  tidy_bariatric_surgery %>% 
  mutate_if(is.numeric, as.integer) 
tidy_bariatric_surgery$eventdate <- as_date(tidy_bariatric_surgery$eventdate)
tidy_bariatric_surgery$bariatric_surgery <- factor(tidy_bariatric_surgery$bariatric_surgery, levels = c(1:3), 
                             labels = c("History of bariatric surgery", "Possible bariatric surgery",
                                        "Bariatric surgery"))

# Check correct coercion of variable classes
str(tidy_bariatric_surgery)
levels(tidy_bariatric_surgery$bariatric_surgery)

# Find missing values
any(is.na(tidy_bariatric_surgery)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_bariatric_surgery)
count(tidy_bariatric_surgery) 

## brca ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(brca) == count(brca) # True
distinct_brca <- brca %>% distinct()
n_distinct(distinct_brca) == count(distinct(distinct_brca, patid, eventdate, medcode, category,  .keep_all = TRUE))  
distinct_brca <- distinct_brca %>% distinct(patid, eventdate, medcode,  category,  .keep_all = TRUE)
count(distinct_brca)

# Drop unneeded variables
tidy_brca <- dplyr::select(distinct_brca,  -c(sysdate,episode, enttype, staffid, constype, consid, medcode,
                                       adid, data1, data2, data3, data4, data5, data6 , data7)) 

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_brca <-  tidy_brca %>% 
  mutate_if(is.numeric, as.integer) 
tidy_brca$eventdate <- as_date(tidy_brca$eventdate)
tidy_brca$category <- factor(tidy_brca$category, levels = c(1:2), 
                             labels = c("BRCA 1", "BRCA 2"))

# Check correct coercion of variable classes
str(tidy_brca)
levels(tidy_brca$category)

# Find missing values
any(is.na(tidy_brca)) 

# Look at summary of all variables for outliers and obvious errors
summary(tidy_brca)

## viral_hep ---

# Check for and remove duplicates (whole row and by specific vars)
n_distinct(viral_hep) == count(viral_hep) 
distinct_viral_hep <- viral_hep %>% distinct()
n_distinct(distinct_viral_hep) == count(distinct(distinct_viral_hep, patid, eventdate, medcode, hepatitis_cprd,  .keep_all = TRUE))
distinct_viral_hep <- distinct_viral_hep %>% distinct(patid, eventdate, medcode,  hepatitis_cprd,  .keep_all = TRUE)
count(distinct_viral_hep) 

# Drop unneeded variables
tidy_viral_hep <- distinct_viral_hep

# Change variables to correct data type based on CPRD GOLD data specification & mapping
tidy_viral_hep <-  tidy_viral_hep %>% 
  mutate_if(is.numeric, as.integer) 
tidy_viral_hep$hepatitis_cprd <- factor(tidy_viral_hep$hepatitis_cprd, levels = c(3:7), 
                             labels = c("Hepatitis A", "Hepatitis B", "Hepatitis C", "Hepatitis D", "Hepatitis E"))

# Check correct coercion of variable classes
str(tidy_viral_hep)
levels(tidy_viral_hep$hepatitis_cprd)

# Find missing values
any(is.na(tidy_viral_hep))

# Look at summary of all variables for outliers and obvious errors
summary(tidy_viral_hep)

## select relevant variables -----------------------------------------------------------

contr_presc_clean <- dplyr::select(tidy_contr_presc, c(patid, eventdate,prodcode, qty, numdays, numpacks, issueseq, category))
contraception_clean <- tidy_contraception
pregnancy_clean <- tidy_pregnancy
hysterectomy_clean <- tidy_hysterectomy
sterilisation_clean <- tidy_sterilisation
cervical_ca_clean <- dplyr::select(tidy_cervical_ca, c(patid,eventdate))
endometrial_ca_clean <- dplyr::select(tidy_endometrial_ca, c(patid,eventdate))
gtd_clean <- dplyr::select(tidy_gtd, c(patid,eventdate,category))
pid_clean <- dplyr::select(tidy_pid, c(patid,eventdate))
chlamydia_clean <- dplyr::select(tidy_chlamydia, c(patid,eventdate))
gonorrhoea_clean <- dplyr::select(tidy_gonorrhoea, c(patid,eventdate))
pelvic_tb_clean <- dplyr::select(tidy_pelvic_tb, c(patid,eventdate))
breast_ca_clean <-  tidy_breast_ca
dcld_jaundice_clean <- tidy_decomp_liver_dx_jaundice
dcld_ascites_clean <- tidy_decomp_liver_dx_ascites
dcld_cirrhosis_clean <- tidy_decomp_liver_dx_cirrhosis
dcld_varices_clean <- tidy_decomp_liver_dx_varices
hep_adenoma_clean <- tidy_hep_adenoma
hep_ca_clean <- dplyr::select(tidy_hep_ca, c(patid, eventdate))
IHD_epicardial_clean <- dplyr::select(tidy_IHD_epicardial, c(patid, eventdate))
IHD_chd_clean <- tidy_IHD_chd
stroke_ich_clean  <- tidy_stroke_ich
stroke_isch_clean <- tidy_stroke_isch
stroke_nos_clean <- tidy_stroke_nos
stroke_TIA_clean <- tidy_stroke_TIA
smoking_clean <- tidy_smoking
obesity_clean <- tidy_obesity
diabetes_diag_clean <- tidy_diabetes_diag
dm_clean <- tidy_dm
hypertension_clean <- tidy_hypertension
vasculardx_clean <- tidy_vasculardx
vte_not_pe_clean <- tidy_vte_not_pe
pe_clean<- tidy_pe
vte_tx_clean <- tidy_vte_tx
thrombophilias_clean <- tidy_thrombophilias
valve_dx_clean <- tidy_valve_dx
congenhd_clean <- dplyr::select(tidy_congenhd, c(patid, eventdate))
cm_dilated_clean <- tidy_cm_dilated
hocm_clean <- dplyr::select(tidy_hocm, c(patid, eventdate))
af_clean <- dplyr::select(tidy_af, c(patid, eventdate))
migraine_clean <- tidy_migraine
bmi_clean <- tidy_bmi
bariatric_surgery_clean <- tidy_bariatric_surgery
brca_clean <- tidy_brca
viral_hep_clean <- tidy_viral_hep

## Recoding/renaming variables --------------------------------------------------------

## rename category to avoid confusion between files
contr_presc_clean <- rename(contr_presc_clean, contrpresccat = category)
contraception_clean <- rename(contraception_clean, contrcat = category)
pregnancy_clean <- rename(pregnancy_clean, pregcat = pregnancy)
hysterectomy_clean <- rename(hysterectomy_clean, hyscat = category)
sterilisation_clean <- rename(sterilisation_clean, stericat = category)
gtd_clean <- rename(gtd_clean, gtdcat = category)
breast_ca_clean <- rename(breast_ca_clean, breastcacat = category)
dcld_jaundice_clean <- rename(dcld_jaundice_clean, jaundicecat = category)
dcld_ascites_clean <- rename(dcld_ascites_clean, ascitescat = category)
dcld_cirrhosis_clean <- rename(dcld_cirrhosis_clean, cirrhosiscat = category)
dcld_varices_clean <- rename(dcld_varices_clean, varicescat = category)
hep_adenoma_clean <- rename(hep_adenoma_clean, hep_adenomacat = category)
IHD_chd_clean <- rename(IHD_chd_clean, chdnoscat = chd_nos_cprd)
stroke_ich_clean  <- rename(stroke_ich_clean, strokeichcat = category)
stroke_isch_clean <- rename(stroke_isch_clean, strokeischcat = category)
stroke_nos_clean <- rename(stroke_nos_clean, strokenoscat = category)
stroke_TIA_clean <- rename(stroke_TIA_clean, stroketiacat = category)
smoking_clean <- rename(smoking_clean, smokingcat = category)
obesity_clean <- rename(obesity_clean, obesitycat = obesity_cat)
diabetes_diag_clean <- rename(diabetes_diag_clean, diabdiagcat = diabdiag_cprd)
dm_clean <- rename(dm_clean, dmcat = dm_cprd)
hypertension_clean <- rename(hypertension_clean, htncat = bp_cprd)
vasculardx_clean <- rename(vasculardx_clean, vascdxcat = category)
vte_not_pe_clean <-  rename(vte_not_pe_clean, vtecat = category)
pe_clean <- rename(pe_clean, pecat = category)
vte_tx_clean <- rename(vte_tx_clean, vtetxcat = anticoagulants_and_protamine_cprdprod)
thrombophilias_clean <- rename(thrombophilias_clean, thrombophiliascat = category)
valve_dx_clean <- rename(valve_dx_clean, valvedxcat = category)
cm_dilated_clean <- rename(cm_dilated_clean, cmdilatedcat = category)
migraine_clean <- rename(migraine_clean, migrainecat = category)
bariatric_surgery_clean <- rename(bariatric_surgery_clean, bariatriccat = bariatric_surgery)
brca_clean <- rename(brca_clean, brcacat = category)
viral_hep_clean <- rename(viral_hep_clean, viralhepcat = hepatitis_cprd)

## Save & load final cleaned  .Rdata file --------------------------------

save(contr_presc_clean, file = "filepath")
save(contraception_clean, file = "filepath")
save(pregnancy_clean, file = "filepath")
save(hysterectomy_clean, file = "filepath") 
save(sterilisation_clean, file = "filepath") 
save(cervical_ca_clean, file = "filepath") 
save(endometrial_ca_clean, file = "filepath") 
save(gtd_clean, file = "filepath")
save(pid_clean, file = "filepath")
save(chlamydia_clean, file = "filepath")
save(gonorrhoea_clean, file = "filepath")
save(pelvic_tb_clean, file = "filepath")
save(breast_ca_clean, file = "filepath")
save(dcld_jaundice_clean, file = "filepath")
save(dcld_ascites_clean, file = "filepath" ) 
save(dcld_cirrhosis_clean, file = "filepath" )
save(dcld_varices_clean, file = "filepath")
save(hep_adenoma_clean, file = "filepath")
save(hep_ca_clean, file = "filepath")
save(IHD_epicardial_clean, file = "filepath")
save(IHD_chd_clean, file = "filepath") 
save(stroke_ich_clean, file = "filepath") 
save(stroke_isch_clean, file = "filepath") 
save(stroke_nos_clean, file = "filepath") 
save(stroke_TIA_clean, file = "filepath") 
save(smoking_clean, file = "filepath") 
save(obesity_clean, file = "filepath") 
save(diabetes_diag_clean, file = "filepath") 
save(dm_clean,  file = "filepath") 
save(hypertension_clean,  file = "filepath") 
save(vasculardx_clean, file = "filepath") 
save(vte_not_pe_clean,  file = "filepath") 
save(pe_clean,  file = "filepath") 
save(vte_tx_clean, file = "filepath") 
save(thrombophilias_clean,  file = "filepath")  
save(valve_dx_clean,   file = "filepath")  
save(congenhd_clean,   file = "filepath") 
save(cm_dilated_clean, file = "filepath")  
save(hocm_clean, file = "filepath") 
save(af_clean, file = "filepath") 
save(migraine_clean,  file = "filepath")  
save(bmi_clean,  file = "filepath")  
save(bariatric_surgery_clean, file = "filepath") 
save(brca_clean, file = "filepath") 
save(viral_hep_clean, file = "filepath") 

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
load(file = "filepath" ) 
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
load(file = "filepath") 
load(file = "filepath") 


## Overall cohort creation ---------------------------------------------

str(reproductive_cohort_final)

##  cohort dates - ensure not entering cohort if had hysterectomy/female sterilisation before cohort entry date ---

# incident hysterectomy events
count(hysterectomy_clean) == count(distinct(hysterectomy_clean, patid))
incidentevents_hysterectomy <- hysterectomy_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hysterectomy <- as.data.frame(incidentevents_hysterectomy)
count(incidentevents_hysterectomy) == count(distinct(hysterectomy_clean, patid)) 

# incident female sterilisation event
femalesteri <- sterilisation_clean %>% filter(stericat == "Female sterilisation")
count(femalesteri) == count(distinct(femalesteri,patid)) 
incidentevents_femalesteri <- femalesteri %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_femalesteri  <- as.data.frame(incidentevents_femalesteri)
count(incidentevents_femalesteri) == count(distinct(femalesteri, patid)) 

# Combine patients with an incident hysterectomy date or female steri date and dplyr::select earliest date of these
hys_steri <- full_join(incidentevents_hysterectomy, incidentevents_femalesteri, 
                       by = c("patid" = "patid", "eventdate" = "eventdate"))
incident_hys_steri <- hys_steri %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_hys_steri <- as.data.frame(incident_hys_steri)
count(incident_hys_steri) == count(distinct(incident_hys_steri))
incident_hys_steri <- rename(incident_hys_steri, hys_steri_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
contraception_cohort <- left_join(reproductive_cohort_final, incident_hys_steri, 
                               by = c("patid" = "patid"))
contraception_cohort_entry <-  contraception_cohort %>% 
  mutate(contr_cohort_entry = ifelse(is.na(hys_steri_eventdate), cohort_entry,
                                     ifelse(hys_steri_eventdate <= cohort_entry, NA, cohort_entry)))
summary(is.na(contraception_cohort_entry$contr_cohort_entry)) 
contraception_cohort_entry_eligible <- contraception_cohort_entry %>% filter(!is.na(contr_cohort_entry))

# Amend cohort exit dates
contraception_cohort_entry_exit <- contraception_cohort_entry_eligible %>% 
  mutate(contr_cohort_exit = ifelse(is.na(hys_steri_eventdate), cohort_exit,
                                   ifelse(hys_steri_eventdate < cohort_exit, hys_steri_eventdate, cohort_exit)))
summary(is.na(contraception_cohort_entry_exit$contr_cohort_exit)) 

# Recalculate pdays and pyears 
contraception_cohort_entry_exit_pdays_pyears <- contraception_cohort_entry_exit %>%
  mutate(contr_pdays = (contr_cohort_exit-contr_cohort_entry)+1)  %>%
  mutate(contr_pyears = contr_pdays/365.25)
contraception_cohort_entry_exit_pdays_pyears$pyears <- as.numeric(contraception_cohort_entry_exit_pdays_pyears$pyears)
contraception_cohort_entry_exit_pdays_pyears$pdays <- as.numeric(contraception_cohort_entry_exit_pdays_pyears$pdays)

# All impacted by pregnancy intervals but different posnatal periods for each contraception so calculated in section separately 

# NB: still eligible for all types of contraception when using another type so person time at risk does not change in relation to this

## Save & load final contraception cohort .Rdata file --------------------------------

save(contraception_cohort_entry_exit_pdays_pyears, file = "filepath") 
load(file = "filepath") 


## Contr presc episodes -----------------

load(file = "filepath") 
load(file = "filepath")

contraception_cohort_final <- contraception_cohort_entry_exit_pdays_pyears
contraception_cohort_final$contr_cohort_entry <- as_date(contraception_cohort_final$contr_cohort_entry)
contraception_cohort_final$contr_cohort_exit <- as_date(contraception_cohort_final$contr_cohort_exit)
contr_presc <- contr_presc_clean

## Change prescribing events that are same category on same day, to one event ---

n_distinct(contr_presc) == count(distinct(contr_presc, patid, eventdate, contrpresccat,  .keep_all = TRUE)) 
contr_presc_oneeventperdate <- contr_presc %>% distinct(patid, eventdate, contrpresccat,  .keep_all = TRUE)
count(contr_presc_oneeventperdate)

## Drop events that are not in contr cohort entry/exit time ---

contr_presc_indates <- left_join(contr_presc_oneeventperdate, contraception_cohort_final,
                                by = c("patid" = "patid"))
contr_presc_indates_2 <- contr_presc_indates %>% 
  filter(eventdate >= contr_cohort_entry & eventdate <= contr_cohort_exit) 
count(contr_presc_indates_2)

## Create annual contr presc counts ---


# create contr cohort file with line for each year that patient is in cohort 
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(contraception_cohort_final)))
colnames(year_variable) <- 'eventyear'
contr_annual_cohort <- contraception_cohort_final %>%
  slice(rep(1:n(), each=10))
contr_annual_cohort$eventyear <- year_variable$eventyear
contr_annual_cohort <- contr_annual_cohort %>% filter(eventyear >= year(contr_cohort_entry) & eventyear <= year(contr_cohort_exit))

# Create annual contr presc episodes count 

contrpresc_annual_counts <- contr_presc_indates_2 %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear,contrpresccat) %>%
  add_count(patid) %>%
  rename(contrpresc_n = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, contrpresc_n, contrpresccat) %>%
  distinct()


# Join annual cohort file to annual contr presc files, change NAs to 0

contrpresc_annual_counts <- contr_annual_cohort %>%
  left_join(contrpresc_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear"))
contrpresc_annual_counts$contrpresc_n <- contrpresc_annual_counts$contrpresc_n %>% replace_na(0)


# Add age during event year and age category 

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

contrpresc_annual_counts_age <- contrpresc_annual_counts %>% mutate(age_date= eventyear) 
contrpresc_annual_counts_age$age_date <- as.character(contrpresc_annual_counts_age$age_date)
contrpresc_annual_counts_age$age_date <- paste("01-01-", contrpresc_annual_counts_age$age_date, sep="")
head(contrpresc_annual_counts_age$age_date)
contrpresc_annual_counts_age$age_date <- dmy(contrpresc_annual_counts_age$age_date)
head(contrpresc_annual_counts_age$age_date)
class(contrpresc_annual_counts_age$age_date)
contrpresc_annual_counts_age <- contrpresc_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(contrpresc_annual_counts_age$eventyear_age)
max(contrpresc_annual_counts_age$eventyear_age) 
contrpresc_annual_counts_age_50 <- contrpresc_annual_counts_age %>% filter(eventyear_age == 50)
contrpresc_annual_counts_age_final <- contrpresc_annual_counts_age %>% 
  filter(eventyear_age != 50) %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
contrpresc_annual_counts_age_final$eventyear_agecat <- factor(contrpresc_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                              labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Final file
contrpresc_annual_counts_final <- contrpresc_annual_counts_age_final %>% dplyr::select(-c(age_date))

## Save and load final file
save(contrpresc_annual_counts_final, file = "filepath")
load(file = "filepath")



## Set up data for stratifying by time variant variable (i.e add pdays per year preg max and pregmin, duration of preg/pic/ct/gc removed per year ---

# Add year durations
contrpresc_annual_counts_final_extra <- contrpresc_annual_counts_final %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
contrpresc_annual_counts_final_extra$start_eventyear <- as_date(contrpresc_annual_counts_final_extra$start_eventyear)
contrpresc_annual_counts_final_extra$end_eventyear <- as_date(contrpresc_annual_counts_final_extra$end_eventyear)
contrpresc_annual_counts_final_extra <- contrpresc_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(contr_cohort_entry <= start_eventyear, start_eventyear,contr_cohort_entry))
contrpresc_annual_counts_final_extra$t0_ey <- as_date(contrpresc_annual_counts_final_extra$t0_ey)
contrpresc_annual_counts_final_extra <- contrpresc_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(contr_cohort_exit >= end_eventyear, end_eventyear,contr_cohort_exit))
contrpresc_annual_counts_final_extra$t1_ey <- as_date(contrpresc_annual_counts_final_extra$t1_ey)
contrpresc_annual_counts_final_extra <- contrpresc_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) 
contrpresc_annual_counts_final_extra$pdays_ey <- as.numeric(contrpresc_annual_counts_final_extra$pdays_ey)
contrpresc_annual_counts_final_extra <- contrpresc_annual_counts_final_extra %>% mutate(pyears_ey=pdays_ey/365.25)

## Save & load annual counts final extra (for glms) ---

contrpresc_annual_counts_final_extra <- contrpresc_annual_counts_final_extra %>% dplyr::select(patid, eventyear, pracid, prac_region, dob, yob, patimd, pracimd, imd, 
                                                                                              migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                              data_start, data_end, contr_cohort_entry, contr_cohort_exit, contr_pdays, 
                                                                                              contr_pyears, eventyear_age, eventyear_agecat, 
                                                                                              contrpresc_n, contrpresc_n, pdays_ey, 
                                                                                              pyears_ey)



save(contrpresc_annual_counts_final_extra, file = "filepath")
load(file = "filepath")



## Cu IUD cohort creation --------------------------------------------------------

## Adjust for conditions that affect cohort entry/exit ---

# dplyr::select incident cervical_ca events
count(cervical_ca_clean) == count(distinct(cervical_ca_clean, patid)) 
incidentevents_cervical_ca <- cervical_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_cervical_ca <- as.data.frame(incidentevents_cervical_ca)
count(incidentevents_cervical_ca) == count(distinct(cervical_ca_clean, patid))

# dplyr::select incident endometrial_ca events
count(endometrial_ca_clean) == count(distinct(endometrial_ca_clean, patid)) 
incidentevents_endometrial_ca <- endometrial_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_endometrial_ca <- as.data.frame(incidentevents_endometrial_ca)
count(incidentevents_endometrial_ca) == count(distinct(endometrial_ca_clean, patid))

# dplyr::select incident malignant gtd events 
malignant_gtd <- gtd_clean %>% filter(gtdcat == "Definite")
count(malignant_gtd) == count(distinct(malignant_gtd, patid)) 
incidentevents_malignant_gtd <- malignant_gtd %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_malignant_gtd <- as.data.frame(incidentevents_malignant_gtd)
count(incidentevents_malignant_gtd) == count(distinct(malignant_gtd, patid)) 

# dplyr::select incident pelvic_tb events
count(pelvic_tb_clean) == count(distinct(pelvic_tb_clean, patid)) 
incidentevents_pelvic_tb <- pelvic_tb_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_pelvic_tb <- as.data.frame(incidentevents_pelvic_tb)
count(incidentevents_pelvic_tb) == count(distinct(pelvic_tb_clean, patid)) 

# Combine patients with an incident cervical ca/endometrial ca/malign gtd/pelvic tb date and dplyr::select earliest date of these
cerv_endom_gtd_pelvictb <- full_join(incidentevents_cervical_ca, incidentevents_endometrial_ca, 
                       by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_malignant_gtd, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_pelvic_tb, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_cerv_endom_gtd_pelvictb <- cerv_endom_gtd_pelvictb %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_cerv_endom_gtd_pelvictb <- as.data.frame(incident_cerv_endom_gtd_pelvictb)
count(incident_cerv_endom_gtd_pelvictb) == count(distinct(incident_cerv_endom_gtd_pelvictb))
incident_cerv_endom_gtd_pelvictb <- rename(incident_cerv_endom_gtd_pelvictb, cerv_endom_gtd_pelvictb_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
cuiud_cohort <- left_join(contraception_cohort_entry_exit_pdays_pyears, incident_cerv_endom_gtd_pelvictb, 
                                  by = c("patid" = "patid"))
cuiud_cohort_entry <-  cuiud_cohort %>% 
  mutate(cuiud_cohort_entry = ifelse(is.na(cerv_endom_gtd_pelvictb_eventdate), contr_cohort_entry,
                                     ifelse(cerv_endom_gtd_pelvictb_eventdate <= contr_cohort_entry, NA, contr_cohort_entry)))
summary(is.na(cuiud_cohort_entry$cuiud_cohort_entry)) 
cuiud_cohort_entry_eligible <- cuiud_cohort_entry %>% filter(!is.na(cuiud_cohort_entry))

# Amend cohort exit dates
cuiud_cohort_entry_exit <- cuiud_cohort_entry_eligible %>% 
  mutate(cuiud_cohort_exit = ifelse(is.na(cerv_endom_gtd_pelvictb_eventdate), contr_cohort_exit,
                                    ifelse(cerv_endom_gtd_pelvictb_eventdate < contr_cohort_exit, cerv_endom_gtd_pelvictb_eventdate, contr_cohort_exit)))
summary(is.na(cuiud_cohort_entry_exit$contr_cohort_exit)) 

# Recalculate pdays and pyears based on those eligible after adjusting for cervical.ca.endomca/malign gtd
cuiud_cohort_entry_exit_pdays <- cuiud_cohort_entry_exit %>%
  mutate(cuiud_pdays = (cuiud_cohort_exit-cuiud_cohort_entry)+1) 
cuiud_cohort_entry_exit_pdays$pdays <- as.numeric(cuiud_cohort_entry_exit_pdays$pdays)

# convert to date format
cuiud_cohort_entry_exit_pdays$cuiud_cohort_entry <-  as_date(cuiud_cohort_entry_exit_pdays$cuiud_cohort_entry) 
cuiud_cohort_entry_exit_pdays$cuiud_cohort_exit <-  as_date(cuiud_cohort_entry_exit_pdays$cuiud_cohort_exit) 



## Adjust for pregnancy and postnatal intervals based on pregnancy outcomes (max and min pdays and pyears) ---

# dplyr::select pregnancy outcomes only (Drop currently pregnant, postnatal and vague timing)

pregnancy_outcomes <- pregnancy_clean %>% filter(pregcat != "Currently pregnant" & pregcat != "Pregnancy of childbirth-related terms with vague timing"
                                                 & pregcat != "Puerperium or neonatal(within 6 weeks after delivery)"
                                                 & pregcat != "Postnatal (after delivery)")

# create episode_start (max and min) and episode_end (max and min) dates for pregnancy events

pregnancy_max <- pregnancy_outcomes %>% 
  mutate(episode_start_max = ifelse(pregcat == "Abnormal product of conception", (eventdate-84),
                                    ifelse(pregcat == "Spontaneous abortion", (eventdate-168),
                                           ifelse(pregcat == "Termination of pregnancy", (eventdate-168),
                                                  ifelse(pregcat == "Unspecified abortion", (eventdate-168),
                                                         ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate-301),
                                                                ifelse(pregcat == "Ectopic pregnancy", (eventdate-84),
                                                                       ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate-301), NA))))))))

pregnancy_max <- pregnancy_max  %>% 
  mutate(episode_end_max = ifelse(pregcat == "Abnormal product of conception", (eventdate),
                                  ifelse(pregcat == "Spontaneous abortion", (eventdate),
                                         ifelse(pregcat == "Termination of pregnancy", (eventdate),
                                                ifelse(pregcat == "Unspecified abortion", (eventdate),
                                                       ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate),
                                                              ifelse(pregcat == "Ectopic pregnancy", (eventdate),
                                                                     ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate), NA))))))))

pregnancy_max_min <- pregnancy_max %>% 
  mutate(episode_start_min = ifelse(pregcat == "Abnormal product of conception", (eventdate-42),
                                    ifelse(pregcat == "Spontaneous abortion", (eventdate-28),
                                           ifelse(pregcat == "Termination of pregnancy", (eventdate-42),
                                                  ifelse(pregcat == "Unspecified abortion", (eventdate-28),
                                                         ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate-161),
                                                                ifelse(pregcat == "Ectopic pregnancy", (eventdate-42),
                                                                       ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate-100), NA))))))))


pregnancy_max_min <- pregnancy_max_min  %>% 
  mutate(episode_end_min = ifelse(pregcat == "Abnormal product of conception", (eventdate),
                                  ifelse(pregcat == "Spontaneous abortion", (eventdate),
                                         ifelse(pregcat == "Termination of pregnancy", (eventdate),
                                                ifelse(pregcat == "Unspecified abortion", (eventdate),
                                                       ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate),
                                                              ifelse(pregcat == "Ectopic pregnancy", (eventdate),
                                                                     ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate), NA))))))))

# convert to date format
pregnancy_max_min  <-  pregnancy_max_min  %>%
  mutate_if(is.numeric, as_date)
pregnancy_max_min$patid  <-  as.integer(pregnancy_max_min$patid)  

# Filter for labour/del/condition at birth & stillbirth/iud/perinatal i.e. delivery after 24 weeks

preg_24plus <- pregnancy_max_min %>% filter(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death" )
preg_pre24 <- pregnancy_max_min %>% filter(pregcat == "Abnormal product of conception" | pregcat == "Spontaneous abortion" |
                                             pregcat == "Termination of pregnancy" | pregcat == "Unspecified abortion" |pregcat == "Ectopic pregnancy")


# Drop events within 175 days of preg_24plus events (21 d until earliest possible ovulation + 22 weeks from then until earliest possibledelivery after 24 weeks)
preg_24plus_lag <- preg_24plus %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
preg_24plus_episodes <- preg_24plus_lag %>% filter(eventdate > (previous+175) | is.na(previous))

# Drop events within 19 days of preg_24plus events (5 d until earliest possible ovulation + 2 weeks until earliest possible positive pregnanct test and subsequent pre 24 week outcome) 
preg_pre24_lag <- preg_pre24 %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
preg_pre24_episodes <- preg_pre24_lag %>% filter(eventdate > (previous+19) | is.na(previous)) 

# add exclusion period end date (i.e. date after event when other outcome events cannot occur) + join dataframes
preg_24plus_episodes_excl_end <- preg_24plus_episodes %>% mutate(excl_end = (eventdate+175)) 
preg_pre24_episodes_excl_end <- preg_pre24_episodes %>% mutate(excl_end = (eventdate+19)) 
preg_excl_end <- full_join(preg_24plus_episodes_excl_end, preg_pre24_episodes_excl_end, 
                           by = c("patid" = "patid", "pregcat" = "pregcat", "eventdate" = "eventdate", 
                                  "episode_start_max" = "episode_start_max","episode_end_max" = "episode_end_max",
                                  "episode_start_min" = "episode_start_min", "episode_end_min" = "episode_end_min",
                                  "previous" = "previous", "excl_end" = "excl_end"))
 
# add previous events exclusion end date 
preg_excl_end_prev <- preg_excl_end %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous_excl_end = lag(excl_end))

# drop episodes that fall into prev events exclusion period
preg_episodes <- preg_excl_end_prev %>% filter(eventdate > previous_excl_end | is.na(previous_excl_end))

# extend preg episode to include postnatal 4 weeks for 24 week plus pregnancy outcomes)
preg_episodes_pn4w <- preg_episodes %>% 
  mutate(episode_end_max = ifelse(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death", 
                                  episode_end_max+28, episode_end_max)) %>%
  mutate(episode_end_min = ifelse(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death", 
                                  episode_end_min+28, episode_end_min)) 
preg_episodes_pn4w  <-  preg_episodes_pn4w  %>%
  mutate_if(is.numeric, as_date)
preg_episodes_pn4w$patid  <-  as.integer(preg_episodes_pn4w$patid)  

# drop episodes that are not in cohort entry/exit period for patient 
preg_episodes_pn4w_cohort <- left_join(preg_episodes_pn4w, cuiud_cohort_entry_exit_pdays, by = c("patid" = "patid"))
preg_episodes_pn4w_valid <-preg_episodes_pn4w_cohort %>% filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit) %>% 
  dplyr::select(c(patid, eventdate, pregcat, episode_start_max, episode_end_max, episode_start_min, episode_end_min))

# calculate episode durations 
preg_episodes_pn4w_duration_max <- preg_episodes_pn4w_valid  %>% mutate(duration_max =(episode_end_max-episode_start_max)+1) 
preg_episodes_pn4w_duration_max_min <- preg_episodes_pn4w_duration_max %>%  mutate(duration_min = (episode_end_min-episode_start_min)+1)
preg_episodes_pn4w_total_max <- preg_episodes_pn4w_duration_max_min %>% group_by(patid) %>% tally(duration_max)
preg_episodes_pn4w_total_min <- preg_episodes_pn4w_duration_max_min %>% group_by(patid) %>% tally(duration_min)
preg_episodes_pn4w_total_max <- rename(preg_episodes_pn4w_total_max, preg_total_max = n)
preg_episodes_pn4w_total_min <- rename(preg_episodes_pn4w_total_min, preg_total_min = n)

# recalculate pdays (max and min based on max and min preg durations)
cuiud_cohort_pregnancy_pn4w_episodes <- left_join(cuiud_cohort_entry_exit_pdays, preg_episodes_pn4w_total_max,
                                             by = c("patid" = "patid")) %>%
  left_join(preg_episodes_pn4w_total_min, by = c("patid" = "patid"))
cuiud_cohort_pregnancy_pn4w_episodes$preg_total_max <- cuiud_cohort_pregnancy_pn4w_episodes$preg_total_max %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes$preg_total_min <- cuiud_cohort_pregnancy_pn4w_episodes$preg_total_min %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays <- cuiud_cohort_pregnancy_pn4w_episodes %>% 
  mutate(cuiud_pdays_max = (cuiud_pdays-preg_total_max)) %>%
  mutate(cuiud_pdays_min = (cuiud_pdays-preg_total_min)) 
cuiud_cohort_pregnancy_pn4w_episodes_pdays$cuiud_pdays_max <- as.numeric(cuiud_cohort_pregnancy_pn4w_episodes_pdays$cuiud_pdays_max)
cuiud_cohort_pregnancy_pn4w_episodes_pdays$cuiud_pdays_min <- as.numeric(cuiud_cohort_pregnancy_pn4w_episodes_pdays$cuiud_pdays_min)


## Adjust for conditions that have short intervals that affect pdays/pyears ---

# dplyr::select incident events for pid (drop codes that occur within 2 weeks of another code), CT (drop codes that occur within 1 weeks of another code) , & GC (drop codes that occur within 1 weeks of another code) 
pid_lag <- pid_clean %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate)) 
pid_episodes <- pid_lag %>% 
  filter(eventdate > (previous+14) | is.na(previous))
chlamydia_lag <- chlamydia_clean %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
chlamydia_episodes <- chlamydia_lag %>% filter(eventdate > (previous+7) | is.na(previous))
gonorrhoea_lag <- gonorrhoea_clean  %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
gonorrhoea_episodes <- gonorrhoea_lag %>% filter(eventdate > (previous+7) | is.na(previous))


# Drop episodes that occur outside of cohort entry/exit 
pid_episodes_cuiudcohort<- left_join(pid_episodes, cuiud_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
pidepisodes_cuiudvalid <- pid_episodes_cuiudcohort %>% filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit)
chlamydia_episodes_cuiudcohort<- left_join(chlamydia_episodes, cuiud_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
chlamydiaepisodes_cuiudvalid <- chlamydia_episodes_cuiudcohort %>% filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit)
gonorrhoea_episodes_cuiudcohort <- left_join(gonorrhoea_episodes, cuiud_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
gonorrhoeaepisodes_cuiudvalid <- gonorrhoea_episodes_cuiudcohort %>% filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit)


# create wide data of pregnancy max start date
preg_max <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_start_max))
preg_max_long <- preg_max %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_max_long)
preg_max_distinct_patid <- preg_max_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_max_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_max_distinct_patid_2 <- preg_max_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_max_distinct_patid_2$eventid <- eventid_variable$eventid
preg_max_long_expanded <-  left_join(preg_max_distinct_patid_2, preg_max_long, 
                                by = c("patid" = "patid", "eventid" = "id"))
preg_max_wide_start <- spread(preg_max_long_expanded , eventid, episode_start_max)
preg_max_wide_start <- preg_max_wide_start %>% rename_with( ~ paste("preg_max_start", .x, sep = "_"))

# create wide data of pregnancy max end date
preg_max <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_end_max))
preg_max_long <- preg_max %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_max_long)
preg_max_distinct_patid <- preg_max_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_max_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_max_distinct_patid_2 <- preg_max_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_max_distinct_patid_2$eventid <- eventid_variable$eventid
preg_max_long_expanded <-  left_join(preg_max_distinct_patid_2, preg_max_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_max_wide_end <- spread(preg_max_long_expanded , eventid, episode_end_max)
preg_max_wide_end <- preg_max_wide_end %>% rename_with( ~ paste("preg_max_end", .x, sep = "_"))

# Join preg max wide datasets
preg_max_wide <- full_join(preg_max_wide_start, preg_max_wide_end, by = c("preg_max_start_patid" = "preg_max_end_patid"))

# Drop pid events that fall into preg max
pid_preg_max <- left_join(pidepisodes_cuiudvalid, preg_max_wide, 
                            by = c("patid" = "preg_max_start_patid"))
pid_notinpregmax <- pid_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
pid_notinpregmax <- dplyr::select(pid_notinpregmax, c(patid, eventdate))
count(pid_notinpregmax)

# Drop chlamydia events that fall into preg max
chlamydia_preg_max <- left_join(chlamydiaepisodes_cuiudvalid, preg_max_wide, 
                          by = c("patid" = "preg_max_start_patid"))
chlamydia_notinpregmax <- chlamydia_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
chlamydia_notinpregmax <- dplyr::select(chlamydia_notinpregmax, c(patid, eventdate))
count(chlamydia_notinpregmax)

# Drop gonorrhoea events that fall into preg max
gonorrhoea_preg_max <- left_join(gonorrhoeaepisodes_cuiudvalid, preg_max_wide, 
                          by = c("patid" = "preg_max_start_patid"))
gonorrhoea_notinpregmax <- gonorrhoea_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
gonorrhoea_notinpregmax <- dplyr::select(gonorrhoea_notinpregmax, c(patid, eventdate))
count(gonorrhoea_notinpregmax)


# create wide data of pregnancy min start date
preg_min <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_start_min))
preg_min_long <- preg_min %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_min_long)
preg_min_distinct_patid <- preg_min_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_min_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_min_distinct_patid_2 <- preg_min_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_min_distinct_patid_2$eventid <- eventid_variable$eventid
preg_min_long_expanded <-  left_join(preg_min_distinct_patid_2, preg_min_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_min_wide_start <- spread(preg_min_long_expanded , eventid, episode_start_min)
preg_min_wide_start <- preg_min_wide_start %>% rename_with( ~ paste("preg_min_start", .x, sep = "_"))

# create wide data of pregnancy min end date
preg_min <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_end_min))
preg_min_long <- preg_min %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_min_long)
preg_min_distinct_patid <- preg_min_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_min_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_min_distinct_patid_2 <- preg_min_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_min_distinct_patid_2$eventid <- eventid_variable$eventid
preg_min_long_expanded <-  left_join(preg_min_distinct_patid_2, preg_min_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_min_wide_end <- spread(preg_min_long_expanded , eventid, episode_end_min)
preg_min_wide_end <- preg_min_wide_end %>% rename_with( ~ paste("preg_min_end", .x, sep = "_"))

# Join preg min wide datasets
preg_min_wide <- full_join(preg_min_wide_start, preg_min_wide_end, by = c("preg_min_start_patid" = "preg_min_end_patid"))

# Drop pid events that fall into preg min
pid_preg_min <- left_join(pidepisodes_cuiudvalid, preg_min_wide, 
                          by = c("patid" = "preg_min_start_patid"))
pid_notinpregmin <- pid_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
pid_notinpregmin <- dplyr::select(pid_notinpregmin, c(patid, eventdate))
count(pid_notinpregmin) 

# Drop chlamydia events that fall into preg min
chlamydia_preg_min <- left_join(chlamydiaepisodes_cuiudvalid, preg_min_wide, 
                                by = c("patid" = "preg_min_start_patid"))
chlamydia_notinpregmin <- chlamydia_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
chlamydia_notinpregmin <- dplyr::select(chlamydia_notinpregmin, c(patid, eventdate))
count(chlamydia_notinpregmin) 

# Drop gonorrhoea events that fall into preg min
gonorrhoea_preg_min <- left_join(gonorrhoeaepisodes_cuiudvalid, preg_min_wide, 
                                 by = c("patid" = "preg_min_start_patid"))
gonorrhoea_notinpregmin <- gonorrhoea_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
gonorrhoea_notinpregmin <- dplyr::select(gonorrhoea_notinpregmin, c(patid, eventdate))
count(gonorrhoea_notinpregmin) 



# add durations for infection intervals (pid, chlamydia, gonorrhoea) 
pid_episodes_preg_max_duration <- pid_notinpregmax %>% mutate(duration = 14) 
pid_episodes_preg_max_total <-pid_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
pid_episodes_preg_max_total <- rename(pid_episodes_preg_max_total, pid_preg_max_total = n)
pid_episodes_preg_min_duration <- pid_notinpregmin %>% mutate(duration = 14) 
pid_episodes_preg_min_total <-pid_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
pid_episodes_preg_min_total <- rename(pid_episodes_preg_min_total, pid_preg_min_total = n)

chlamydia_episodes_preg_max_duration <- chlamydia_notinpregmax %>% mutate(duration = 14) 
chlamydia_episodes_preg_max_total <-chlamydia_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
chlamydia_episodes_preg_max_total <- rename(chlamydia_episodes_preg_max_total, chlamydia_preg_max_total = n)
chlamydia_episodes_preg_min_duration <- chlamydia_notinpregmin %>% mutate(duration = 14) 
chlamydia_episodes_preg_min_total <-chlamydia_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
chlamydia_episodes_preg_min_total <- rename(chlamydia_episodes_preg_min_total, chlamydia_preg_min_total = n)

gonorrhoea_episodes_preg_max_duration <- gonorrhoea_notinpregmax %>% mutate(duration = 14) 
gonorrhoea_episodes_preg_max_total <-gonorrhoea_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
gonorrhoea_episodes_preg_max_total <- rename(gonorrhoea_episodes_preg_max_total, gonorrhoea_preg_max_total = n)
gonorrhoea_episodes_preg_min_duration <- gonorrhoea_notinpregmin %>% mutate(duration = 14) 
gonorrhoea_episodes_preg_min_total <-gonorrhoea_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
gonorrhoea_episodes_preg_min_total <- rename(gonorrhoea_episodes_preg_min_total, gonorrhoea_preg_min_total = n)

# recalculate pdays 
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc <- left_join(cuiud_cohort_pregnancy_pn4w_episodes_pdays, pid_episodes_preg_max_total,
                                                            by = c("patid" = "patid")) %>%
  left_join(pid_episodes_preg_min_total, by = c("patid" = "patid"))  %>%
  left_join(chlamydia_episodes_preg_max_total, by = c("patid" = "patid"))  %>%
  left_join(chlamydia_episodes_preg_min_total, by = c("patid" = "patid"))  %>%
  left_join(gonorrhoea_episodes_preg_max_total, by = c("patid" = "patid"))  %>%
  left_join(gonorrhoea_episodes_preg_min_total, by = c("patid" = "patid"))
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_max_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_max_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_min_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_min_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_max_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_max_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_min_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_min_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_max_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_max_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_min_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_min_total %>% replace_na(0)


cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc %>% 
  mutate(cuiud_pdays_max = (cuiud_pdays_max-pid_preg_max_total)) %>% 
  mutate(cuiud_pdays_min = (cuiud_pdays_min-pid_preg_min_total)) %>% 
  mutate(cuiud_pdays_max = (cuiud_pdays_max-chlamydia_preg_max_total)) %>% 
  mutate(cuiud_pdays_min = (cuiud_pdays_min-chlamydia_preg_min_total)) %>% 
  mutate(cuiud_pdays_max = (cuiud_pdays_max-gonorrhoea_preg_max_total)) %>% 
  mutate(cuiud_pdays_min = (cuiud_pdays_min-gonorrhoea_preg_min_total))
  

# Convert pdays to pyears ---

cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays_pyears <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays %>%
  mutate(cuiud_pyears_max = (cuiud_pdays_max/365.25)) %>%
  mutate(cuiud_pyears_min = (cuiud_pdays_min/365.25)) 
  

# dplyr::select final variables for cohort ---
cuiud_cohort_final <- dplyr::select(cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays_pyears, c(patid, pracid, prac_region,
                dob, yob, patimd, pracimd, imd, migrant_status, migcertainty, ethnicat, ethnicat6, data_start, data_end,
                cuiud_cohort_entry, cuiud_cohort_exit, cuiud_pdays_max, cuiud_pdays_min, cuiud_pyears_max,
                cuiud_pyears_min)) 

## Save & load CU IUD cohort .Rdata files --------------------------------------------------------

save(cuiud_cohort_final, file = "filepath")
load(file = "filepath") 


## Cu IUD prescribing episodes --------------------------------------------------------

cuiud_presc <- contr_presc_clean %>% filter(contrpresccat == "Copper intra-uterine device") 

## Change prescribing events that are same category on same day, to one event ---

n_distinct(cuiud_presc) == count(distinct(cuiud_presc, patid, eventdate, contrpresccat,  .keep_all = TRUE)) 
cuiudpresc_oneeventperdate <- cuiud_presc %>% distinct(patid, eventdate, contrpresccat,  .keep_all = TRUE)
count(cuiudpresc_oneeventperdate)

## Drop events that are not in cuiud cohort entry/exit time ---

cuiudpresc_indates <- left_join(cuiudpresc_oneeventperdate, cuiud_cohort_final,
                               by = c("patid" = "patid"))
cuiudpresc_indates_2 <- cuiudpresc_indates %>% 
  filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit) 
count(cuiudpresc_indates_2)


## Drop events that fall into pregnancy durations  ---

# Assign row ids to preg outcome episodes & convert to wide dataset
preg_episodes_pn4w_valid_vars <- dplyr::select(preg_episodes_pn4w_valid, 
                                               c(patid,eventdate,episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_pn4w_valid_eventids <- preg_episodes_pn4w_valid_vars %>% group_by(patid) %>% mutate(eventid = row_number())
summary(preg_episodes_pn4w_valid_eventids) 
preg_episodes_pn4w_valid_wide <- pivot_wider(preg_episodes_pn4w_valid_eventids, names_from = eventid, 
                                             values_from = c(eventdate, episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_pn4w_valid_wide <- as.data.frame(preg_episodes_pn4w_valid_wide)
count(preg_episodes_pn4w_valid_wide)
count(distinct(preg_episodes_pn4w_valid_eventids)) 

# Join wide pregnancy to cu iud prescriptions 
cuiudpresc_pregpn4w <- left_join(cuiudpresc_indates_2, preg_episodes_pn4w_valid_wide , 
                                 by = c("patid" = "patid"))

# Drop cu iud presc events that fall into pregn max intervals
cuiudpresc_notinpregpn4w_max <- cuiudpresc_pregpn4w %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_max_1 & eventdate <= episode_end_max_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_max_2 & eventdate <= episode_end_max_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_max_3 & eventdate <= episode_end_max_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_max_4 & eventdate <= episode_end_max_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_max_5 & eventdate <= episode_end_max_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_max_6 & eventdate <= episode_end_max_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_max_7 & eventdate <= episode_end_max_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_max_8 & eventdate <= episode_end_max_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_max_9 & eventdate <= episode_end_max_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_max_10 & eventdate <= episode_end_max_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_max_11 & eventdate <= episode_end_max_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no") 

# Drop cu iud presc events that fall into pregn min intervals
cuiudpresc_notinpregpn4w_min <- cuiudpresc_pregpn4w %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_min_1 & eventdate <= episode_end_min_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_min_2 & eventdate <= episode_end_min_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_min_3 & eventdate <= episode_end_min_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_min_4 & eventdate <= episode_end_min_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_min_5 & eventdate <= episode_end_min_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_min_6 & eventdate <= episode_end_min_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_min_7 & eventdate <= episode_end_min_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_min_8 & eventdate <= episode_end_min_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_min_9 & eventdate <= episode_end_min_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_min_10 & eventdate <= episode_end_min_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_min_11 & eventdate <= episode_end_min_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no")


# Final cuiud presc files with pregn max duration assumption and pregn min duration assumption
cuiudpresc_pregmaxassumption <- dplyr::select(cuiudpresc_notinpregpn4w_max, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                              issueseq))
cuiudpresc_pregminassumption <- dplyr::select(cuiudpresc_notinpregpn4w_min, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                              issueseq))
count(cuiudpresc_pregmaxassumption)
count(cuiudpresc_pregminassumption)


## Drop events that fall into conditions with short intervals based on preg max/min assumption ---

## Drop events that fall into pid not in preg max
pid_long <- pid_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(pid_long)

pid_distinct_patid <- pid_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,5,1), times = nrow(pid_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
pid_distinct_patid_2 <- pid_distinct_patid %>%
  slice(rep(1:n(), each=5))
pid_distinct_patid_2$eventid <- eventid_variable$eventid

pid_long_expanded <-  left_join(pid_distinct_patid_2, pid_long, 
                                by = c("patid" = "patid", "eventid" = "id"))

pid_long <- as.data.frame(pid_long)
pid_wide <- spread(pid_long_expanded, eventid, eventdate)
pid_wide <- pid_wide %>% rename_with( ~ paste("pid", .x, sep = "_"))

cuiudpresc_pid <- left_join(cuiudpresc_pregmaxassumption, pid_wide, 
                            by = c("patid" = "pid_patid"))
cuiudpresc_notinpid_pregmax <- cuiudpresc_pid %>% 
  mutate(to_drop = ifelse(is.na(pid_1), "no",
                          ifelse(eventdate >= pid_1 & eventdate <= (pid_1+14), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(pid_2), "no",
                          ifelse(eventdate >= pid_2 & eventdate <= (pid_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(pid_3), "no",
                           ifelse(eventdate >= pid_3 & eventdate <= (pid_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(pid_4), "no",
                           ifelse(eventdate >= pid_4 & eventdate <= (pid_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(pid_5), "no",
                           ifelse(eventdate >= pid_5 & eventdate <= (pid_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") 
cuiudpresc_notinpid_pregmax <- dplyr::select(cuiudpresc_notinpid_pregmax,c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                           issueseq))
count(cuiudpresc_notinpid_pregmax) 

## Drop events that fall into ct pregmax durations
chlamydia_long <- chlamydia_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(chlamydia_long)

chlamydia_distinct_patid <- chlamydia_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,7,1), times = nrow(chlamydia_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
chlamydia_distinct_patid_2 <- chlamydia_distinct_patid %>%
  slice(rep(1:n(), each=7))
chlamydia_distinct_patid_2$eventid <- eventid_variable$eventid

chlamydia_long_expanded <-  left_join(chlamydia_distinct_patid_2, chlamydia_long, 
                                by = c("patid" = "patid", "eventid" = "id"))

chlamydia_long <- as.data.frame(chlamydia_long)
chlamydia_wide <- spread(chlamydia_long_expanded, eventid, eventdate)
chlamydia_wide <- chlamydia_wide %>% rename_with( ~ paste("chlamydia", .x, sep = "_"))

cuiudpresc_chlamydia <- left_join(cuiudpresc_notinpid_pregmax, chlamydia_wide, 
                            by = c("patid" = "chlamydia_patid"))
cuiudpresc_notinchlamydia_pregmax <- cuiudpresc_chlamydia %>% 
  mutate(to_drop = ifelse(is.na(chlamydia_1), "no",
                          ifelse(eventdate >= chlamydia_1 & eventdate <= (chlamydia_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(chlamydia_2), "no",
                           ifelse(eventdate >= chlamydia_2 & eventdate <= (chlamydia_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(chlamydia_3), "no",
                           ifelse(eventdate >= chlamydia_3 & eventdate <= (chlamydia_3+7), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(chlamydia_4), "no",
                           ifelse(eventdate >= chlamydia_4 & eventdate <= (chlamydia_4+7), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(chlamydia_5), "no",
                           ifelse(eventdate >= chlamydia_5 & eventdate <= (chlamydia_5+7), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(chlamydia_6), "no",
                           ifelse(eventdate >= chlamydia_6 & eventdate <= (chlamydia_6+7), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(chlamydia_7), "no",
                           ifelse(eventdate >= chlamydia_7 & eventdate <= (chlamydia_7+7), "yes", "no"))) %>% 
  filter(to_drop7 == "no") 

cuiudpresc_notinpidct_pregmax <- dplyr::select(cuiudpresc_notinchlamydia_pregmax, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                    issueseq))
count(cuiudpresc_notinpidct_pregmax) 

## Drop events that fall into gc durations
gonorrhoea_long <- gonorrhoea_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(gonorrhoea_long)

gonorrhoea_distinct_patid <- gonorrhoea_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,2,1), times = nrow(gonorrhoea_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
gonorrhoea_distinct_patid_2 <- gonorrhoea_distinct_patid %>%
  slice(rep(1:n(), each=2))
gonorrhoea_distinct_patid_2$eventid <- eventid_variable$eventid

gonorrhoea_long_expanded <-  left_join(gonorrhoea_distinct_patid_2, gonorrhoea_long, 
                                      by = c("patid" = "patid", "eventid" = "id"))

gonorrhoea_long <- as.data.frame(gonorrhoea_long)
gonorrhoea_wide <- spread(gonorrhoea_long_expanded, eventid, eventdate)
gonorrhoea_wide <- gonorrhoea_wide %>% rename_with( ~ paste("gonorrhoea", .x, sep = "_"))

cuiudpresc_gonorrhoea <- left_join(cuiudpresc_notinpidct_pregmax, gonorrhoea_wide, 
                                  by = c("patid" = "gonorrhoea_patid"))
cuiudpresc_notingonorrhoea_pregmax <- cuiudpresc_gonorrhoea %>% 
  mutate(to_drop = ifelse(is.na(gonorrhoea_1), "no",
                          ifelse(eventdate >= gonorrhoea_1 & eventdate <= (gonorrhoea_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(gonorrhoea_2), "no",
                           ifelse(eventdate >= gonorrhoea_2 & eventdate <= (gonorrhoea_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") 


cuiudpresc_notinpidctgc_pregmax <- dplyr::select(cuiudpresc_notingonorrhoea_pregmax, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                       issueseq))
count(cuiudpresc_notinpidctgc_pregmax) 


## Drop events that fall into pid not in preg min
pid_long <- pid_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(pid_long)

pid_distinct_patid <- pid_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,5,1), times = nrow(pid_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
pid_distinct_patid_2 <- pid_distinct_patid %>%
  slice(rep(1:n(), each=5))
pid_distinct_patid_2$eventid <- eventid_variable$eventid

pid_long_expanded <-  left_join(pid_distinct_patid_2, pid_long, 
                                by = c("patid" = "patid", "eventid" = "id"))

pid_long <- as.data.frame(pid_long)
pid_wide <- spread(pid_long_expanded, eventid, eventdate)
pid_wide <- pid_wide %>% rename_with( ~ paste("pid", .x, sep = "_"))

cuiudpresc_pid <- left_join(cuiudpresc_pregminassumption, pid_wide, 
                            by = c("patid" = "pid_patid"))
cuiudpresc_notinpid_pregmin <- cuiudpresc_pid %>% 
  mutate(to_drop = ifelse(is.na(pid_1), "no",
                          ifelse(eventdate >= pid_1 & eventdate <= (pid_1+14), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(pid_2), "no",
                           ifelse(eventdate >= pid_2 & eventdate <= (pid_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(pid_3), "no",
                           ifelse(eventdate >= pid_3 & eventdate <= (pid_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(pid_4), "no",
                           ifelse(eventdate >= pid_4 & eventdate <= (pid_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(pid_5), "no",
                           ifelse(eventdate >= pid_5 & eventdate <= (pid_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") 
cuiudpresc_notinpid_pregmin <- dplyr::select(cuiudpresc_notinpid_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                            issueseq))
count(cuiudpresc_notinpid_pregmin)

## Drop events that fall into ct pregmin durations
chlamydia_long <- chlamydia_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(chlamydia_long)

chlamydia_distinct_patid <- chlamydia_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,7,1), times = nrow(chlamydia_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
chlamydia_distinct_patid_2 <- chlamydia_distinct_patid %>%
  slice(rep(1:n(), each=7))
chlamydia_distinct_patid_2$eventid <- eventid_variable$eventid

chlamydia_long_expanded <-  left_join(chlamydia_distinct_patid_2, chlamydia_long, 
                                      by = c("patid" = "patid", "eventid" = "id"))

chlamydia_long <- as.data.frame(chlamydia_long)
chlamydia_wide <- spread(chlamydia_long_expanded, eventid, eventdate)
chlamydia_wide <- chlamydia_wide %>% rename_with( ~ paste("chlamydia", .x, sep = "_"))

cuiudpresc_chlamydia <- left_join(cuiudpresc_notinpid_pregmin, chlamydia_wide, 
                                  by = c("patid" = "chlamydia_patid"))
cuiudpresc_notinchlamydia_pregmin <- cuiudpresc_chlamydia %>% 
  mutate(to_drop = ifelse(is.na(chlamydia_1), "no",
                          ifelse(eventdate >= chlamydia_1 & eventdate <= (chlamydia_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(chlamydia_2), "no",
                           ifelse(eventdate >= chlamydia_2 & eventdate <= (chlamydia_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(chlamydia_3), "no",
                           ifelse(eventdate >= chlamydia_3 & eventdate <= (chlamydia_3+7), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(chlamydia_4), "no",
                           ifelse(eventdate >= chlamydia_4 & eventdate <= (chlamydia_4+7), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(chlamydia_5), "no",
                           ifelse(eventdate >= chlamydia_5 & eventdate <= (chlamydia_5+7), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(chlamydia_6), "no",
                           ifelse(eventdate >= chlamydia_6 & eventdate <= (chlamydia_6+7), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(chlamydia_7), "no",
                           ifelse(eventdate >= chlamydia_7 & eventdate <= (chlamydia_7+7), "yes", "no"))) %>% 
  filter(to_drop7 == "no") 

cuiudpresc_notinpidct_pregmin <- dplyr::select(cuiudpresc_notinchlamydia_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                   issueseq))
count(cuiudpresc_notinpidct_pregmin)

## Drop events that fall into gc pregmin durations
gonorrhoea_long <- gonorrhoea_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(gonorrhoea_long)

gonorrhoea_distinct_patid <- gonorrhoea_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,2,1), times = nrow(gonorrhoea_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
gonorrhoea_distinct_patid_2 <- gonorrhoea_distinct_patid %>%
  slice(rep(1:n(), each=2))
gonorrhoea_distinct_patid_2$eventid <- eventid_variable$eventid

gonorrhoea_long_expanded <-  left_join(gonorrhoea_distinct_patid_2, gonorrhoea_long, 
                                       by = c("patid" = "patid", "eventid" = "id"))

gonorrhoea_long <- as.data.frame(gonorrhoea_long)
gonorrhoea_wide <- spread(gonorrhoea_long_expanded, eventid, eventdate)
gonorrhoea_wide <- gonorrhoea_wide %>% rename_with( ~ paste("gonorrhoea", .x, sep = "_"))

cuiudpresc_gonorrhoea <- left_join(cuiudpresc_notinpidct_pregmin, gonorrhoea_wide, 
                                   by = c("patid" = "gonorrhoea_patid"))
cuiudpresc_notingonorrhoea_pregmin <- cuiudpresc_gonorrhoea %>% 
  mutate(to_drop = ifelse(is.na(gonorrhoea_1), "no",
                          ifelse(eventdate >= gonorrhoea_1 & eventdate <= (gonorrhoea_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(gonorrhoea_2), "no",
                           ifelse(eventdate >= gonorrhoea_2 & eventdate <= (gonorrhoea_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") 


cuiudpresc_notinpidctgc_pregmin <- dplyr::select(cuiudpresc_notingonorrhoea_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                      issueseq))
count(cuiudpresc_notinpidctgc_pregmin) 


## Create annual cuiud presc counts (preg max and min assumption) ---


# create cuiud cohort file with line for each year that patient is in cohort 
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(cuiud_cohort_final)))
colnames(year_variable) <- 'eventyear'
cuiud_annual_cohort <- cuiud_cohort_final %>%
  slice(rep(1:n(), each=10))
cuiud_annual_cohort$eventyear <- year_variable$eventyear
cuiud_annual_cohort <- filter(cuiud_annual_cohort, eventyear >= year(cuiud_cohort_entry) & eventyear <= year(cuiud_cohort_exit))

# Create annual cuiud presc episodes count 

cuiudpresc_pregmaxassumption_annual_counts <- cuiudpresc_notinpidctgc_pregmax %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(cuiudpresc_n_pregmax = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, cuiudpresc_n_pregmax) %>%
  distinct()

cuiudpresc_pregminassumption_annual_counts <- cuiudpresc_notinpidctgc_pregmin %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(cuiudpresc_n_pregmin = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, cuiudpresc_n_pregmin) %>%
  distinct()

# Join annual cohort file to annual cuiud presc files, change NAs to 0

cuiudpresc_annual_counts <- cuiud_annual_cohort %>%
  left_join(cuiudpresc_pregmaxassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(cuiudpresc_pregminassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear"))
cuiudpresc_annual_counts$cuiudpresc_n_pregmax <- cuiudpresc_annual_counts$cuiudpresc_n_pregmax %>% replace_na(0)
cuiudpresc_annual_counts$cuiudpresc_n_pregmin <- cuiudpresc_annual_counts$cuiudpresc_n_pregmin %>% replace_na(0)

# Add age during event year and age category 

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

cuiudpresc_annual_counts_age <- cuiudpresc_annual_counts %>% mutate(age_date= eventyear) 
cuiudpresc_annual_counts_age$age_date <- as.character(cuiudpresc_annual_counts_age$age_date)
cuiudpresc_annual_counts_age$age_date <- paste("01-01-", cuiudpresc_annual_counts_age$age_date, sep="")
head(cuiudpresc_annual_counts_age$age_date)
cuiudpresc_annual_counts_age$age_date <- dmy(cuiudpresc_annual_counts_age$age_date)
head(cuiudpresc_annual_counts_age$age_date)
class(cuiudpresc_annual_counts_age$age_date)
cuiudpresc_annual_counts_age <- cuiudpresc_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(cuiudpresc_annual_counts_age$eventyear_age)
max(cuiudpresc_annual_counts_age$eventyear_age) 
cuiudpresc_annual_counts_age_50 <- cuiudpresc_annual_counts_age %>% filter(eventyear_age == 50)
cuiudpresc_annual_counts_age_final <- cuiudpresc_annual_counts_age %>% 
  filter(eventyear_age != 50) %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
cuiudpresc_annual_counts_age_final$eventyear_agecat <- factor(cuiudpresc_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                                labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Final file
cuiudpresc_annual_counts_final <- cuiudpresc_annual_counts_age_final %>% dplyr::select(-c(age_date))

## Save and load final file
save(cuiudpresc_annual_counts_final, file = "filepath")
load(file = "filepath")



## Set up data for stratifying by time variant variable (i.e add pdays per year preg max and pregmin, duration of preg/pic/ct/gc removed per year ---

# Add year durations
cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
cuiudpresc_annual_counts_final_extra$start_eventyear <- as_date(cuiudpresc_annual_counts_final_extra$start_eventyear)
cuiudpresc_annual_counts_final_extra$end_eventyear <- as_date(cuiudpresc_annual_counts_final_extra$end_eventyear)
cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(cuiud_cohort_entry <= start_eventyear, start_eventyear,cuiud_cohort_entry))
cuiudpresc_annual_counts_final_extra$t0_ey <- as_date(cuiudpresc_annual_counts_final_extra$t0_ey)
cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(cuiud_cohort_exit >= end_eventyear, end_eventyear,cuiud_cohort_exit))
cuiudpresc_annual_counts_final_extra$t1_ey <- as_date(cuiudpresc_annual_counts_final_extra$t1_ey)
cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) 
cuiudpresc_annual_counts_final_extra$pdays_ey <- as.numeric(cuiudpresc_annual_counts_final_extra$pdays_ey)

# Calc duration of PID/CT/GC/Pregn in each year based on preg max/min assumption

x <- preg_episodes_pn4w_valid %>% filter(year(episode_end_max) == year(episode_start_max)) %>% 
  mutate(year = year(episode_end_max), pregduration_max = (episode_end_max - episode_start_max)+1) %>% 
  dplyr::select(patid, year, pregduration_max)
x$pregduration_max <- as.numeric(x$pregduration_max)

y <- preg_episodes_pn4w_valid %>% filter(year(episode_end_max) != year(episode_start_max)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_max = ifelse(start_end == "episode_start_max", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_max", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_max)

preg_annual_durations_max <- rbind(x,y) 
preg_annual_durations_max_grouped <- preg_annual_durations_max %>% group_by(patid, year) %>% tally(pregduration_max) 
preg_annual_durations_max_grouped <- rename(preg_annual_durations_max_grouped, pregduration_max = n)

x <- preg_episodes_pn4w_valid %>% filter(year(episode_end_min) == year(episode_start_min)) %>% 
  mutate(year = year(episode_end_min), pregduration_min = (episode_end_min - episode_start_min)+1) %>% 
  dplyr::select(patid, year, pregduration_min)
x$pregduration_min <- as.numeric(x$pregduration_min)

y <- preg_episodes_pn4w_valid %>% filter(year(episode_end_min) != year(episode_start_min)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_min = ifelse(start_end == "episode_start_min", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_min", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_min)

preg_annual_durations_min <- rbind(x,y) 
preg_annual_durations_min_grouped <- preg_annual_durations_min %>% group_by(patid, year) %>% tally(pregduration_min)
preg_annual_durations_min_grouped <- rename(preg_annual_durations_min_grouped, pregduration_min = n)
                       
pid_annual_durations_pregmax <- pid_notinpregmax %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(pidduration_max = 14*n) %>% 
  dplyr::select(patid, year, pidduration_max)

ct_annual_durations_pregmax <- chlamydia_notinpregmax  %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(ctduration_max = 7*n)  %>% 
  dplyr::select(patid, year, ctduration_max)

gc_annual_durations_pregmax <- gonorrhoea_notinpregmax %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(gcduration_max = 14*n)  %>% 
  dplyr::select(patid, year, gcduration_max)

pid_annual_durations_pregmin <- pid_notinpregmin %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(pidduration_min = 14*n)  %>% 
  dplyr::select(patid, year, pidduration_min)

ct_annual_durations_pregmin <- chlamydia_notinpregmin  %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(ctduration_min = 7*n)  %>% 
  dplyr::select(patid, year, ctduration_min)

gc_annual_durations_pregmin <- gonorrhoea_notinpregmin %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(gcduration_min = 14*n)  %>% 
  dplyr::select(patid, year, gcduration_min)

## subtract preg/pid/ct/gc duratins from pdays_ey (max and min)

cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>% 
  left_join(preg_annual_durations_max_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(pid_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(ct_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(gc_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(preg_annual_durations_min_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(pid_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(ct_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(gc_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year"))


cuiudpresc_annual_counts_final_extra$pregduration_max <- cuiudpresc_annual_counts_final_extra$pregduration_max %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$pidduration_max <- cuiudpresc_annual_counts_final_extra$pidduration_max %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$ctduration_max <- cuiudpresc_annual_counts_final_extra$ctduration_max %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$gcduration_max <- cuiudpresc_annual_counts_final_extra$gcduration_max %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$pregduration_min <- cuiudpresc_annual_counts_final_extra$pregduration_min %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$pidduration_min <- cuiudpresc_annual_counts_final_extra$pidduration_min %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$ctduration_min <- cuiudpresc_annual_counts_final_extra$ctduration_min %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$gcduration_min <- cuiudpresc_annual_counts_final_extra$gcduration_min %>% replace_na(0)

cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>% 
  mutate(pdays_ey_pregmax = pdays_ey-(pregduration_max + pidduration_max + ctduration_max + gcduration_max)) %>% 
  mutate(pdays_ey_pregmin = pdays_ey-(pregduration_min + pidduration_min + ctduration_min + gcduration_min)) %>% 
  mutate(pyears_ey_pregmax=pdays_ey_pregmax/365.25) %>% 
  mutate(pyears_ey_pregmin=pdays_ey_pregmin/365.25)


## Save & load annual counts final extra (for glms) ---

cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>%dplyr::select(patid, eventyear, pracid, prac_region, dob, yob, patimd, pracimd, imd, 
                                                                                              migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                              data_start, data_end, cuiud_cohort_entry, cuiud_cohort_exit, cuiud_pdays_max, 
                                                                                              cuiud_pdays_min, cuiud_pyears_max, cuiud_pyears_min, eventyear_age, eventyear_agecat, 
                                                                                              cuiudpresc_n_pregmax, cuiudpresc_n_pregmin, pdays_ey_pregmax, pdays_ey_pregmin, 
                                                                                              pyears_ey_pregmax, pyears_ey_pregmin)





save(cuiudpresc_annual_counts_final_extra, file = "filepath")
load(file = "filepath")

## IUS cohort creation -------------------------------------------------------------------------

## Adjust for conditions that affect cohort entry/exit ---

# dplyr::select incident cervical_ca events
count(cervical_ca_clean) == count(distinct(cervical_ca_clean, patid)) 
incidentevents_cervical_ca <- cervical_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_cervical_ca <- as.data.frame(incidentevents_cervical_ca)
count(incidentevents_cervical_ca) == count(distinct(cervical_ca_clean, patid)) 

# dplyr::select incident endometrial_ca events
count(endometrial_ca_clean) == count(distinct(endometrial_ca_clean, patid))
incidentevents_endometrial_ca <- endometrial_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_endometrial_ca <- as.data.frame(incidentevents_endometrial_ca)
count(incidentevents_endometrial_ca) == count(distinct(endometrial_ca_clean, patid)) 

# dplyr::select incident malignant gtd events 
malignant_gtd <- gtd_clean %>% filter(gtdcat == "Definite")
count(malignant_gtd) == count(distinct(malignant_gtd, patid)) 
incidentevents_malignant_gtd <- malignant_gtd %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_malignant_gtd <- as.data.frame(incidentevents_malignant_gtd)
count(incidentevents_malignant_gtd) == count(distinct(malignant_gtd, patid)) 

# dplyr::select incident pelvic_tb events
count(pelvic_tb_clean) == count(distinct(pelvic_tb_clean, patid)) 
incidentevents_pelvic_tb <- pelvic_tb_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_pelvic_tb <- as.data.frame(incidentevents_pelvic_tb)
count(incidentevents_pelvic_tb) == count(distinct(pelvic_tb_clean, patid))  

# dplyr::select incident cirrhosis + dcld events, combine and then dplyr::select incident event for those with in cirrhosis event
# to end up with incident severe decompensated cirrhosis event
count(dcld_cirrhosis_clean) == count(distinct(dcld_cirrhosis_clean, patid))
incidentevents_dcld_cirrhosis <- dcld_cirrhosis_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_cirrhosis <- as.data.frame(incidentevents_dcld_cirrhosis)
count(incidentevents_dcld_cirrhosis) == count(distinct(dcld_cirrhosis_clean, patid)) 

count(dcld_ascites_clean) == count(distinct(dcld_ascites_clean, patid)) 
incidentevents_dcld_ascites <- dcld_ascites_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_ascites <- as.data.frame(incidentevents_dcld_ascites)
count(incidentevents_dcld_ascites) == count(distinct(dcld_ascites_clean, patid))

count(dcld_jaundice_clean) == count(distinct(dcld_jaundice_clean, patid)) 
incidentevents_dcld_jaundice <- dcld_jaundice_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_jaundice <- as.data.frame(incidentevents_dcld_jaundice)
count(incidentevents_dcld_jaundice) == count(distinct(dcld_jaundice_clean, patid))

count(dcld_varices_clean) == count(distinct(dcld_varices_clean, patid))
incidentevents_dcld_varices <- dcld_varices_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_varices <- as.data.frame(incidentevents_dcld_varices)
count(incidentevents_dcld_varices) == count(distinct(dcld_varices_clean, patid)) 

dcld_all <- full_join(incidentevents_dcld_cirrhosis, incidentevents_dcld_jaundice, 
                                     by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_dcld_varices, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_dcld_all <- dcld_all %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_dcld_all <- as.data.frame(incident_dcld_all)
count(incident_dcld_all) == count(distinct(incident_dcld_all)) 
incidentevents_dcld_all_cirrhosis <- incident_dcld_all %>% filter(incident_dcld_all$patid %in% incidentevents_dcld_cirrhosis$patid)

# dplyr::select incident hep adenoma 
count(hep_adenoma_clean) == count(distinct(hep_adenoma_clean, patid)) 
incidentevents_hep_adenoma <- hep_adenoma_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_adenoma <- as.data.frame(incidentevents_hep_adenoma)
count(incidentevents_hep_adenoma) == count(distinct(hep_adenoma_clean, patid))

# dplyr::select incident hep ca
count(hep_ca_clean) == count(distinct(hep_ca_clean, patid))
incidentevents_hep_ca <- hep_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_ca <- as.data.frame(incidentevents_hep_ca)
count(incidentevents_hep_ca) == count(distinct(hep_ca_clean, patid))  

# Combine patients with incident condition affecting cohort entry/exit and dplyr::select earliest date of these
conditions <- full_join(incidentevents_cervical_ca, incidentevents_endometrial_ca, 
                                     by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_malignant_gtd, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_pelvic_tb, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_dcld_all_cirrhosis, 
            by = c("patid" = "patid", "eventdate" = "eventdate"))  %>% 
  full_join(incidentevents_hep_adenoma, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_hep_ca, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_conditions <- conditions %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_conditions <- as.data.frame(incident_conditions)
count(incident_conditions) == count(distinct(incident_conditions))
incident_conditions <- rename(incident_conditions, conditions_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
ius_cohort <- left_join(contraception_cohort_entry_exit_pdays_pyears, incident_conditions, 
                          by = c("patid" = "patid"))
ius_cohort_entry <-  ius_cohort %>% 
  mutate(ius_cohort_entry = ifelse(is.na(conditions_eventdate), contr_cohort_entry,
                                     ifelse(conditions_eventdate <= contr_cohort_entry, NA, contr_cohort_entry)))
summary(is.na(ius_cohort_entry$ius_cohort_entry)) 
ius_cohort_entry_eligible <- ius_cohort_entry %>% filter(!is.na(ius_cohort_entry))

# Amend cohort exit dates
ius_cohort_entry_exit <- ius_cohort_entry_eligible %>% 
  mutate(ius_cohort_exit = ifelse(is.na(conditions_eventdate), contr_cohort_exit,
                                    ifelse(conditions_eventdate < contr_cohort_exit, conditions_eventdate, contr_cohort_exit)))
summary(is.na(ius_cohort_entry_exit$contr_cohort_exit))

# Recalculate pdays and pyears based on those eligible after adjusting for cervical.ca.endomca/malign gtd
ius_cohort_entry_exit_pdays <- ius_cohort_entry_exit %>%
  mutate(ius_pdays = (ius_cohort_exit-ius_cohort_entry)+1)  
ius_cohort_entry_exit_pdays$pdays <- as.numeric(ius_cohort_entry_exit_pdays$pdays)

# convert to date format
ius_cohort_entry_exit_pdays$ius_cohort_entry <-  as_date(ius_cohort_entry_exit_pdays$ius_cohort_entry) 
ius_cohort_entry_exit_pdays$ius_cohort_exit <-  as_date(ius_cohort_entry_exit_pdays$ius_cohort_exit) 



## Adjust for pregnancy and postnatal intervals based on pregnancy outcomes (max and min pdays and pyears) ---

# dplyr::select pregnancy outcomes only (Drop currently pregnant, postnatal and vague timing)

pregnancy_outcomes <- pregnancy_clean %>% filter(pregcat != "Currently pregnant" & pregcat != "Pregnancy of childbirth-related terms with vague timing"
                                                 & pregcat != "Puerperium or neonatal(within 6 weeks after delivery)"
                                                 & pregcat != "Postnatal (after delivery)")

# create episode_start (max and min) and episode_end (max and min) dates for pregnancy events

pregnancy_max <- pregnancy_outcomes %>% 
  mutate(episode_start_max = ifelse(pregcat == "Abnormal product of conception", (eventdate-84),
                                    ifelse(pregcat == "Spontaneous abortion", (eventdate-168),
                                           ifelse(pregcat == "Termination of pregnancy", (eventdate-168),
                                                  ifelse(pregcat == "Unspecified abortion", (eventdate-168),
                                                         ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate-301),
                                                                ifelse(pregcat == "Ectopic pregnancy", (eventdate-84),
                                                                       ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate-301), NA))))))))

pregnancy_max <- pregnancy_max  %>% 
  mutate(episode_end_max = ifelse(pregcat == "Abnormal product of conception", (eventdate),
                                  ifelse(pregcat == "Spontaneous abortion", (eventdate),
                                         ifelse(pregcat == "Termination of pregnancy", (eventdate),
                                                ifelse(pregcat == "Unspecified abortion", (eventdate),
                                                       ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate),
                                                              ifelse(pregcat == "Ectopic pregnancy", (eventdate),
                                                                     ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate), NA))))))))

pregnancy_max_min <- pregnancy_max %>% 
  mutate(episode_start_min = ifelse(pregcat == "Abnormal product of conception", (eventdate-42),
                                    ifelse(pregcat == "Spontaneous abortion", (eventdate-28),
                                           ifelse(pregcat == "Termination of pregnancy", (eventdate-42),
                                                  ifelse(pregcat == "Unspecified abortion", (eventdate-28),
                                                         ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate-161),
                                                                ifelse(pregcat == "Ectopic pregnancy", (eventdate-42),
                                                                       ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate-100), NA))))))))


pregnancy_max_min <- pregnancy_max_min  %>% 
  mutate(episode_end_min = ifelse(pregcat == "Abnormal product of conception", (eventdate),
                                  ifelse(pregcat == "Spontaneous abortion", (eventdate),
                                         ifelse(pregcat == "Termination of pregnancy", (eventdate),
                                                ifelse(pregcat == "Unspecified abortion", (eventdate),
                                                       ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate),
                                                              ifelse(pregcat == "Ectopic pregnancy", (eventdate),
                                                                     ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate), NA))))))))

# convert to date format
pregnancy_max_min  <-  pregnancy_max_min  %>%
  mutate_if(is.numeric, as_date)
pregnancy_max_min$patid  <-  as.integer(pregnancy_max_min$patid)  

# Filter for labour/del/condition at birth & stillbirth/iud/perinatal i.e. delivery after 24 weeks

preg_24plus <- pregnancy_max_min %>% filter(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death" )
preg_pre24 <- pregnancy_max_min %>% filter(pregcat == "Abnormal product of conception" | pregcat == "Spontaneous abortion" |
                                             pregcat == "Termination of pregnancy" | pregcat == "Unspecified abortion" |pregcat == "Ectopic pregnancy")


# Drop events within 175 days of preg_24plus events (21 d until earliest possible ovulation + 22 weeks from then until earliest possibledelivery after 24 weeks)
preg_24plus_lag <- preg_24plus %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
preg_24plus_episodes <- preg_24plus_lag %>% filter(eventdate > (previous+175) | is.na(previous))

# Drop events within 19 days of preg_24plus events (5 d until earliest possible ovulation + 2 weeks until earliest possible positive pregnanct test and subsequent pre 24 week outcome) 
preg_pre24_lag <- preg_pre24 %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
preg_pre24_episodes <- preg_pre24_lag %>% filter(eventdate > (previous+19) | is.na(previous)) 

# add exclusion period end date (i.e. date after event when other outcome events cannot occur) + join dataframes
preg_24plus_episodes_excl_end <- preg_24plus_episodes %>% mutate(excl_end = (eventdate+175)) 
preg_pre24_episodes_excl_end <- preg_pre24_episodes %>% mutate(excl_end = (eventdate+19)) 
preg_excl_end <- full_join(preg_24plus_episodes_excl_end, preg_pre24_episodes_excl_end, 
                           by = c("patid" = "patid", "pregcat" = "pregcat", "eventdate" = "eventdate", 
                                  "episode_start_max" = "episode_start_max","episode_end_max" = "episode_end_max",
                                  "episode_start_min" = "episode_start_min", "episode_end_min" = "episode_end_min",
                                  "previous" = "previous", "excl_end" = "excl_end"))

# add previous events exclusion end date 
preg_excl_end_prev <- preg_excl_end %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous_excl_end = lag(excl_end))

# drop episodes that fall into prev events exclusion period
preg_episodes <- preg_excl_end_prev %>% filter(eventdate > previous_excl_end | is.na(previous_excl_end))

# extend preg episode to include postnatal 4 weeks for 24 week plus pregnancy outcomes)
preg_episodes_pn4w <- preg_episodes %>% 
  mutate(episode_end_max = ifelse(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death", 
                                  episode_end_max+28, episode_end_max)) %>%
  mutate(episode_end_min = ifelse(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death", 
                                  episode_end_min+28, episode_end_min)) 
preg_episodes_pn4w  <-  preg_episodes_pn4w  %>%
  mutate_if(is.numeric, as_date)
preg_episodes_pn4w$patid  <-  as.integer(preg_episodes_pn4w$patid)  

# drop episodes that are not in cohort entry/exit period for patient 
preg_episodes_pn4w_cohort <- left_join(preg_episodes_pn4w, ius_cohort_entry_exit_pdays, by = c("patid" = "patid"))
preg_episodes_pn4w_valid <- preg_episodes_pn4w_cohort %>% filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit) %>% 
  dplyr::select(c(patid, eventdate, pregcat, episode_start_max, episode_end_max, episode_start_min, episode_end_min))

# calculate episode durations 
preg_episodes_pn4w_duration_max <- preg_episodes_pn4w_valid  %>% mutate(duration_max =(episode_end_max-episode_start_max)+1) 
preg_episodes_pn4w_duration_max_min <- preg_episodes_pn4w_duration_max %>%  mutate(duration_min = (episode_end_min-episode_start_min)+1)
preg_episodes_pn4w_total_max <- preg_episodes_pn4w_duration_max_min %>% group_by(patid) %>% tally(duration_max)
preg_episodes_pn4w_total_min <- preg_episodes_pn4w_duration_max_min %>% group_by(patid) %>% tally(duration_min)
preg_episodes_pn4w_total_max <- rename(preg_episodes_pn4w_total_max, preg_total_max = n)
preg_episodes_pn4w_total_min <- rename(preg_episodes_pn4w_total_min, preg_total_min = n)

# recalculate pdays and pyears (max and min based on max and min preg durations)
ius_cohort_pregnancy_pn4w_episodes <- left_join(ius_cohort_entry_exit_pdays, preg_episodes_pn4w_total_max,
                                                by = c("patid" = "patid")) %>%
  left_join(preg_episodes_pn4w_total_min, by = c("patid" = "patid"))
ius_cohort_pregnancy_pn4w_episodes$preg_total_max <- ius_cohort_pregnancy_pn4w_episodes$preg_total_max %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes$preg_total_min <- ius_cohort_pregnancy_pn4w_episodes$preg_total_min %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays <- ius_cohort_pregnancy_pn4w_episodes %>% 
  mutate(ius_pdays_max = (ius_pdays-preg_total_max)) %>%
  mutate(ius_pdays_min = (ius_pdays-preg_total_min)) 
ius_cohort_pregnancy_pn4w_episodes_pdays$ius_pdays_max <- as.numeric(ius_cohort_pregnancy_pn4w_episodes_pdays$ius_pdays_max)
ius_cohort_pregnancy_pn4w_episodes_pdays$ius_pdays_min <- as.numeric(ius_cohort_pregnancy_pn4w_episodes_pdays$ius_pdays_min)


## Adjust for conditions that have short intervals that affect pdays/pyears ---

# dplyr::select incident events for pid (drop codes that occur within 2 weeks of another code), CT (drop codes that occur within 1 weeks of another code) , & GC (drop codes that occur within 1 weeks of another code) 
pid_lag <- pid_clean %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate)) 
pid_episodes <- pid_lag %>% 
  filter(eventdate > (previous+14) | is.na(previous))
chlamydia_lag <- chlamydia_clean %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
chlamydia_episodes <- chlamydia_lag %>% filter(eventdate > (previous+7) | is.na(previous))
gonorrhoea_lag <- gonorrhoea_clean  %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
gonorrhoea_episodes <- gonorrhoea_lag %>% filter(eventdate > (previous+7) | is.na(previous))


# Drop episodes that occur outside of cohort entry/exit 
pid_episodes_iuscohort<- left_join(pid_episodes, ius_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
pidepisodes_iusvalid <- pid_episodes_iuscohort %>% filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit)
chlamydia_episodes_iuscohort<- left_join(chlamydia_episodes, ius_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
chlamydiaepisodes_iusvalid <- chlamydia_episodes_iuscohort %>% filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit)
gonorrhoea_episodes_iuscohort <- left_join(gonorrhoea_episodes, ius_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
gonorrhoeaepisodes_iusvalid <- gonorrhoea_episodes_iuscohort %>% filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit)


# create wide data of pregnancy max start date
preg_max <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_start_max))
preg_max_long <- preg_max %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_max_long)
preg_max_distinct_patid <- preg_max_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_max_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_max_distinct_patid_2 <- preg_max_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_max_distinct_patid_2$eventid <- eventid_variable$eventid
preg_max_long_expanded <-  left_join(preg_max_distinct_patid_2, preg_max_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_max_wide_start <- spread(preg_max_long_expanded , eventid, episode_start_max)
preg_max_wide_start <- preg_max_wide_start %>% rename_with( ~ paste("preg_max_start", .x, sep = "_"))

# create wide data of pregnancy max end date
preg_max <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_end_max))
preg_max_long <- preg_max %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_max_long)
preg_max_distinct_patid <- preg_max_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_max_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_max_distinct_patid_2 <- preg_max_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_max_distinct_patid_2$eventid <- eventid_variable$eventid
preg_max_long_expanded <-  left_join(preg_max_distinct_patid_2, preg_max_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_max_wide_end <- spread(preg_max_long_expanded , eventid, episode_end_max)
preg_max_wide_end <- preg_max_wide_end %>% rename_with( ~ paste("preg_max_end", .x, sep = "_"))

# Join preg max wide datasets
preg_max_wide <- full_join(preg_max_wide_start, preg_max_wide_end, by = c("preg_max_start_patid" = "preg_max_end_patid"))

# Drop pid events that fall into preg max
pid_preg_max <- left_join(pidepisodes_iusvalid, preg_max_wide, 
                          by = c("patid" = "preg_max_start_patid"))
pid_notinpregmax <- pid_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
pid_notinpregmax <- dplyr::select(pid_notinpregmax, c(patid, eventdate))
count(pid_notinpregmax) 

# Drop chlamydia events that fall into preg max
chlamydia_preg_max <- left_join(chlamydiaepisodes_iusvalid, preg_max_wide, 
                                by = c("patid" = "preg_max_start_patid"))
chlamydia_notinpregmax <- chlamydia_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
chlamydia_notinpregmax <- dplyr::select(chlamydia_notinpregmax, c(patid, eventdate))
count(chlamydia_notinpregmax) 

# Drop gonorrhoea events that fall into preg max
gonorrhoea_preg_max <- left_join(gonorrhoeaepisodes_iusvalid, preg_max_wide, 
                                 by = c("patid" = "preg_max_start_patid"))
gonorrhoea_notinpregmax <- gonorrhoea_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
gonorrhoea_notinpregmax <- dplyr::select(gonorrhoea_notinpregmax, c(patid, eventdate))
count(gonorrhoea_notinpregmax) 


# create wide data of pregnancy min start date
preg_min <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_start_min))
preg_min_long <- preg_min %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_min_long)
preg_min_distinct_patid <- preg_min_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_min_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_min_distinct_patid_2 <- preg_min_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_min_distinct_patid_2$eventid <- eventid_variable$eventid
preg_min_long_expanded <-  left_join(preg_min_distinct_patid_2, preg_min_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_min_wide_start <- spread(preg_min_long_expanded , eventid, episode_start_min)
preg_min_wide_start <- preg_min_wide_start %>% rename_with( ~ paste("preg_min_start", .x, sep = "_"))

# create wide data of pregnancy min end date
preg_min <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_end_min))
preg_min_long <- preg_min %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_min_long)
preg_min_distinct_patid <- preg_min_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_min_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_min_distinct_patid_2 <- preg_min_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_min_distinct_patid_2$eventid <- eventid_variable$eventid
preg_min_long_expanded <-  left_join(preg_min_distinct_patid_2, preg_min_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_min_wide_end <- spread(preg_min_long_expanded , eventid, episode_end_min)
preg_min_wide_end <- preg_min_wide_end %>% rename_with( ~ paste("preg_min_end", .x, sep = "_"))

# Join preg min wide datasets
preg_min_wide <- full_join(preg_min_wide_start, preg_min_wide_end, by = c("preg_min_start_patid" = "preg_min_end_patid"))

# Drop pid events that fall into preg min
pid_preg_min <- left_join(pidepisodes_iusvalid, preg_min_wide, 
                          by = c("patid" = "preg_min_start_patid"))
pid_notinpregmin <- pid_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
pid_notinpregmin <- dplyr::select(pid_notinpregmin, c(patid, eventdate))
count(pid_notinpregmin) 

# Drop chlamydia events that fall into preg min
chlamydia_preg_min <- left_join(chlamydiaepisodes_iusvalid, preg_min_wide, 
                                by = c("patid" = "preg_min_start_patid"))
chlamydia_notinpregmin <- chlamydia_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
chlamydia_notinpregmin <- dplyr::select(chlamydia_notinpregmin, c(patid, eventdate))
count(chlamydia_notinpregmin) 

# Drop gonorrhoea events that fall into preg min
gonorrhoea_preg_min <- left_join(gonorrhoeaepisodes_iusvalid, preg_min_wide, 
                                 by = c("patid" = "preg_min_start_patid"))
gonorrhoea_notinpregmin <- gonorrhoea_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
gonorrhoea_notinpregmin <- dplyr::select(gonorrhoea_notinpregmin, c(patid, eventdate))
count(gonorrhoea_notinpregmin) 



# add durations for infection intervals (pid, chlamydia, gonorrhoea) 
pid_episodes_preg_max_duration <- pid_notinpregmax %>% mutate(duration = 14) 
pid_episodes_preg_max_total <-pid_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
pid_episodes_preg_max_total <- rename(pid_episodes_preg_max_total, pid_preg_max_total = n)
pid_episodes_preg_min_duration <- pid_notinpregmin %>% mutate(duration = 14) 
pid_episodes_preg_min_total <-pid_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
pid_episodes_preg_min_total <- rename(pid_episodes_preg_min_total, pid_preg_min_total = n)

chlamydia_episodes_preg_max_duration <- chlamydia_notinpregmax %>% mutate(duration = 14) 
chlamydia_episodes_preg_max_total <-chlamydia_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
chlamydia_episodes_preg_max_total <- rename(chlamydia_episodes_preg_max_total, chlamydia_preg_max_total = n)
chlamydia_episodes_preg_min_duration <- chlamydia_notinpregmin %>% mutate(duration = 14) 
chlamydia_episodes_preg_min_total <-chlamydia_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
chlamydia_episodes_preg_min_total <- rename(chlamydia_episodes_preg_min_total, chlamydia_preg_min_total = n)

gonorrhoea_episodes_preg_max_duration <- gonorrhoea_notinpregmax %>% mutate(duration = 14) 
gonorrhoea_episodes_preg_max_total <-gonorrhoea_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
gonorrhoea_episodes_preg_max_total <- rename(gonorrhoea_episodes_preg_max_total, gonorrhoea_preg_max_total = n)
gonorrhoea_episodes_preg_min_duration <- gonorrhoea_notinpregmin %>% mutate(duration = 14) 
gonorrhoea_episodes_preg_min_total <-gonorrhoea_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
gonorrhoea_episodes_preg_min_total <- rename(gonorrhoea_episodes_preg_min_total, gonorrhoea_preg_min_total = n)

# recalculate pdays 
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc <- left_join(ius_cohort_pregnancy_pn4w_episodes_pdays, pid_episodes_preg_max_total,
                                                                  by = c("patid" = "patid")) %>%
  left_join(pid_episodes_preg_min_total, by = c("patid" = "patid"))  %>%
  left_join(chlamydia_episodes_preg_max_total, by = c("patid" = "patid"))  %>%
  left_join(chlamydia_episodes_preg_min_total, by = c("patid" = "patid"))  %>%
  left_join(gonorrhoea_episodes_preg_max_total, by = c("patid" = "patid"))  %>%
  left_join(gonorrhoea_episodes_preg_min_total, by = c("patid" = "patid"))
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_max_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_max_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_min_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_min_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_max_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_max_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_min_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_min_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_max_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_max_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_min_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_min_total %>% replace_na(0)


ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc %>% 
  mutate(ius_pdays_max = (ius_pdays_max-pid_preg_max_total)) %>% 
  mutate(ius_pdays_min = (ius_pdays_min-pid_preg_min_total)) %>% 
  mutate(ius_pdays_max = (ius_pdays_max-chlamydia_preg_max_total)) %>% 
  mutate(ius_pdays_min = (ius_pdays_min-chlamydia_preg_min_total)) %>% 
  mutate(ius_pdays_max = (ius_pdays_max-gonorrhoea_preg_max_total)) %>% 
  mutate(ius_pdays_min = (ius_pdays_min-gonorrhoea_preg_min_total))


# Convert pdays to pyears ---

ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays_pyears <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays %>%
  mutate(ius_pyears_max = (ius_pdays_max/365.25)) %>%
  mutate(ius_pyears_min = (ius_pdays_min/365.25)) 


# dplyr::select final variables for cohort --- 
ius_cohort_final <- dplyr::select(ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays_pyears, c(patid, pracid, prac_region, 
                                                                                  dob, yob, patimd, pracimd, imd, migrant_status, migcertainty, ethnicat, ethnicat6, data_start, data_end,
                                                                                  ius_cohort_entry, ius_cohort_exit, ius_pdays_max, ius_pdays_min, ius_pyears_max,
                                                                                  ius_pyears_min)) 


## Save & load IUS cohort  .Rdata files --------------------------------------------------------
save(ius_cohort_final, file = "filepath")
load(file = "filepath") 

## IUS prescribing episodes ---------------------------------------------------------------------

ius_presc <- contr_presc_clean %>% filter(contrpresccat == "Hormonal intra-uterine system") 

## Change prescribing events that are same category on same day, to one event ---

n_distinct(ius_presc) == count(distinct(ius_presc, patid, eventdate, contrpresccat,  .keep_all = TRUE)) 
iuspresc_oneeventperdate <- ius_presc %>% distinct(patid, eventdate, contrpresccat,  .keep_all = TRUE)
count(iuspresc_oneeventperdate) 

## Drop events that are not in ius cohort entry/exit time ---

iuspresc_indates <- left_join(iuspresc_oneeventperdate, ius_cohort_final,
                                by = c("patid" = "patid"))
iuspresc_indates_2 <- iuspresc_indates %>% 
  filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit) 
count(iuspresc_indates_2) 


## Drop events that fall into pregnancy durations  ---

# Assign row ids to preg outcome episodes & convert to wide dataset
preg_episodes_pn4w_valid_vars <- dplyr::select(preg_episodes_pn4w_valid, 
                                               c(patid,eventdate,episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_pn4w_valid_eventids <- preg_episodes_pn4w_valid_vars %>% group_by(patid) %>% mutate(eventid = row_number())
summary(preg_episodes_pn4w_valid_eventids) 
preg_episodes_pn4w_valid_wide <- pivot_wider(preg_episodes_pn4w_valid_eventids, names_from = eventid, 
                                             values_from = c(eventdate, episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_pn4w_valid_wide <- as.data.frame(preg_episodes_pn4w_valid_wide)
count(preg_episodes_pn4w_valid_wide) 
count(distinct(preg_episodes_pn4w_valid_eventids))  

# Join wide pregnancy to cu iud prescriptions 
iuspresc_pregpn4w <- left_join(iuspresc_indates_2, preg_episodes_pn4w_valid_wide , 
                                 by = c("patid" = "patid"))

# Drop cu iud presc events that fall into pregn max intervals
iuspresc_notinpregpn4w_max <- iuspresc_pregpn4w %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_max_1 & eventdate <= episode_end_max_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_max_2 & eventdate <= episode_end_max_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_max_3 & eventdate <= episode_end_max_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_max_4 & eventdate <= episode_end_max_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_max_5 & eventdate <= episode_end_max_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_max_6 & eventdate <= episode_end_max_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_max_7 & eventdate <= episode_end_max_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_max_8 & eventdate <= episode_end_max_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_max_9 & eventdate <= episode_end_max_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_max_10 & eventdate <= episode_end_max_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_max_11 & eventdate <= episode_end_max_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no") 

# Drop cu iud presc events that fall into pregn min intervals
iuspresc_notinpregpn4w_min <- iuspresc_pregpn4w %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_min_1 & eventdate <= episode_end_min_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_min_2 & eventdate <= episode_end_min_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_min_3 & eventdate <= episode_end_min_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_min_4 & eventdate <= episode_end_min_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_min_5 & eventdate <= episode_end_min_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_min_6 & eventdate <= episode_end_min_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_min_7 & eventdate <= episode_end_min_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_min_8 & eventdate <= episode_end_min_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_min_9 & eventdate <= episode_end_min_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_min_10 & eventdate <= episode_end_min_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_min_11 & eventdate <= episode_end_min_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no")


# Final ius presc files with pregn max duration assumption and pregn min duration assumption
iuspresc_pregmaxassumption <- dplyr::select(iuspresc_notinpregpn4w_max, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                              issueseq))
iuspresc_pregminassumption <- dplyr::select(iuspresc_notinpregpn4w_min, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                              issueseq))
count(iuspresc_pregmaxassumption) 
count(iuspresc_pregminassumption) 


## Drop events that fall into conditions with short intervals based on preg max/min assumption ---

## Drop events that fall into pid not in preg max
pid_long <- pid_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(pid_long)

pid_distinct_patid <- pid_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,5,1), times = nrow(pid_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
pid_distinct_patid_2 <- pid_distinct_patid %>%
  slice(rep(1:n(), each=5))
pid_distinct_patid_2$eventid <- eventid_variable$eventid

pid_long_expanded <-  left_join(pid_distinct_patid_2, pid_long, 
                                by = c("patid" = "patid", "eventid" = "id"))

pid_long <- as.data.frame(pid_long)
pid_wide <- spread(pid_long_expanded, eventid, eventdate)
pid_wide <- pid_wide %>% rename_with( ~ paste("pid", .x, sep = "_"))

iuspresc_pid <- left_join(iuspresc_pregmaxassumption, pid_wide, 
                            by = c("patid" = "pid_patid"))
iuspresc_notinpid_pregmax <- iuspresc_pid %>% 
  mutate(to_drop = ifelse(is.na(pid_1), "no",
                          ifelse(eventdate >= pid_1 & eventdate <= (pid_1+14), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(pid_2), "no",
                           ifelse(eventdate >= pid_2 & eventdate <= (pid_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(pid_3), "no",
                           ifelse(eventdate >= pid_3 & eventdate <= (pid_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(pid_4), "no",
                           ifelse(eventdate >= pid_4 & eventdate <= (pid_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(pid_5), "no",
                           ifelse(eventdate >= pid_5 & eventdate <= (pid_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") 
iuspresc_notinpid_pregmax <- dplyr::select(iuspresc_notinpid_pregmax,c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                           issueseq))
count(iuspresc_notinpid_pregmax) 

## Drop events that fall into ct pregmax durations
chlamydia_long <- chlamydia_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(chlamydia_long)

chlamydia_distinct_patid <- chlamydia_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,7,1), times = nrow(chlamydia_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
chlamydia_distinct_patid_2 <- chlamydia_distinct_patid %>%
  slice(rep(1:n(), each=7))
chlamydia_distinct_patid_2$eventid <- eventid_variable$eventid

chlamydia_long_expanded <-  left_join(chlamydia_distinct_patid_2, chlamydia_long, 
                                      by = c("patid" = "patid", "eventid" = "id"))

chlamydia_long <- as.data.frame(chlamydia_long)
chlamydia_wide <- spread(chlamydia_long_expanded, eventid, eventdate)
chlamydia_wide <- chlamydia_wide %>% rename_with( ~ paste("chlamydia", .x, sep = "_"))

iuspresc_chlamydia <- left_join(iuspresc_notinpid_pregmax, chlamydia_wide, 
                                  by = c("patid" = "chlamydia_patid"))
iuspresc_notinchlamydia_pregmax <- iuspresc_chlamydia %>% 
  mutate(to_drop = ifelse(is.na(chlamydia_1), "no",
                          ifelse(eventdate >= chlamydia_1 & eventdate <= (chlamydia_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(chlamydia_2), "no",
                           ifelse(eventdate >= chlamydia_2 & eventdate <= (chlamydia_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(chlamydia_3), "no",
                           ifelse(eventdate >= chlamydia_3 & eventdate <= (chlamydia_3+7), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(chlamydia_4), "no",
                           ifelse(eventdate >= chlamydia_4 & eventdate <= (chlamydia_4+7), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(chlamydia_5), "no",
                           ifelse(eventdate >= chlamydia_5 & eventdate <= (chlamydia_5+7), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(chlamydia_6), "no",
                           ifelse(eventdate >= chlamydia_6 & eventdate <= (chlamydia_6+7), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(chlamydia_7), "no",
                           ifelse(eventdate >= chlamydia_7 & eventdate <= (chlamydia_7+7), "yes", "no"))) %>% 
  filter(to_drop7 == "no") 

iuspresc_notinpidct_pregmax <- dplyr::select(iuspresc_notinchlamydia_pregmax, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                    issueseq))
count(iuspresc_notinpidct_pregmax) 

## Drop events that fall into gc durations
gonorrhoea_long <- gonorrhoea_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(gonorrhoea_long)

gonorrhoea_distinct_patid <- gonorrhoea_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,2,1), times = nrow(gonorrhoea_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
gonorrhoea_distinct_patid_2 <- gonorrhoea_distinct_patid %>%
  slice(rep(1:n(), each=2))
gonorrhoea_distinct_patid_2$eventid <- eventid_variable$eventid

gonorrhoea_long_expanded <-  left_join(gonorrhoea_distinct_patid_2, gonorrhoea_long, 
                                       by = c("patid" = "patid", "eventid" = "id"))

gonorrhoea_long <- as.data.frame(gonorrhoea_long)
gonorrhoea_wide <- spread(gonorrhoea_long_expanded, eventid, eventdate)
gonorrhoea_wide <- gonorrhoea_wide %>% rename_with( ~ paste("gonorrhoea", .x, sep = "_"))

iuspresc_gonorrhoea <- left_join(iuspresc_notinpidct_pregmax, gonorrhoea_wide, 
                                   by = c("patid" = "gonorrhoea_patid"))
iuspresc_notingonorrhoea_pregmax <- iuspresc_gonorrhoea %>% 
  mutate(to_drop = ifelse(is.na(gonorrhoea_1), "no",
                          ifelse(eventdate >= gonorrhoea_1 & eventdate <= (gonorrhoea_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(gonorrhoea_2), "no",
                           ifelse(eventdate >= gonorrhoea_2 & eventdate <= (gonorrhoea_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") 


iuspresc_notinpidctgc_pregmax <- dplyr::select(iuspresc_notingonorrhoea_pregmax, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                       issueseq))
count(iuspresc_notinpidctgc_pregmax) 


## Drop events that fall into pid not in preg min
pid_long <- pid_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(pid_long)

pid_distinct_patid <- pid_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,5,1), times = nrow(pid_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
pid_distinct_patid_2 <- pid_distinct_patid %>%
  slice(rep(1:n(), each=5))
pid_distinct_patid_2$eventid <- eventid_variable$eventid

pid_long_expanded <-  left_join(pid_distinct_patid_2, pid_long, 
                                by = c("patid" = "patid", "eventid" = "id"))

pid_long <- as.data.frame(pid_long)
pid_wide <- spread(pid_long_expanded, eventid, eventdate)
pid_wide <- pid_wide %>% rename_with( ~ paste("pid", .x, sep = "_"))

iuspresc_pid <- left_join(iuspresc_pregminassumption, pid_wide, 
                            by = c("patid" = "pid_patid"))
iuspresc_notinpid_pregmin <- iuspresc_pid %>% 
  mutate(to_drop = ifelse(is.na(pid_1), "no",
                          ifelse(eventdate >= pid_1 & eventdate <= (pid_1+14), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(pid_2), "no",
                           ifelse(eventdate >= pid_2 & eventdate <= (pid_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(pid_3), "no",
                           ifelse(eventdate >= pid_3 & eventdate <= (pid_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(pid_4), "no",
                           ifelse(eventdate >= pid_4 & eventdate <= (pid_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(pid_5), "no",
                           ifelse(eventdate >= pid_5 & eventdate <= (pid_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") 
iuspresc_notinpid_pregmin <- dplyr::select(iuspresc_notinpid_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                            issueseq))
count(iuspresc_notinpid_pregmin) 

## Drop events that fall into ct pregmin durations
chlamydia_long <- chlamydia_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(chlamydia_long)

chlamydia_distinct_patid <- chlamydia_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,7,1), times = nrow(chlamydia_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
chlamydia_distinct_patid_2 <- chlamydia_distinct_patid %>%
  slice(rep(1:n(), each=7))
chlamydia_distinct_patid_2$eventid <- eventid_variable$eventid

chlamydia_long_expanded <-  left_join(chlamydia_distinct_patid_2, chlamydia_long, 
                                      by = c("patid" = "patid", "eventid" = "id"))

chlamydia_long <- as.data.frame(chlamydia_long)
chlamydia_wide <- spread(chlamydia_long_expanded, eventid, eventdate)
chlamydia_wide <- chlamydia_wide %>% rename_with( ~ paste("chlamydia", .x, sep = "_"))

iuspresc_chlamydia <- left_join(iuspresc_notinpid_pregmin, chlamydia_wide, 
                                  by = c("patid" = "chlamydia_patid"))
iuspresc_notinchlamydia_pregmin <- iuspresc_chlamydia %>% 
  mutate(to_drop = ifelse(is.na(chlamydia_1), "no",
                          ifelse(eventdate >= chlamydia_1 & eventdate <= (chlamydia_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(chlamydia_2), "no",
                           ifelse(eventdate >= chlamydia_2 & eventdate <= (chlamydia_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(chlamydia_3), "no",
                           ifelse(eventdate >= chlamydia_3 & eventdate <= (chlamydia_3+7), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(chlamydia_4), "no",
                           ifelse(eventdate >= chlamydia_4 & eventdate <= (chlamydia_4+7), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(chlamydia_5), "no",
                           ifelse(eventdate >= chlamydia_5 & eventdate <= (chlamydia_5+7), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(chlamydia_6), "no",
                           ifelse(eventdate >= chlamydia_6 & eventdate <= (chlamydia_6+7), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(chlamydia_7), "no",
                           ifelse(eventdate >= chlamydia_7 & eventdate <= (chlamydia_7+7), "yes", "no"))) %>% 
  filter(to_drop7 == "no") 

iuspresc_notinpidct_pregmin <- dplyr::select(iuspresc_notinchlamydia_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                    issueseq))
count(iuspresc_notinpidct_pregmin) 

## Drop events that fall into gc pregmin durations
gonorrhoea_long <- gonorrhoea_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(gonorrhoea_long)

gonorrhoea_distinct_patid <- gonorrhoea_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,2,1), times = nrow(gonorrhoea_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
gonorrhoea_distinct_patid_2 <- gonorrhoea_distinct_patid %>%
  slice(rep(1:n(), each=2))
gonorrhoea_distinct_patid_2$eventid <- eventid_variable$eventid

gonorrhoea_long_expanded <-  left_join(gonorrhoea_distinct_patid_2, gonorrhoea_long, 
                                       by = c("patid" = "patid", "eventid" = "id"))

gonorrhoea_long <- as.data.frame(gonorrhoea_long)
gonorrhoea_wide <- spread(gonorrhoea_long_expanded, eventid, eventdate)
gonorrhoea_wide <- gonorrhoea_wide %>% rename_with( ~ paste("gonorrhoea", .x, sep = "_"))

iuspresc_gonorrhoea <- left_join(iuspresc_notinpidct_pregmin, gonorrhoea_wide, 
                                   by = c("patid" = "gonorrhoea_patid"))
iuspresc_notingonorrhoea_pregmin <- iuspresc_gonorrhoea %>% 
  mutate(to_drop = ifelse(is.na(gonorrhoea_1), "no",
                          ifelse(eventdate >= gonorrhoea_1 & eventdate <= (gonorrhoea_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(gonorrhoea_2), "no",
                           ifelse(eventdate >= gonorrhoea_2 & eventdate <= (gonorrhoea_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") 


iuspresc_notinpidctgc_pregmin <- dplyr::select(iuspresc_notingonorrhoea_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                       issueseq))
count(iuspresc_notinpidctgc_pregmin)


## Create annual ius presc counts (preg max and min assumption) ---


# create ius cohort file with line for each year that patient is in cohort 
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(ius_cohort_final)))
colnames(year_variable) <- 'eventyear'
ius_annual_cohort <- ius_cohort_final %>%
  slice(rep(1:n(), each=10))
ius_annual_cohort$eventyear <- year_variable$eventyear
ius_annual_cohort <- filter(ius_annual_cohort, eventyear >= year(ius_cohort_entry) & eventyear <= year(ius_cohort_exit))

# Create annual ius presc episodes count 

iuspresc_pregmaxassumption_annual_counts <- iuspresc_notinpidctgc_pregmax %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(iuspresc_n_pregmax = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, iuspresc_n_pregmax) %>%
  distinct()

iuspresc_pregminassumption_annual_counts <- iuspresc_notinpidctgc_pregmin %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(iuspresc_n_pregmin = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, iuspresc_n_pregmin) %>%
  distinct()

# Join annual cohort file to annual ius presc files, change NAs to 0

iuspresc_annual_counts <- ius_annual_cohort %>%
  left_join(iuspresc_pregmaxassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(iuspresc_pregminassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear"))
iuspresc_annual_counts$iuspresc_n_pregmax <- iuspresc_annual_counts$iuspresc_n_pregmax %>% replace_na(0)
iuspresc_annual_counts$iuspresc_n_pregmin <- iuspresc_annual_counts$iuspresc_n_pregmin %>% replace_na(0)

# Add age during event year and age category 

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

iuspresc_annual_counts_age <- iuspresc_annual_counts %>% mutate(age_date= eventyear) 
iuspresc_annual_counts_age$age_date <- as.character(iuspresc_annual_counts_age$age_date)
iuspresc_annual_counts_age$age_date <- paste("01-01-", iuspresc_annual_counts_age$age_date, sep="")
head(iuspresc_annual_counts_age$age_date)
iuspresc_annual_counts_age$age_date <- dmy(iuspresc_annual_counts_age$age_date)
head(iuspresc_annual_counts_age$age_date)
class(iuspresc_annual_counts_age$age_date)
iuspresc_annual_counts_age <- iuspresc_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(iuspresc_annual_counts_age$eventyear_age)
max(iuspresc_annual_counts_age$eventyear_age) 
iuspresc_annual_counts_age_50 <- iuspresc_annual_counts_age %>% filter(eventyear_age == 50)
iuspresc_annual_counts_age_final <- iuspresc_annual_counts_age %>% 
  filter(eventyear_age != 50) %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
iuspresc_annual_counts_age_final$eventyear_agecat <- factor(iuspresc_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                              labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Final file
iuspresc_annual_counts_final <- iuspresc_annual_counts_age_final %>% dplyr::select(-c(age_date))

## Save and load final file
save(iuspresc_annual_counts_final, file = "cleaned_files/iuspresc_annual_counts_final.Rdata")
load(file = "cleaned_files/iuspresc_annual_counts_final.Rdata")

## Set up data for stratifying by time variant variable (i.e add pdays per year preg max and pregmin, duration of preg/pic/ct/gc removed per year ---

# Add year durations
iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
iuspresc_annual_counts_final_extra$start_eventyear <- as_date(iuspresc_annual_counts_final_extra$start_eventyear)
iuspresc_annual_counts_final_extra$end_eventyear <- as_date(iuspresc_annual_counts_final_extra$end_eventyear)
iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(ius_cohort_entry <= start_eventyear, start_eventyear,ius_cohort_entry))
iuspresc_annual_counts_final_extra$t0_ey <- as_date(iuspresc_annual_counts_final_extra$t0_ey)
iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(ius_cohort_exit >= end_eventyear, end_eventyear,ius_cohort_exit))
iuspresc_annual_counts_final_extra$t1_ey <- as_date(iuspresc_annual_counts_final_extra$t1_ey)
iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) 
iuspresc_annual_counts_final_extra$pdays_ey <- as.numeric(iuspresc_annual_counts_final_extra$pdays_ey)

# Calc duration of PID/CT/GC/Pregn in each year based on preg max/min assumption

x <- preg_episodes_pn4w_valid %>% filter(year(episode_end_max) == year(episode_start_max)) %>% 
  mutate(year = year(episode_end_max), pregduration_max = (episode_end_max - episode_start_max)+1) %>% 
  dplyr::select(patid, year, pregduration_max)
x$pregduration_max <- as.numeric(x$pregduration_max)

y <- preg_episodes_pn4w_valid %>% filter(year(episode_end_max) != year(episode_start_max)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_max = ifelse(start_end == "episode_start_max", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_max", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_max)

preg_annual_durations_max <- rbind(x,y) 
preg_annual_durations_max_grouped <- preg_annual_durations_max %>% group_by(patid, year) %>% tally(pregduration_max) 
preg_annual_durations_max_grouped <- rename(preg_annual_durations_max_grouped, pregduration_max = n)

x <- preg_episodes_pn4w_valid %>% filter(year(episode_end_min) == year(episode_start_min)) %>% 
  mutate(year = year(episode_end_min), pregduration_min = (episode_end_min - episode_start_min)+1) %>% 
  dplyr::select(patid, year, pregduration_min)
x$pregduration_min <- as.numeric(x$pregduration_min)

y <- preg_episodes_pn4w_valid %>% filter(year(episode_end_min) != year(episode_start_min)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_min = ifelse(start_end == "episode_start_min", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_min", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_min)

preg_annual_durations_min <- rbind(x,y) 
preg_annual_durations_min_grouped <- preg_annual_durations_min %>% group_by(patid, year) %>% tally(pregduration_min)
preg_annual_durations_min_grouped <- rename(preg_annual_durations_min_grouped, pregduration_min = n)

pid_annual_durations_pregmax <- pid_notinpregmax %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(pidduration_max = 14*n) %>% 
  dplyr::select(patid, year, pidduration_max)

ct_annual_durations_pregmax <- chlamydia_notinpregmax  %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(ctduration_max = 7*n)  %>% 
  dplyr::select(patid, year, ctduration_max)

gc_annual_durations_pregmax <- gonorrhoea_notinpregmax %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(gcduration_max = 14*n)  %>% 
  dplyr::select(patid, year, gcduration_max)

pid_annual_durations_pregmin <- pid_notinpregmin %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(pidduration_min = 14*n)  %>% 
  dplyr::select(patid, year, pidduration_min)

ct_annual_durations_pregmin <- chlamydia_notinpregmin  %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(ctduration_min = 7*n)  %>% 
  dplyr::select(patid, year, ctduration_min)

gc_annual_durations_pregmin <- gonorrhoea_notinpregmin %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(gcduration_min = 14*n)  %>% 
  dplyr::select(patid, year, gcduration_min)

## subtract preg/pid/ct/gc duratins from pdays_ey (max and min)

iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>% 
  left_join(preg_annual_durations_max_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(pid_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(ct_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(gc_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(preg_annual_durations_min_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(pid_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(ct_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(gc_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year"))


iuspresc_annual_counts_final_extra$pregduration_max <- iuspresc_annual_counts_final_extra$pregduration_max %>% replace_na(0)
iuspresc_annual_counts_final_extra$pidduration_max <- iuspresc_annual_counts_final_extra$pidduration_max %>% replace_na(0)
iuspresc_annual_counts_final_extra$ctduration_max <- iuspresc_annual_counts_final_extra$ctduration_max %>% replace_na(0)
iuspresc_annual_counts_final_extra$gcduration_max <- iuspresc_annual_counts_final_extra$gcduration_max %>% replace_na(0)
iuspresc_annual_counts_final_extra$pregduration_min <- iuspresc_annual_counts_final_extra$pregduration_min %>% replace_na(0)
iuspresc_annual_counts_final_extra$pidduration_min <- iuspresc_annual_counts_final_extra$pidduration_min %>% replace_na(0)
iuspresc_annual_counts_final_extra$ctduration_min <- iuspresc_annual_counts_final_extra$ctduration_min %>% replace_na(0)
iuspresc_annual_counts_final_extra$gcduration_min <- iuspresc_annual_counts_final_extra$gcduration_min %>% replace_na(0)

iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>% 
  mutate(pdays_ey_pregmax = pdays_ey-(pregduration_max + pidduration_max + ctduration_max + gcduration_max)) %>% 
  mutate(pdays_ey_pregmin = pdays_ey-(pregduration_min + pidduration_min + ctduration_min + gcduration_min)) %>% 
  mutate(pyears_ey_pregmax=pdays_ey_pregmax/365.25) %>% 
  mutate(pyears_ey_pregmin=pdays_ey_pregmin/365.25)


## Save & load annual counts final extra (for glms) ---

iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>%dplyr::select(patid, eventyear, pracid, prac_region, dob, yob, patimd, pracimd, imd, 
                                                                                              migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                              data_start, data_end, ius_cohort_entry, ius_cohort_exit, ius_pdays_max, 
                                                                                              ius_pdays_min, ius_pyears_max, ius_pyears_min, eventyear_age, eventyear_agecat, 
                                                                                              iuspresc_n_pregmax, iuspresc_n_pregmin, pdays_ey_pregmax, pdays_ey_pregmin, 
                                                                                              pyears_ey_pregmax, pyears_ey_pregmin)





save(iuspresc_annual_counts_final_extra, file = "cleaned_files/iuspresc_annual_counts_final_extra.Rdata")
load(file = "cleaned_files/iuspresc_annual_counts_final_extra.Rdata")

## SDI cohort creation ------------------------------------------------------------------------

## Adjust for conditions that affect cohort entry/exit ---

# dplyr::select incident breast ca events
count(breast_ca_clean) == count(distinct(breast_ca_clean, patid)) 
incidentevents_breast_ca <- breast_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_breast_ca <- as.data.frame(incidentevents_breast_ca)
count(incidentevents_breast_ca) == count(distinct(breast_ca_clean, patid)) 

# dplyr::select incident cirrhosis + dcld events, combine and then dplyr::select incident event for those with in cirrhosis event
# to end up with incident severe decompensated cirrhosis event
count(dcld_cirrhosis_clean) == count(distinct(dcld_cirrhosis_clean, patid)) 
incidentevents_dcld_cirrhosis <- dcld_cirrhosis_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_cirrhosis <- as.data.frame(incidentevents_dcld_cirrhosis)
count(incidentevents_dcld_cirrhosis) == count(distinct(dcld_cirrhosis_clean, patid)) 

count(dcld_ascites_clean) == count(distinct(dcld_ascites_clean, patid)) 
incidentevents_dcld_ascites <- dcld_ascites_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_ascites <- as.data.frame(incidentevents_dcld_ascites)
count(incidentevents_dcld_ascites) == count(distinct(dcld_ascites_clean, patid)) 

count(dcld_jaundice_clean) == count(distinct(dcld_jaundice_clean, patid))
incidentevents_dcld_jaundice <- dcld_jaundice_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_jaundice <- as.data.frame(incidentevents_dcld_jaundice)
count(incidentevents_dcld_jaundice) == count(distinct(dcld_jaundice_clean, patid)) 

count(dcld_varices_clean) == count(distinct(dcld_varices_clean, patid)) 
incidentevents_dcld_varices <- dcld_varices_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_varices <- as.data.frame(incidentevents_dcld_varices)
count(incidentevents_dcld_varices) == count(distinct(dcld_varices_clean, patid))  

dcld_all <- full_join(incidentevents_dcld_cirrhosis, incidentevents_dcld_jaundice, 
                      by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_dcld_varices, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_dcld_all <- dcld_all %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_dcld_all <- as.data.frame(incident_dcld_all)
count(incident_dcld_all) == count(distinct(incident_dcld_all)) 
incidentevents_dcld_all_cirrhosis <- incident_dcld_all %>% filter(incident_dcld_all$patid %in% incidentevents_dcld_cirrhosis$patid)

# dplyr::select incident hep adenoma 
count(hep_adenoma_clean) == count(distinct(hep_adenoma_clean, patid)) 
incidentevents_hep_adenoma <- hep_adenoma_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_adenoma <- as.data.frame(incidentevents_hep_adenoma)
count(incidentevents_hep_adenoma) == count(distinct(hep_adenoma_clean, patid)) 

# dplyr::select incident hep ca
count(hep_ca_clean) == count(distinct(hep_ca_clean, patid)) 
incidentevents_hep_ca <- hep_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_ca <- as.data.frame(incidentevents_hep_ca)
count(incidentevents_hep_ca) == count(distinct(hep_ca_clean, patid)) 

# Combine patients with incident condition affecting cohort entry/exit and dplyr::select earliest date of these
conditions <- full_join(incidentevents_breast_ca, incidentevents_dcld_cirrhosis, 
            by = c("patid" = "patid", "eventdate" = "eventdate"))%>% 
  full_join(incidentevents_dcld_all_cirrhosis, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_hep_adenoma, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_hep_ca, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_conditions <- conditions %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_conditions <- as.data.frame(incident_conditions)
count(incident_conditions) == count(distinct(incident_conditions)) 
incident_conditions <- rename(incident_conditions, conditions_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
sdi_cohort <- left_join(contraception_cohort_entry_exit_pdays_pyears, incident_conditions, 
                        by = c("patid" = "patid"))
sdi_cohort_entry <-  sdi_cohort %>% 
  mutate(sdi_cohort_entry = ifelse(is.na(conditions_eventdate), contr_cohort_entry,
                                   ifelse(conditions_eventdate <= contr_cohort_entry, NA, contr_cohort_entry)))
summary(is.na(sdi_cohort_entry$sdi_cohort_entry))
sdi_cohort_entry_eligible <- sdi_cohort_entry %>% filter(!is.na(sdi_cohort_entry))

# Amend cohort exit dates
sdi_cohort_entry_exit <- sdi_cohort_entry_eligible %>% 
  mutate(sdi_cohort_exit = ifelse(is.na(conditions_eventdate), contr_cohort_exit,
                                  ifelse(conditions_eventdate < contr_cohort_exit, conditions_eventdate, contr_cohort_exit)))
summary(is.na(sdi_cohort_entry_exit$contr_cohort_exit)) 

# Recalculate pdays and pyears based on those eligible after adjusting for cervical.ca.endomca/malign gtd
sdi_cohort_entry_exit_pdays <- sdi_cohort_entry_exit %>%
  mutate(sdi_pdays = (sdi_cohort_exit-sdi_cohort_entry)+1)  
sdi_cohort_entry_exit_pdays$pdays <- as.numeric(sdi_cohort_entry_exit_pdays$pdays)

# convert to date format
sdi_cohort_entry_exit_pdays$sdi_cohort_entry <-  as_date(sdi_cohort_entry_exit_pdays$sdi_cohort_entry) 
sdi_cohort_entry_exit_pdays$sdi_cohort_exit <-  as_date(sdi_cohort_entry_exit_pdays$sdi_cohort_exit) 


## Adjust for pregnancy intervals based on pregnancy outcomes (max and min pdays and pyears) ---

# drop episodes that are not in cohort entry/exit period for patient 
preg_episodes_cohort <- left_join(preg_episodes, sdi_cohort_entry_exit_pdays, by = c("patid" = "patid"))
preg_episodes_valid <-preg_episodes_cohort %>% filter(eventdate >= sdi_cohort_entry & eventdate <= sdi_cohort_exit) %>% 
  dplyr::select(c(patid, eventdate, pregcat, episode_start_max, episode_end_max, episode_start_min, episode_end_min))

# calculate episode durations 
preg_episodes_duration_max <- preg_episodes_valid  %>% mutate(duration_max =(episode_end_max-episode_start_max)+1) 
preg_episodes_duration_max_min <- preg_episodes_duration_max %>%  mutate(duration_min = (episode_end_min-episode_start_min)+1)
preg_episodes_total_max <- preg_episodes_duration_max_min %>% group_by(patid) %>% tally(duration_max)
preg_episodes_total_min <- preg_episodes_duration_max_min %>% group_by(patid) %>% tally(duration_min)
preg_episodes_total_max <- rename(preg_episodes_total_max, preg_total_max = n)
preg_episodes_total_min <- rename(preg_episodes_total_min, preg_total_min = n)

# recalculate pdays and pyears (max and min based on max and min preg durations)
sdi_cohort_pregnancy_episodes <- left_join(sdi_cohort_entry_exit_pdays, preg_episodes_total_max,
                                                  by = c("patid" = "patid")) %>%
  left_join(preg_episodes_total_min, by = c("patid" = "patid"))
sdi_cohort_pregnancy_episodes$preg_total_max <- sdi_cohort_pregnancy_episodes$preg_total_max %>% replace_na(0)
sdi_cohort_pregnancy_episodes$preg_total_min <- sdi_cohort_pregnancy_episodes$preg_total_min %>% replace_na(0)
sdi_cohort_pregnancy_episodes_pdays_pyears <- sdi_cohort_pregnancy_episodes %>% 
  mutate(sdi_pdays_max = (sdi_pdays-preg_total_max)) %>%
  mutate(sdi_pdays_min = (sdi_pdays-preg_total_min)) %>%
  mutate(sdi_pyears_max = (sdi_pdays_max/365.25)) %>% 
  mutate(sdi_pyears_min = (sdi_pdays_min/365.25))
sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pyears_max <- as.numeric(sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pyears_max)
sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pyears_min <- as.numeric(sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pyears_min)
sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pdays_max <- as.numeric(sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pdays_max)
sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pdays_min <- as.numeric(sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pdays_min)


## Next step not applicable to SDI 
## Adjust for conditions that have short intervals that affect pdays/pyears ---

# dplyr::select final variables for cohort --- 
sdi_cohort_final <- dplyr::select(sdi_cohort_pregnancy_episodes_pdays_pyears, c(patid, pracid, prac_region, 
                                                                              dob, yob, patimd, pracimd, imd, migrant_status, migcertainty, ethnicat, ethnicat6, data_start, data_end,
                                                                              sdi_cohort_entry, sdi_cohort_exit, sdi_pdays_max,sdi_pyears_max, sdi_pdays_min, sdi_pyears_min )) 

## Save & load SDI cohort  .Rdata files --------------------------------------------------------
save(sdi_cohort_final, file = "filepath" )
load(file = "filepath") 


## SDI prescribing episodes --------------------------------------------------------------------

sdi_presc <- contr_presc_clean %>% filter(contrpresccat == "Implant") 

## Change prescribing events that are same category on same day, to one event ---

n_distinct(sdi_presc) == count(distinct(sdi_presc, patid, eventdate, contrpresccat,  .keep_all = TRUE))
sdipresc_oneeventperdate <- sdi_presc %>% distinct(patid, eventdate, contrpresccat,  .keep_all = TRUE)
count(sdipresc_oneeventperdate) 

## Drop events that are not in sdi cohort entry/exit time ---

sdipresc_indates <- left_join(sdipresc_oneeventperdate, sdi_cohort_final,
                              by = c("patid" = "patid"))
sdipresc_indates_2 <- sdipresc_indates %>% 
  filter(eventdate >= sdi_cohort_entry & eventdate <= sdi_cohort_exit) 
count(sdipresc_indates_2)


## Drop events that fall into pregnancy durations  ---

# Assign row ids to preg outcome episodes & convert to wide dataset
preg_episodes_valid_vars <- dplyr::select(preg_episodes_valid, 
                                               c(patid,eventdate,episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_valid_eventids <- preg_episodes_valid_vars %>% group_by(patid) %>% mutate(eventid = row_number())
summary(preg_episodes_valid_eventids) 
preg_episodes_valid_wide <- pivot_wider(preg_episodes_valid_eventids, names_from = eventid, 
                                             values_from = c(eventdate, episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_valid_wide <- as.data.frame(preg_episodes_valid_wide)
count(preg_episodes_valid_wide) 
count(distinct(preg_episodes_valid_eventids))



# Join wide pregnancy to cu iud prescriptions 
sdipresc_preg <- left_join(sdipresc_indates_2, preg_episodes_valid_wide , 
                               by = c("patid" = "patid"))

# Drop cu iud presc events that fall into pregn max intervals
sdipresc_notinpreg_max <- sdipresc_preg %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_max_1 & eventdate <= episode_end_max_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_max_2 & eventdate <= episode_end_max_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_max_3 & eventdate <= episode_end_max_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_max_4 & eventdate <= episode_end_max_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_max_5 & eventdate <= episode_end_max_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_max_6 & eventdate <= episode_end_max_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_max_7 & eventdate <= episode_end_max_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_max_8 & eventdate <= episode_end_max_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_max_9 & eventdate <= episode_end_max_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_max_10 & eventdate <= episode_end_max_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_max_11 & eventdate <= episode_end_max_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no") 

# Drop cu iud presc events that fall into pregn min intervals
sdipresc_notinpreg_min <- sdipresc_preg %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_min_1 & eventdate <= episode_end_min_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_min_2 & eventdate <= episode_end_min_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_min_3 & eventdate <= episode_end_min_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_min_4 & eventdate <= episode_end_min_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_min_5 & eventdate <= episode_end_min_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_min_6 & eventdate <= episode_end_min_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_min_7 & eventdate <= episode_end_min_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_min_8 & eventdate <= episode_end_min_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_min_9 & eventdate <= episode_end_min_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_min_10 & eventdate <= episode_end_min_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_min_11 & eventdate <= episode_end_min_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no")


# Final sdi presc files with pregn max duration assumption and pregn min duration assumption
sdipresc_pregmaxassumption <- dplyr::select(sdipresc_notinpreg_max, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                          issueseq))
sdipresc_pregminassumption <- dplyr::select(sdipresc_notinpreg_min, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                          issueseq))
count(sdipresc_pregmaxassumption) 
count(sdipresc_pregminassumption) 


## Drop events that fall into conditions with short intervals based on preg max/min assumption ---
# step not applicable to SDI

## Create annual sdi presc counts (preg max and min assumption) ---


# create sdi cohort file with line for each year that patient is in cohort 
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(sdi_cohort_final)))
colnames(year_variable) <- 'eventyear'
sdi_annual_cohort <- sdi_cohort_final %>%
  slice(rep(1:n(), each=10))
sdi_annual_cohort$eventyear <- year_variable$eventyear
sdi_annual_cohort <- filter(sdi_annual_cohort, eventyear >= year(sdi_cohort_entry) & eventyear <= year(sdi_cohort_exit))

# Create annual sdi presc episodes count 

sdipresc_pregmaxassumption_annual_counts <- sdipresc_pregmaxassumption %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(sdipresc_n_pregmax = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, sdipresc_n_pregmax) %>%
  distinct()

sdipresc_pregminassumption_annual_counts <- sdipresc_pregminassumption %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(sdipresc_n_pregmin = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, sdipresc_n_pregmin) %>%
  distinct()

# Join annual cohort file to annual sdi presc files, change NAs to 0

sdipresc_annual_counts <- sdi_annual_cohort %>%
  left_join(sdipresc_pregmaxassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(sdipresc_pregminassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear"))
sdipresc_annual_counts$sdipresc_n_pregmax <- sdipresc_annual_counts$sdipresc_n_pregmax %>% replace_na(0)
sdipresc_annual_counts$sdipresc_n_pregmin <- sdipresc_annual_counts$sdipresc_n_pregmin %>% replace_na(0)

# Add age during event year and age category 

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

sdipresc_annual_counts_age <- sdipresc_annual_counts %>% mutate(age_date= eventyear) 
sdipresc_annual_counts_age$age_date <- as.character(sdipresc_annual_counts_age$age_date)
sdipresc_annual_counts_age$age_date <- paste("01-01-", sdipresc_annual_counts_age$age_date, sep="")
head(sdipresc_annual_counts_age$age_date)
sdipresc_annual_counts_age$age_date <- dmy(sdipresc_annual_counts_age$age_date)
head(sdipresc_annual_counts_age$age_date)
class(sdipresc_annual_counts_age$age_date)
sdipresc_annual_counts_age <- sdipresc_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(sdipresc_annual_counts_age$eventyear_age)
max(sdipresc_annual_counts_age$eventyear_age)  
sdipresc_annual_counts_age_50 <- sdipresc_annual_counts_age %>% filter(eventyear_age == 50)
sdipresc_annual_counts_age_final <- sdipresc_annual_counts_age %>% 
  filter(eventyear_age != 50) %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
sdipresc_annual_counts_age_final$eventyear_agecat <- factor(sdipresc_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                            labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Final file
sdipresc_annual_counts_final <- sdipresc_annual_counts_age_final %>% dplyr::select(-c(age_date))

## Save and load final file
save(sdipresc_annual_counts_final, file = "filepath")
load(file = "filepath")


## Set up data for stratifying by time variant variable (i.e add pdays per year max and min with  preg max and pregmin removed per year) ---

# Add year durations
sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
sdipresc_annual_counts_final_extra$start_eventyear <- as_date(sdipresc_annual_counts_final_extra$start_eventyear)
sdipresc_annual_counts_final_extra$end_eventyear <- as_date(sdipresc_annual_counts_final_extra$end_eventyear)
sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(sdi_cohort_entry <= start_eventyear, start_eventyear,sdi_cohort_entry))
sdipresc_annual_counts_final_extra$t0_ey <- as_date(sdipresc_annual_counts_final_extra$t0_ey)
sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(sdi_cohort_exit >= end_eventyear, end_eventyear,sdi_cohort_exit))
sdipresc_annual_counts_final_extra$t1_ey <- as_date(sdipresc_annual_counts_final_extra$t1_ey)
sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) 
sdipresc_annual_counts_final_extra$pdays_ey <- as.numeric(sdipresc_annual_counts_final_extra$pdays_ey)

# Calc duration of Pregn in each year based on preg max/min assumption

x <- preg_episodes_valid %>% filter(year(episode_end_max) == year(episode_start_max)) %>% 
  mutate(year = year(episode_end_max), pregduration_max = (episode_end_max - episode_start_max)+1) %>% 
  dplyr::select(patid, year, pregduration_max)
x$pregduration_max <- as.numeric(x$pregduration_max)

y <- preg_episodes_valid %>% filter(year(episode_end_max) != year(episode_start_max)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_max = ifelse(start_end == "episode_start_max", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_max", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_max)

preg_annual_durations_max <- rbind(x,y) 
preg_annual_durations_max_grouped <- preg_annual_durations_max %>% group_by(patid, year) %>% tally(pregduration_max) 
preg_annual_durations_max_grouped <- rename(preg_annual_durations_max_grouped, pregduration_max = n)

x <- preg_episodes_valid %>% filter(year(episode_end_min) == year(episode_start_min)) %>% 
  mutate(year = year(episode_end_min), pregduration_min = (episode_end_min - episode_start_min)+1) %>% 
  dplyr::select(patid, year, pregduration_min)
x$pregduration_min <- as.numeric(x$pregduration_min)

y <- preg_episodes_valid %>% filter(year(episode_end_min) != year(episode_start_min)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_min = ifelse(start_end == "episode_start_min", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_min", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_min)

preg_annual_durations_min <- rbind(x,y)
preg_annual_durations_min_grouped <- preg_annual_durations_min %>% group_by(patid, year) %>% tally(pregduration_min) 
preg_annual_durations_min_grouped <- rename(preg_annual_durations_min_grouped, pregduration_min = n)

## subtract preg duratins from pdays_ey (max and min)

sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>% 
  left_join(preg_annual_durations_max_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>%  
  left_join(preg_annual_durations_min_grouped, by = c("patid" = "patid", "eventyear" = "year")) 


sdipresc_annual_counts_final_extra$pregduration_max <- sdipresc_annual_counts_final_extra$pregduration_max %>% replace_na(0)
sdipresc_annual_counts_final_extra$pregduration_min <- sdipresc_annual_counts_final_extra$pregduration_min %>% replace_na(0)

sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>% 
  mutate(pdays_ey_pregmax = pdays_ey-(pregduration_max)) %>% 
  mutate(pdays_ey_pregmin = pdays_ey-(pregduration_min)) %>% 
  mutate(pyears_ey_pregmax=pdays_ey_pregmax/365.25) %>% 
  mutate(pyears_ey_pregmin=pdays_ey_pregmin/365.25)


## Save & load annual counts final extra (for glms) ---

sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>% dplyr::select(patid, eventyear, pracid, prac_region, dob, yob, patimd, pracimd, imd, 
                                                                                              migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                              data_start, data_end, sdi_cohort_entry, sdi_cohort_exit, sdi_pdays_max, 
                                                                                              sdi_pdays_min, sdi_pyears_max, sdi_pyears_min, eventyear_age, eventyear_agecat, 
                                                                                              sdipresc_n_pregmax, sdipresc_n_pregmin, pdays_ey_pregmax, pdays_ey_pregmin, 
                                                                                              pyears_ey_pregmax, pyears_ey_pregmin)





save(sdipresc_annual_counts_final_extra, file = "filepath")
load(file = "filepath")

## POI cohort creation ----------------------------------------------------------------------------

## Adjust for conditions that affect cohort entry/exit ---

# dplyr::select incident breast ca events
count(breast_ca_clean) == count(distinct(breast_ca_clean, patid))
incidentevents_breast_ca <- breast_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_breast_ca <- as.data.frame(incidentevents_breast_ca)
count(incidentevents_breast_ca) == count(distinct(breast_ca_clean, patid)) 

# dplyr::select incident vascular dx events
count(vasculardx_clean) == count(distinct(vasculardx_clean, patid)) 
incidentevents_vasculardx <- vasculardx_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_vasculardx <- as.data.frame(incidentevents_vasculardx)
count(incidentevents_vasculardx) == count(distinct(vasculardx_clean, patid))

# dplyr::select incident IHD chd events
count(IHD_chd_clean) == count(distinct(IHD_chd_clean, patid)) 
incidentevents_IHD_chd <- IHD_chd_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_IHD_chd <- as.data.frame(incidentevents_IHD_chd)
count(incidentevents_IHD_chd) == count(distinct(IHD_chd_clean, patid)) 

# dplyr::select incident IHD epicardial events
count(IHD_epicardial_clean) == count(distinct(IHD_epicardial_clean, patid)) 
incidentevents_IHD_epicardial <- IHD_epicardial_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_IHD_epicardial <- as.data.frame(incidentevents_IHD_epicardial)
count(incidentevents_IHD_epicardial) == count(distinct(IHD_epicardial_clean, patid)) 

# dplyr::select stroke ich events
count(stroke_ich_clean) == count(distinct(stroke_ich_clean, patid)) 
incidentevents_stroke_ich <- stroke_ich_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_stroke_ich <- as.data.frame(incidentevents_stroke_ich)
count(incidentevents_stroke_ich) == count(distinct(stroke_ich_clean, patid))  

# dplyr::select stroke isch events
count(stroke_isch_clean) == count(distinct(stroke_isch_clean, patid)) 
incidentevents_stroke_isch <- stroke_isch_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_stroke_isch <- as.data.frame(incidentevents_stroke_isch)
count(incidentevents_stroke_isch) == count(distinct(stroke_isch_clean, patid)) 

# dplyr::select stroke nos events
count(stroke_nos_clean) == count(distinct(stroke_nos_clean, patid)) 
incidentevents_stroke_nos <- stroke_nos_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_stroke_nos <- as.data.frame(incidentevents_stroke_nos)
count(incidentevents_stroke_nos) == count(distinct(stroke_nos_clean, patid))  

# dplyr::select stroke nos events
count(stroke_TIA_clean) == count(distinct(stroke_TIA_clean, patid)) 
incidentevents_stroke_TIA <- stroke_TIA_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_stroke_TIA <- as.data.frame(incidentevents_stroke_TIA)
count(incidentevents_stroke_TIA) == count(distinct(stroke_TIA_clean, patid)) 

# dplyr::select incident hep adenoma 
count(hep_adenoma_clean) == count(distinct(hep_adenoma_clean, patid)) 
incidentevents_hep_adenoma <- hep_adenoma_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_adenoma <- as.data.frame(incidentevents_hep_adenoma)
count(incidentevents_hep_adenoma) == count(distinct(hep_adenoma_clean, patid)) 

# dplyr::select incident hep ca
count(hep_ca_clean) == count(distinct(hep_ca_clean, patid)) 
incidentevents_hep_ca <- hep_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_ca <- as.data.frame(incidentevents_hep_ca)
count(incidentevents_hep_ca) == count(distinct(hep_ca_clean, patid)) 

# Combine patients with incident condition affecting cohort entry/exit and dplyr::select earliest date of these
conditions <- full_join(incidentevents_breast_ca, incidentevents_vasculardx, 
                        by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_IHD_chd, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_IHD_epicardial, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_stroke_ich, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_stroke_isch, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_stroke_nos, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_stroke_TIA, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_hep_adenoma, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_hep_ca, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 

incident_conditions <- conditions %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_conditions <- as.data.frame(incident_conditions)
count(incident_conditions) == count(distinct(incident_conditions))
incident_conditions <- rename(incident_conditions, conditions_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
poi_cohort <- left_join(contraception_cohort_entry_exit_pdays_pyears, incident_conditions, 
                        by = c("patid" = "patid"))
poi_cohort_entry <-  poi_cohort %>% 
  mutate(poi_cohort_entry = ifelse(is.na(conditions_eventdate), contr_cohort_entry,
                                   ifelse(conditions_eventdate <= contr_cohort_entry, NA, contr_cohort_entry)))
summary(is.na(poi_cohort_entry$poi_cohort_entry)) 
poi_cohort_entry_eligible <- poi_cohort_entry %>% filter(!is.na(poi_cohort_entry))

# Amend cohort exit dates
poi_cohort_entry_exit <- poi_cohort_entry_eligible %>% 
  mutate(poi_cohort_exit = ifelse(is.na(conditions_eventdate), contr_cohort_exit,
                                  ifelse(conditions_eventdate < contr_cohort_exit, conditions_eventdate, contr_cohort_exit)))
summary(is.na(poi_cohort_entry_exit$contr_cohort_exit)) 

# Recalculate pdays and pyears based on those eligible after adjusting for cervical.ca.endomca/malign gtd
poi_cohort_entry_exit_pdays <- poi_cohort_entry_exit %>%
  mutate(poi_pdays = (poi_cohort_exit-poi_cohort_entry)+1) 
poi_cohort_entry_exit_pdays$pdays <- as.numeric(poi_cohort_entry_exit_pdays$pdays)

# convert to date format
poi_cohort_entry_exit_pdays$poi_cohort_entry <-  as_date(poi_cohort_entry_exit_pdays$poi_cohort_entry) 
poi_cohort_entry_exit_pdays$poi_cohort_exit <-  as_date(poi_cohort_entry_exit_pdays$poi_cohort_exit) 


## Adjust for pregnancy intervals based on pregnancy outcomes (max and min pdays and pyears) ---

# drop episodes that are not in cohort entry/exit period for patient 
preg_episodes_cohort <- left_join(preg_episodes, poi_cohort_entry_exit_pdays, by = c("patid" = "patid"))
preg_episodes_valid <-preg_episodes_cohort %>% filter(eventdate >= poi_cohort_entry & eventdate <= poi_cohort_exit) %>% 
  dplyr::select(c(patid, eventdate, pregcat, episode_start_max, episode_end_max, episode_start_min, episode_end_min))

# calculate episode durations 
preg_episodes_duration_max <- preg_episodes_valid  %>% mutate(duration_max =(episode_end_max-episode_start_max)+1) 
preg_episodes_duration_max_min <- preg_episodes_duration_max %>%  mutate(duration_min = (episode_end_min-episode_start_min)+1)
preg_episodes_total_max <- preg_episodes_duration_max_min %>% group_by(patid) %>% tally(duration_max)
preg_episodes_total_min <- preg_episodes_duration_max_min %>% group_by(patid) %>% tally(duration_min)
preg_episodes_total_max <- rename(preg_episodes_total_max, preg_total_max = n)
preg_episodes_total_min <- rename(preg_episodes_total_min, preg_total_min = n)

# recalculate pdays and pyears (max and min based on max and min preg durations)
poi_cohort_pregnancy_episodes <- left_join(poi_cohort_entry_exit_pdays, preg_episodes_total_max,
                                           by = c("patid" = "patid")) %>%
  left_join(preg_episodes_total_min, by = c("patid" = "patid"))
poi_cohort_pregnancy_episodes$preg_total_max <- poi_cohort_pregnancy_episodes$preg_total_max %>% replace_na(0)
poi_cohort_pregnancy_episodes$preg_total_min <- poi_cohort_pregnancy_episodes$preg_total_min %>% replace_na(0)
poi_cohort_pregnancy_episodes_pdays_pyears <- poi_cohort_pregnancy_episodes %>% 
  mutate(poi_pdays_max = (poi_pdays-preg_total_max)) %>%
  mutate(poi_pdays_min = (poi_pdays-preg_total_min)) %>%
  mutate(poi_pyears_max = (poi_pdays_max/365.25)) %>% 
  mutate(poi_pyears_min = (poi_pdays_min/365.25))
poi_cohort_pregnancy_episodes_pdays_pyears$poi_pyears_max <- as.numeric(poi_cohort_pregnancy_episodes_pdays_pyears$poi_pyears_max)
poi_cohort_pregnancy_episodes_pdays_pyears$poi_pyears_min <- as.numeric(poi_cohort_pregnancy_episodes_pdays_pyears$poi_pyears_min)
poi_cohort_pregnancy_episodes_pdays_pyears$poi_pdays_max <- as.numeric(poi_cohort_pregnancy_episodes_pdays_pyears$poi_pdays_max)
poi_cohort_pregnancy_episodes_pdays_pyears$poi_pdays_min <- as.numeric(poi_cohort_pregnancy_episodes_pdays_pyears$poi_pdays_min)


## Next step not applicable to POI
## Adjust for conditions that have short intervals that affect pdays/pyears ---


# dplyr::select final variables for cohort --- 
poi_cohort_final <- dplyr::select(poi_cohort_pregnancy_episodes_pdays_pyears, c(patid, pracid, prac_region,
                                                                 dob, yob, patimd, pracimd, imd, migrant_status, migcertainty, ethnicat, ethnicat6, data_start, data_end,
                                                                 poi_cohort_entry, poi_cohort_exit, poi_pdays_max, poi_pdays_min, poi_pyears_max, poi_pyears_min )) 


## Save & load POI cohort  .Rdata files --------------------------------------------------------
save(poi_cohort_final, file = "filepath") 
load(file = "filepath") 

## POI prescribing episodes -----------------------------------------------------------------------

poi_presc <- contr_presc_clean %>% filter(contrpresccat == "Progesterone-only injectable contraceptive") 


## Change prescribing events that are same category on same day, to one event ---

n_distinct(poi_presc) == count(distinct(poi_presc, patid, eventdate, contrpresccat,  .keep_all = TRUE)) 
poipresc_oneeventperdate <- poi_presc %>% distinct(patid, eventdate, contrpresccat,  .keep_all = TRUE)
count(poipresc_oneeventperdate) 

## Drop events that are not in poi cohort entry/exit time ---

poipresc_indates <- left_join(poipresc_oneeventperdate, poi_cohort_final,
                              by = c("patid" = "patid"))
poipresc_indates_2 <- poipresc_indates %>% 
  filter(eventdate >= poi_cohort_entry & eventdate <= poi_cohort_exit) 
count(poipresc_indates_2)


## Drop events that fall into pregnancy durations  ---

# Assign row ids to preg outcome episodes & convert to wide dataset
preg_episodes_valid_vars <- dplyr::select(preg_episodes_valid, 
                                          c(patid,eventdate,episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_valid_eventids <- preg_episodes_valid_vars %>% group_by(patid) %>% mutate(eventid = row_number())
summary(preg_episodes_valid_eventids) 
preg_episodes_valid_wide <- pivot_wider(preg_episodes_valid_eventids, names_from = eventid, 
                                        values_from = c(eventdate, episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_valid_wide <- as.data.frame(preg_episodes_valid_wide)
count(preg_episodes_valid_wide)
count(distinct(preg_episodes_valid_eventids)) 



# Join wide pregnancy to cu iud prescriptions 
poipresc_preg <- left_join(poipresc_indates_2, preg_episodes_valid_wide , 
                           by = c("patid" = "patid"))

# Drop cu iud presc events that fall into pregn max intervals
poipresc_notinpreg_max <- poipresc_preg %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_max_1 & eventdate <= episode_end_max_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_max_2 & eventdate <= episode_end_max_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_max_3 & eventdate <= episode_end_max_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_max_4 & eventdate <= episode_end_max_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_max_5 & eventdate <= episode_end_max_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_max_6 & eventdate <= episode_end_max_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_max_7 & eventdate <= episode_end_max_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_max_8 & eventdate <= episode_end_max_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_max_9 & eventdate <= episode_end_max_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_max_10 & eventdate <= episode_end_max_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_max_11 & eventdate <= episode_end_max_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no") 

# Drop cu iud presc events that fall into pregn min intervals
poipresc_notinpreg_min <- poipresc_preg %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_min_1 & eventdate <= episode_end_min_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_min_2 & eventdate <= episode_end_min_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_min_3 & eventdate <= episode_end_min_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_min_4 & eventdate <= episode_end_min_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_min_5 & eventdate <= episode_end_min_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_min_6 & eventdate <= episode_end_min_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_min_7 & eventdate <= episode_end_min_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_min_8 & eventdate <= episode_end_min_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_min_9 & eventdate <= episode_end_min_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_min_10 & eventdate <= episode_end_min_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_min_11 & eventdate <= episode_end_min_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no")


# Final poi presc files with pregn max duration assumption and pregn min duration assumption
poipresc_pregmaxassumption <- dplyr::select(poipresc_notinpreg_max, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                      issueseq))
poipresc_pregminassumption <- dplyr::select(poipresc_notinpreg_min, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                      issueseq))
count(poipresc_pregmaxassumption) 
count(poipresc_pregminassumption)


## Drop events that fall into conditions with short intervals based on preg max/min assumption ---
# step not applicable to poi

## Create annual poi presc counts (preg max and min assumption) ---


# create poi cohort file with line for each year that patient is in cohort 
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(poi_cohort_final)))
colnames(year_variable) <- 'eventyear'
poi_annual_cohort <- poi_cohort_final %>%
  slice(rep(1:n(), each=10))
poi_annual_cohort$eventyear <- year_variable$eventyear
poi_annual_cohort <- filter(poi_annual_cohort, eventyear >= year(poi_cohort_entry) & eventyear <= year(poi_cohort_exit))

# Create annual poi presc episodes count 

poipresc_pregmaxassumption_annual_counts <- poipresc_pregmaxassumption %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(poipresc_n_pregmax = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, poipresc_n_pregmax) %>%
  distinct()

poipresc_pregminassumption_annual_counts <- poipresc_pregminassumption %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(poipresc_n_pregmin = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, poipresc_n_pregmin) %>%
  distinct()

# Join annual cohort file to annual poi presc files, change NAs to 0

poipresc_annual_counts <- poi_annual_cohort %>%
  left_join(poipresc_pregmaxassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(poipresc_pregminassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear"))
poipresc_annual_counts$poipresc_n_pregmax <- poipresc_annual_counts$poipresc_n_pregmax %>% replace_na(0)
poipresc_annual_counts$poipresc_n_pregmin <- poipresc_annual_counts$poipresc_n_pregmin %>% replace_na(0)

# Add age during event year and age category 

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

poipresc_annual_counts_age <- poipresc_annual_counts %>% mutate(age_date= eventyear) 
poipresc_annual_counts_age$age_date <- as.character(poipresc_annual_counts_age$age_date)
poipresc_annual_counts_age$age_date <- paste("01-01-", poipresc_annual_counts_age$age_date, sep="")
head(poipresc_annual_counts_age$age_date)
poipresc_annual_counts_age$age_date <- dmy(poipresc_annual_counts_age$age_date)
head(poipresc_annual_counts_age$age_date)
class(poipresc_annual_counts_age$age_date)
poipresc_annual_counts_age <- poipresc_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(poipresc_annual_counts_age$eventyear_age)
max(poipresc_annual_counts_age$eventyear_age) 
poipresc_annual_counts_age_50 <- poipresc_annual_counts_age %>% filter(eventyear_age == 50)
poipresc_annual_counts_age_final <- poipresc_annual_counts_age %>% 
  filter(eventyear_age != 50) %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
poipresc_annual_counts_age_final$eventyear_agecat <- factor(poipresc_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                            labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Final file
poipresc_annual_counts_final <- poipresc_annual_counts_age_final %>% dplyr::select(-c(age_date))

## Save and load final file
save(poipresc_annual_counts_final, file = "filepath")
load(file = "filepath")


## Set up data for stratifying by time variant variable (i.e add pdays per year max and min with  preg max and pregmin removed per year) ---

# Add year durations
poipresc_annual_counts_final_extra <- poipresc_annual_counts_final %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
poipresc_annual_counts_final_extra$start_eventyear <- as_date(poipresc_annual_counts_final_extra$start_eventyear)
poipresc_annual_counts_final_extra$end_eventyear <- as_date(poipresc_annual_counts_final_extra$end_eventyear)
poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(poi_cohort_entry <= start_eventyear, start_eventyear,poi_cohort_entry))
poipresc_annual_counts_final_extra$t0_ey <- as_date(poipresc_annual_counts_final_extra$t0_ey)
poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(poi_cohort_exit >= end_eventyear, end_eventyear,poi_cohort_exit))
poipresc_annual_counts_final_extra$t1_ey <- as_date(poipresc_annual_counts_final_extra$t1_ey)
poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) 
poipresc_annual_counts_final_extra$pdays_ey <- as.numeric(poipresc_annual_counts_final_extra$pdays_ey)

# Calc duration of Pregn in each year based on preg max/min assumption

x <- preg_episodes_valid %>% filter(year(episode_end_max) == year(episode_start_max)) %>% 
  mutate(year = year(episode_end_max), pregduration_max = (episode_end_max - episode_start_max)+1) %>% 
  dplyr::select(patid, year, pregduration_max)
x$pregduration_max <- as.numeric(x$pregduration_max)

y <- preg_episodes_valid %>% filter(year(episode_end_max) != year(episode_start_max)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_max = ifelse(start_end == "episode_start_max", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_max", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_max)

preg_annual_durations_max <- rbind(x,y) 
preg_annual_durations_max_grouped <- preg_annual_durations_max %>% group_by(patid, year) %>% tally(pregduration_max)
preg_annual_durations_max_grouped <- rename(preg_annual_durations_max_grouped, pregduration_max = n)

x <- preg_episodes_valid %>% filter(year(episode_end_min) == year(episode_start_min)) %>% 
  mutate(year = year(episode_end_min), pregduration_min = (episode_end_min - episode_start_min)+1) %>% 
  dplyr::select(patid, year, pregduration_min)
x$pregduration_min <- as.numeric(x$pregduration_min)

y <- preg_episodes_valid %>% filter(year(episode_end_min) != year(episode_start_min)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_min = ifelse(start_end == "episode_start_min", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_min", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_min)

preg_annual_durations_min <- rbind(x,y) 
preg_annual_durations_min_grouped <- preg_annual_durations_min %>% group_by(patid, year) %>% tally(pregduration_min) 
preg_annual_durations_min_grouped <- rename(preg_annual_durations_min_grouped, pregduration_min = n)

## subtract preg duratins from pdays_ey (max and min)

poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>% 
  left_join(preg_annual_durations_max_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>%  
  left_join(preg_annual_durations_min_grouped, by = c("patid" = "patid", "eventyear" = "year")) 


poipresc_annual_counts_final_extra$pregduration_max <- poipresc_annual_counts_final_extra$pregduration_max %>% replace_na(0)
poipresc_annual_counts_final_extra$pregduration_min <- poipresc_annual_counts_final_extra$pregduration_min %>% replace_na(0)

poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>% 
  mutate(pdays_ey_pregmax = pdays_ey-(pregduration_max)) %>% 
  mutate(pdays_ey_pregmin = pdays_ey-(pregduration_min)) %>% 
  mutate(pyears_ey_pregmax=pdays_ey_pregmax/365.25) %>% 
  mutate(pyears_ey_pregmin=pdays_ey_pregmin/365.25)


## Save & load annual counts final extra (for glms) ---

poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>% dplyr::select(patid, eventyear, pracid, prac_region, dob, yob, patimd, pracimd, imd, 
                                                                                           migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                           data_start, data_end, poi_cohort_entry, poi_cohort_exit, poi_pdays_max, 
                                                                                           poi_pdays_min, poi_pyears_max, poi_pyears_min, eventyear_age, eventyear_agecat, 
                                                                                           poipresc_n_pregmax, poipresc_n_pregmin, pdays_ey_pregmax, pdays_ey_pregmin, 
                                                                                           pyears_ey_pregmax, pyears_ey_pregmin)





save(poipresc_annual_counts_final_extra, file = "filepath")
load(file = "filepath")


## Add 365D Washout -  contraception cohort  ------------

load(file = "filepath")

## Overall cohort creation ---------------------------------------------


##  cohort dates - ensure not entering cohort if had hysterectomy/female sterilisation before cohort entry date ---

# incident hysterectomy events
count(hysterectomy_clean) == count(distinct(hysterectomy_clean, patid)) 
incidentevents_hysterectomy <- hysterectomy_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hysterectomy <- as.data.frame(incidentevents_hysterectomy)
count(incidentevents_hysterectomy) == count(distinct(hysterectomy_clean, patid)) 

# incident female sterilisation event
femalesteri <- sterilisation_clean %>% filter(stericat == "Female sterilisation")
count(femalesteri) == count(distinct(femalesteri,patid)) 
incidentevents_femalesteri <- femalesteri %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_femalesteri  <- as.data.frame(incidentevents_femalesteri)
count(incidentevents_femalesteri) == count(distinct(femalesteri, patid)) 

# Combine patients with an incident hysterectomy date or female steri date and dplyr::select earliest date of these
hys_steri <- full_join(incidentevents_hysterectomy, incidentevents_femalesteri, 
                       by = c("patid" = "patid", "eventdate" = "eventdate"))
incident_hys_steri <- hys_steri %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_hys_steri <- as.data.frame(incident_hys_steri)
count(incident_hys_steri) == count(distinct(incident_hys_steri)) 
incident_hys_steri <- rename(incident_hys_steri, hys_steri_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
contraception_cohort_washout <- left_join(reproductive_cohort_final_washout, incident_hys_steri, 
                                  by = c("patid" = "patid"))
contraception_cohort_entry_washout <-  contraception_cohort_washout %>% 
  mutate(contr_cohort_entry = ifelse(is.na(hys_steri_eventdate), cohort_entry,
                                     ifelse(hys_steri_eventdate <= cohort_entry, NA, cohort_entry)))
summary(is.na(contraception_cohort_entry_washout$contr_cohort_entry)) 
contraception_cohort_entry_eligible_washout <- contraception_cohort_entry_washout %>% filter(!is.na(contr_cohort_entry))

# Amend cohort exit dates
contraception_cohort_entry_exit_washout <- contraception_cohort_entry_eligible_washout %>% 
  mutate(contr_cohort_exit = ifelse(is.na(hys_steri_eventdate), cohort_exit,
                                    ifelse(hys_steri_eventdate < cohort_exit, hys_steri_eventdate, cohort_exit)))
summary(is.na(contraception_cohort_entry_exit_washout$contr_cohort_exit)) 

# Recalculate pdays and pyears 
contraception_cohort_entry_exit_pdays_pyears_washout <- contraception_cohort_entry_exit_washout %>%
  mutate(contr_pdays = (contr_cohort_exit-contr_cohort_entry)+1)  %>%
  mutate(contr_pyears = contr_pdays/365.25)
contraception_cohort_entry_exit_pdays_pyears_washout$pyears <- as.numeric(contraception_cohort_entry_exit_pdays_pyears_washout$pyears)
contraception_cohort_entry_exit_pdays_pyears_washout$pdays <- as.numeric(contraception_cohort_entry_exit_pdays_pyears_washout$pdays)

# All impacted by pregnancy intervals but different posnatal periods for each contraception so calculated in section separately 

# NB: still eligible for all types of contraception when using another type so person time at risk does not change in relation to this
contraception_cohort_final_washout <- contraception_cohort_entry_exit_pdays_pyears_washout


## Save & load final contraception cohort washout .Rdata file 

save(contraception_cohort_final_washout, file = "filepath") 
load(file = "filepath") 




## Cu IUD - cohort with 365D washout -------------------------------------------------------------------------------------------

load(file = "filepath") 

## Adjust for conditions that affect cohort entry/exit ---

# dplyr::select incident cervical_ca events
count(cervical_ca_clean) == count(distinct(cervical_ca_clean, patid)) 
incidentevents_cervical_ca <- cervical_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_cervical_ca <- as.data.frame(incidentevents_cervical_ca)
count(incidentevents_cervical_ca) == count(distinct(cervical_ca_clean, patid)) 

# dplyr::select incident endometrial_ca events
count(endometrial_ca_clean) == count(distinct(endometrial_ca_clean, patid)) 
incidentevents_endometrial_ca <- endometrial_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_endometrial_ca <- as.data.frame(incidentevents_endometrial_ca)
count(incidentevents_endometrial_ca) == count(distinct(endometrial_ca_clean, patid)) 

# dplyr::select incident malignant gtd events 
malignant_gtd <- gtd_clean %>% filter(gtdcat == "Definite")
count(malignant_gtd) == count(distinct(malignant_gtd, patid))
incidentevents_malignant_gtd <- malignant_gtd %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_malignant_gtd <- as.data.frame(incidentevents_malignant_gtd)
count(incidentevents_malignant_gtd) == count(distinct(malignant_gtd, patid)) 

# dplyr::select incident pelvic_tb events
count(pelvic_tb_clean) == count(distinct(pelvic_tb_clean, patid)) 
incidentevents_pelvic_tb <- pelvic_tb_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_pelvic_tb <- as.data.frame(incidentevents_pelvic_tb)
count(incidentevents_pelvic_tb) == count(distinct(pelvic_tb_clean, patid)) 

# Combine patients with an incident cervical ca/endometrial ca/malign gtd/pelvic tb date and dplyr::select earliest date of these
cerv_endom_gtd_pelvictb <- full_join(incidentevents_cervical_ca, incidentevents_endometrial_ca, 
                                     by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_malignant_gtd, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_pelvic_tb, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_cerv_endom_gtd_pelvictb <- cerv_endom_gtd_pelvictb %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_cerv_endom_gtd_pelvictb <- as.data.frame(incident_cerv_endom_gtd_pelvictb)
count(incident_cerv_endom_gtd_pelvictb) == count(distinct(incident_cerv_endom_gtd_pelvictb)) 
incident_cerv_endom_gtd_pelvictb <- rename(incident_cerv_endom_gtd_pelvictb, cerv_endom_gtd_pelvictb_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
cuiud_cohort <- left_join(contraception_cohort_final_washout, incident_cerv_endom_gtd_pelvictb, 
                          by = c("patid" = "patid"))
cuiud_cohort_entry <-  cuiud_cohort %>% 
  mutate(cuiud_cohort_entry = ifelse(is.na(cerv_endom_gtd_pelvictb_eventdate), contr_cohort_entry,
                                     ifelse(cerv_endom_gtd_pelvictb_eventdate <= contr_cohort_entry, NA, contr_cohort_entry)))
summary(is.na(cuiud_cohort_entry$cuiud_cohort_entry)) 
cuiud_cohort_entry_eligible <- cuiud_cohort_entry %>% filter(!is.na(cuiud_cohort_entry))

# Amend cohort exit dates
cuiud_cohort_entry_exit <- cuiud_cohort_entry_eligible %>% 
  mutate(cuiud_cohort_exit = ifelse(is.na(cerv_endom_gtd_pelvictb_eventdate), contr_cohort_exit,
                                    ifelse(cerv_endom_gtd_pelvictb_eventdate < contr_cohort_exit, cerv_endom_gtd_pelvictb_eventdate, contr_cohort_exit)))
summary(is.na(cuiud_cohort_entry_exit$contr_cohort_exit)) 

# Recalculate pdays and pyears based on those eligible after adjusting for cervical.ca.endomca/malign gtd
cuiud_cohort_entry_exit_pdays <- cuiud_cohort_entry_exit %>%
  mutate(cuiud_pdays = (cuiud_cohort_exit-cuiud_cohort_entry)+1) 
cuiud_cohort_entry_exit_pdays$pdays <- as.numeric(cuiud_cohort_entry_exit_pdays$pdays)

# convert to date format
cuiud_cohort_entry_exit_pdays$cuiud_cohort_entry <-  as_date(cuiud_cohort_entry_exit_pdays$cuiud_cohort_entry) 
cuiud_cohort_entry_exit_pdays$cuiud_cohort_exit <-  as_date(cuiud_cohort_entry_exit_pdays$cuiud_cohort_exit) 



## Adjust for pregnancy and postnatal intervals based on pregnancy outcomes (max and min pdays and pyears) ---

# dplyr::select pregnancy outcomes only (Drop currently pregnant, postnatal and vague timing)

pregnancy_outcomes <- pregnancy_clean %>% filter(pregcat != "Currently pregnant" & pregcat != "Pregnancy of childbirth-related terms with vague timing"
                                                 & pregcat != "Puerperium or neonatal(within 6 weeks after delivery)"
                                                 & pregcat != "Postnatal (after delivery)")

# create episode_start (max and min) and episode_end (max and min) dates for pregnancy events

pregnancy_max <- pregnancy_outcomes %>% 
  mutate(episode_start_max = ifelse(pregcat == "Abnormal product of conception", (eventdate-84),
                                    ifelse(pregcat == "Spontaneous abortion", (eventdate-168),
                                           ifelse(pregcat == "Termination of pregnancy", (eventdate-168),
                                                  ifelse(pregcat == "Unspecified abortion", (eventdate-168),
                                                         ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate-301),
                                                                ifelse(pregcat == "Ectopic pregnancy", (eventdate-84),
                                                                       ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate-301), NA))))))))

pregnancy_max <- pregnancy_max  %>% 
  mutate(episode_end_max = ifelse(pregcat == "Abnormal product of conception", (eventdate),
                                  ifelse(pregcat == "Spontaneous abortion", (eventdate),
                                         ifelse(pregcat == "Termination of pregnancy", (eventdate),
                                                ifelse(pregcat == "Unspecified abortion", (eventdate),
                                                       ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate),
                                                              ifelse(pregcat == "Ectopic pregnancy", (eventdate),
                                                                     ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate), NA))))))))

pregnancy_max_min <- pregnancy_max %>% 
  mutate(episode_start_min = ifelse(pregcat == "Abnormal product of conception", (eventdate-42),
                                    ifelse(pregcat == "Spontaneous abortion", (eventdate-28),
                                           ifelse(pregcat == "Termination of pregnancy", (eventdate-42),
                                                  ifelse(pregcat == "Unspecified abortion", (eventdate-28),
                                                         ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate-161),
                                                                ifelse(pregcat == "Ectopic pregnancy", (eventdate-42),
                                                                       ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate-100), NA))))))))


pregnancy_max_min <- pregnancy_max_min  %>% 
  mutate(episode_end_min = ifelse(pregcat == "Abnormal product of conception", (eventdate),
                                  ifelse(pregcat == "Spontaneous abortion", (eventdate),
                                         ifelse(pregcat == "Termination of pregnancy", (eventdate),
                                                ifelse(pregcat == "Unspecified abortion", (eventdate),
                                                       ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate),
                                                              ifelse(pregcat == "Ectopic pregnancy", (eventdate),
                                                                     ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate), NA))))))))

# convert to date format
pregnancy_max_min  <-  pregnancy_max_min  %>%
  mutate_if(is.numeric, as_date)
pregnancy_max_min$patid  <-  as.integer(pregnancy_max_min$patid)  

# Filter for labour/del/condition at birth & stillbirth/iud/perinatal i.e. delivery after 24 weeks

preg_24plus <- pregnancy_max_min %>% filter(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death" )
preg_pre24 <- pregnancy_max_min %>% filter(pregcat == "Abnormal product of conception" | pregcat == "Spontaneous abortion" |
                                             pregcat == "Termination of pregnancy" | pregcat == "Unspecified abortion" |pregcat == "Ectopic pregnancy")


# Drop events within 175 days of preg_24plus events (21 d until earliest possible ovulation + 22 weeks from then until earliest possibledelivery after 24 weeks)
preg_24plus_lag <- preg_24plus %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
preg_24plus_episodes <- preg_24plus_lag %>% filter(eventdate > (previous+175) | is.na(previous))

# Drop events within 19 days of preg_24plus events (5 d until earliest possible ovulation + 2 weeks until earliest possible positive pregnanct test and subsequent pre 24 week outcome) 
preg_pre24_lag <- preg_pre24 %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
preg_pre24_episodes <- preg_pre24_lag %>% filter(eventdate > (previous+19) | is.na(previous)) 

# add exclusion period end date (i.e. date after event when other outcome events cannot occur) + join dataframes
preg_24plus_episodes_excl_end <- preg_24plus_episodes %>% mutate(excl_end = (eventdate+175)) 
preg_pre24_episodes_excl_end <- preg_pre24_episodes %>% mutate(excl_end = (eventdate+19)) 
preg_excl_end <- full_join(preg_24plus_episodes_excl_end, preg_pre24_episodes_excl_end, 
                           by = c("patid" = "patid", "pregcat" = "pregcat", "eventdate" = "eventdate", 
                                  "episode_start_max" = "episode_start_max","episode_end_max" = "episode_end_max",
                                  "episode_start_min" = "episode_start_min", "episode_end_min" = "episode_end_min",
                                  "previous" = "previous", "excl_end" = "excl_end"))

# add previous events exclusion end date 
preg_excl_end_prev <- preg_excl_end %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous_excl_end = lag(excl_end))

# drop episodes that fall into prev events exclusion period
preg_episodes <- preg_excl_end_prev %>% filter(eventdate > previous_excl_end | is.na(previous_excl_end))

# extend preg episode to include postnatal 4 weeks for 24 week plus pregnancy outcomes)
preg_episodes_pn4w <- preg_episodes %>% 
  mutate(episode_end_max = ifelse(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death", 
                                  episode_end_max+28, episode_end_max)) %>%
  mutate(episode_end_min = ifelse(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death", 
                                  episode_end_min+28, episode_end_min)) 
preg_episodes_pn4w  <-  preg_episodes_pn4w  %>%
  mutate_if(is.numeric, as_date)
preg_episodes_pn4w$patid  <-  as.integer(preg_episodes_pn4w$patid)  

# drop episodes that are not in cohort entry/exit period for patient 
preg_episodes_pn4w_cohort <- left_join(preg_episodes_pn4w, cuiud_cohort_entry_exit_pdays, by = c("patid" = "patid"))
preg_episodes_pn4w_valid <-preg_episodes_pn4w_cohort %>% filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit) %>% 
  dplyr::select(c(patid, eventdate, pregcat, episode_start_max, episode_end_max, episode_start_min, episode_end_min))

# calculate episode durations 
preg_episodes_pn4w_duration_max <- preg_episodes_pn4w_valid  %>% mutate(duration_max =(episode_end_max-episode_start_max)+1) 
preg_episodes_pn4w_duration_max_min <- preg_episodes_pn4w_duration_max %>%  mutate(duration_min = (episode_end_min-episode_start_min)+1)
preg_episodes_pn4w_total_max <- preg_episodes_pn4w_duration_max_min %>% group_by(patid) %>% tally(duration_max)
preg_episodes_pn4w_total_min <- preg_episodes_pn4w_duration_max_min %>% group_by(patid) %>% tally(duration_min)
preg_episodes_pn4w_total_max <- rename(preg_episodes_pn4w_total_max, preg_total_max = n)
preg_episodes_pn4w_total_min <- rename(preg_episodes_pn4w_total_min, preg_total_min = n)

# recalculate pdays (max and min based on max and min preg durations)
cuiud_cohort_pregnancy_pn4w_episodes <- left_join(cuiud_cohort_entry_exit_pdays, preg_episodes_pn4w_total_max,
                                                  by = c("patid" = "patid")) %>%
  left_join(preg_episodes_pn4w_total_min, by = c("patid" = "patid"))
cuiud_cohort_pregnancy_pn4w_episodes$preg_total_max <- cuiud_cohort_pregnancy_pn4w_episodes$preg_total_max %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes$preg_total_min <- cuiud_cohort_pregnancy_pn4w_episodes$preg_total_min %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays <- cuiud_cohort_pregnancy_pn4w_episodes %>% 
  mutate(cuiud_pdays_max = (cuiud_pdays-preg_total_max)) %>%
  mutate(cuiud_pdays_min = (cuiud_pdays-preg_total_min)) 
cuiud_cohort_pregnancy_pn4w_episodes_pdays$cuiud_pdays_max <- as.numeric(cuiud_cohort_pregnancy_pn4w_episodes_pdays$cuiud_pdays_max)
cuiud_cohort_pregnancy_pn4w_episodes_pdays$cuiud_pdays_min <- as.numeric(cuiud_cohort_pregnancy_pn4w_episodes_pdays$cuiud_pdays_min)


## Adjust for conditions that have short intervals that affect pdays/pyears ---

# dplyr::select incident events for pid (drop codes that occur within 2 weeks of another code), CT (drop codes that occur within 1 weeks of another code) , & GC (drop codes that occur within 1 weeks of another code) 
pid_lag <- pid_clean %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate)) 
pid_episodes <- pid_lag %>% 
  filter(eventdate > (previous+14) | is.na(previous))
chlamydia_lag <- chlamydia_clean %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
chlamydia_episodes <- chlamydia_lag %>% filter(eventdate > (previous+7) | is.na(previous))
gonorrhoea_lag <- gonorrhoea_clean  %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
gonorrhoea_episodes <- gonorrhoea_lag %>% filter(eventdate > (previous+7) | is.na(previous))


# Drop episodes that occur outside of cohort entry/exit 
pid_episodes_cuiudcohort<- left_join(pid_episodes, cuiud_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
pidepisodes_cuiudvalid <- pid_episodes_cuiudcohort %>% filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit)
chlamydia_episodes_cuiudcohort<- left_join(chlamydia_episodes, cuiud_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
chlamydiaepisodes_cuiudvalid <- chlamydia_episodes_cuiudcohort %>% filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit)
gonorrhoea_episodes_cuiudcohort <- left_join(gonorrhoea_episodes, cuiud_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
gonorrhoeaepisodes_cuiudvalid <- gonorrhoea_episodes_cuiudcohort %>% filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit)


# create wide data of pregnancy max start date
preg_max <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_start_max))
preg_max_long <- preg_max %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_max_long)
preg_max_distinct_patid <- preg_max_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_max_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_max_distinct_patid_2 <- preg_max_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_max_distinct_patid_2$eventid <- eventid_variable$eventid
preg_max_long_expanded <-  left_join(preg_max_distinct_patid_2, preg_max_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_max_wide_start <- spread(preg_max_long_expanded , eventid, episode_start_max)
preg_max_wide_start <- preg_max_wide_start %>% rename_with( ~ paste("preg_max_start", .x, sep = "_"))

# create wide data of pregnancy max end date
preg_max <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_end_max))
preg_max_long <- preg_max %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_max_long)
preg_max_distinct_patid <- preg_max_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_max_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_max_distinct_patid_2 <- preg_max_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_max_distinct_patid_2$eventid <- eventid_variable$eventid
preg_max_long_expanded <-  left_join(preg_max_distinct_patid_2, preg_max_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_max_wide_end <- spread(preg_max_long_expanded , eventid, episode_end_max)
preg_max_wide_end <- preg_max_wide_end %>% rename_with( ~ paste("preg_max_end", .x, sep = "_"))

# Join preg max wide datasets
preg_max_wide <- full_join(preg_max_wide_start, preg_max_wide_end, by = c("preg_max_start_patid" = "preg_max_end_patid"))

# Drop pid events that fall into preg max
pid_preg_max <- left_join(pidepisodes_cuiudvalid, preg_max_wide, 
                          by = c("patid" = "preg_max_start_patid"))
pid_notinpregmax <- pid_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
pid_notinpregmax <- dplyr::select(pid_notinpregmax, c(patid, eventdate))
count(pid_notinpregmax)

# Drop chlamydia events that fall into preg max
chlamydia_preg_max <- left_join(chlamydiaepisodes_cuiudvalid, preg_max_wide, 
                                by = c("patid" = "preg_max_start_patid"))
chlamydia_notinpregmax <- chlamydia_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
chlamydia_notinpregmax <- dplyr::select(chlamydia_notinpregmax, c(patid, eventdate))
count(chlamydia_notinpregmax) 

# Drop gonorrhoea events that fall into preg max
gonorrhoea_preg_max <- left_join(gonorrhoeaepisodes_cuiudvalid, preg_max_wide, 
                                 by = c("patid" = "preg_max_start_patid"))
gonorrhoea_notinpregmax <- gonorrhoea_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
gonorrhoea_notinpregmax <- dplyr::select(gonorrhoea_notinpregmax, c(patid, eventdate))
count(gonorrhoea_notinpregmax) 


# create wide data of pregnancy min start date
preg_min <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_start_min))
preg_min_long <- preg_min %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_min_long)
preg_min_distinct_patid <- preg_min_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_min_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_min_distinct_patid_2 <- preg_min_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_min_distinct_patid_2$eventid <- eventid_variable$eventid
preg_min_long_expanded <-  left_join(preg_min_distinct_patid_2, preg_min_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_min_wide_start <- spread(preg_min_long_expanded , eventid, episode_start_min)
preg_min_wide_start <- preg_min_wide_start %>% rename_with( ~ paste("preg_min_start", .x, sep = "_"))

# create wide data of pregnancy min end date
preg_min <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_end_min))
preg_min_long <- preg_min %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_min_long)
preg_min_distinct_patid <- preg_min_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_min_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_min_distinct_patid_2 <- preg_min_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_min_distinct_patid_2$eventid <- eventid_variable$eventid
preg_min_long_expanded <-  left_join(preg_min_distinct_patid_2, preg_min_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_min_wide_end <- spread(preg_min_long_expanded , eventid, episode_end_min)
preg_min_wide_end <- preg_min_wide_end %>% rename_with( ~ paste("preg_min_end", .x, sep = "_"))

# Join preg min wide datasets
preg_min_wide <- full_join(preg_min_wide_start, preg_min_wide_end, by = c("preg_min_start_patid" = "preg_min_end_patid"))

# Drop pid events that fall into preg min
pid_preg_min <- left_join(pidepisodes_cuiudvalid, preg_min_wide, 
                          by = c("patid" = "preg_min_start_patid"))
pid_notinpregmin <- pid_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
pid_notinpregmin <- dplyr::select(pid_notinpregmin, c(patid, eventdate))
count(pid_notinpregmin) 

# Drop chlamydia events that fall into preg min
chlamydia_preg_min <- left_join(chlamydiaepisodes_cuiudvalid, preg_min_wide, 
                                by = c("patid" = "preg_min_start_patid"))
chlamydia_notinpregmin <- chlamydia_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
chlamydia_notinpregmin <- dplyr::select(chlamydia_notinpregmin, c(patid, eventdate))
count(chlamydia_notinpregmin) 

# Drop gonorrhoea events that fall into preg min
gonorrhoea_preg_min <- left_join(gonorrhoeaepisodes_cuiudvalid, preg_min_wide, 
                                 by = c("patid" = "preg_min_start_patid"))
gonorrhoea_notinpregmin <- gonorrhoea_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
gonorrhoea_notinpregmin <- dplyr::select(gonorrhoea_notinpregmin, c(patid, eventdate))
count(gonorrhoea_notinpregmin)



# add durations for infection intervals (pid, chlamydia, gonorrhoea) 
pid_episodes_preg_max_duration <- pid_notinpregmax %>% mutate(duration = 14) 
pid_episodes_preg_max_total <-pid_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
pid_episodes_preg_max_total <- rename(pid_episodes_preg_max_total, pid_preg_max_total = n)
pid_episodes_preg_min_duration <- pid_notinpregmin %>% mutate(duration = 14) 
pid_episodes_preg_min_total <-pid_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
pid_episodes_preg_min_total <- rename(pid_episodes_preg_min_total, pid_preg_min_total = n)

chlamydia_episodes_preg_max_duration <- chlamydia_notinpregmax %>% mutate(duration = 14) 
chlamydia_episodes_preg_max_total <-chlamydia_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
chlamydia_episodes_preg_max_total <- rename(chlamydia_episodes_preg_max_total, chlamydia_preg_max_total = n)
chlamydia_episodes_preg_min_duration <- chlamydia_notinpregmin %>% mutate(duration = 14) 
chlamydia_episodes_preg_min_total <-chlamydia_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
chlamydia_episodes_preg_min_total <- rename(chlamydia_episodes_preg_min_total, chlamydia_preg_min_total = n)

gonorrhoea_episodes_preg_max_duration <- gonorrhoea_notinpregmax %>% mutate(duration = 14) 
gonorrhoea_episodes_preg_max_total <-gonorrhoea_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
gonorrhoea_episodes_preg_max_total <- rename(gonorrhoea_episodes_preg_max_total, gonorrhoea_preg_max_total = n)
gonorrhoea_episodes_preg_min_duration <- gonorrhoea_notinpregmin %>% mutate(duration = 14) 
gonorrhoea_episodes_preg_min_total <-gonorrhoea_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
gonorrhoea_episodes_preg_min_total <- rename(gonorrhoea_episodes_preg_min_total, gonorrhoea_preg_min_total = n)

# recalculate pdays 
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc <- left_join(cuiud_cohort_pregnancy_pn4w_episodes_pdays, pid_episodes_preg_max_total,
                                                                  by = c("patid" = "patid")) %>%
  left_join(pid_episodes_preg_min_total, by = c("patid" = "patid"))  %>%
  left_join(chlamydia_episodes_preg_max_total, by = c("patid" = "patid"))  %>%
  left_join(chlamydia_episodes_preg_min_total, by = c("patid" = "patid"))  %>%
  left_join(gonorrhoea_episodes_preg_max_total, by = c("patid" = "patid"))  %>%
  left_join(gonorrhoea_episodes_preg_min_total, by = c("patid" = "patid"))
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_max_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_max_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_min_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_min_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_max_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_max_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_min_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_min_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_max_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_max_total %>% replace_na(0)
cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_min_total <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_min_total %>% replace_na(0)


cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc %>% 
  mutate(cuiud_pdays_max = (cuiud_pdays_max-pid_preg_max_total)) %>% 
  mutate(cuiud_pdays_min = (cuiud_pdays_min-pid_preg_min_total)) %>% 
  mutate(cuiud_pdays_max = (cuiud_pdays_max-chlamydia_preg_max_total)) %>% 
  mutate(cuiud_pdays_min = (cuiud_pdays_min-chlamydia_preg_min_total)) %>% 
  mutate(cuiud_pdays_max = (cuiud_pdays_max-gonorrhoea_preg_max_total)) %>% 
  mutate(cuiud_pdays_min = (cuiud_pdays_min-gonorrhoea_preg_min_total))


# Convert pdays to pyears ---

cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays_pyears_washout <- cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays %>%
  mutate(cuiud_pyears_max = (cuiud_pdays_max/365.25)) %>%
  mutate(cuiud_pyears_min = (cuiud_pdays_min/365.25)) 


# dplyr::select final variables for cohort ---
cuiud_cohort_final_washout <- dplyr::select(cuiud_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays_pyears_washout, c(patid, pracid, prac_region,
                                                                                                         dob, yob, patimd, pracimd, imd, migrant_status, migcertainty, ethnicat, ethnicat6, data_start, data_end,
                                                                                                         cuiud_cohort_entry, cuiud_cohort_exit, cuiud_pdays_max, cuiud_pdays_min, cuiud_pyears_max,
                                                                                                         cuiud_pyears_min)) 


save(cuiud_cohort_final_washout, file = "filepath")

## Cu IUD - exact matched cohort 4:1 ----------

load(file = "filepath")

# Derive age at cohort entry
cuiud_cohort_final_washout <- cuiud_cohort_final_washout %>%
  mutate(dob = lubridate::ymd(yob,truncated=2L))

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

cuiud_cohort_final_washout <- cuiud_cohort_final_washout %>%
  mutate(age_cohort_entry= calc_age(dob, cuiud_cohort_entry))

# Derive year of cohort entry
cuiud_cohort_final_washout <- cuiud_cohort_final_washout %>%
  mutate(year_cohort_entry = lubridate::year(ymd(cuiud_cohort_entry)))

# Drop unused levels for migrant status - added earlier in script, delete after re-running rest of script
levels(cuiud_cohort_final_washout$migrant_status)
cuiud_cohort_final_washout$migrant_status <- droplevels(cuiud_cohort_final_washout$migrant_status)
levels(cuiud_cohort_final_washout$migrant_status)

# turn migrant status into binary integer
cuiud_cohort_final_washout <- cuiud_cohort_final_washout %>%
  mutate(migrant_status_binary = as.numeric(migrant_status)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 1, 0)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 2, 1))

# select variables needed
cuiud_cohort_final_washout_2 <- dplyr::select(cuiud_cohort_final_washout, c(patid, migrant_status_binary,year_cohort_entry, age_cohort_entry, prac_region))

# Sample - for testing only
# cuiud_cohort_final_2 <- sample_n(cuiud_cohort_final_2, size=500000, replace=FALSE)

# function requires data.table

library(data.table)

# Turn df into data table

setDT(cuiud_cohort_final_washout_2)

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
cuiud_cohort_final_washout_2[, age_year_region := paste0(age_cohort_entry, '-', year_cohort_entry, '-', prac_region)]

# create matched dataset

matched_data_4to1 <- smatch(cuiud_cohort_final_washout_2, 'migrant_status_binary', 'age_year_region') # gives a different matched set each time
# matched_data <- smatch(all_patients_alldata_2, 'migrant_status_binary', 'age_year_region', seed = 5) # gives the same matched set each time (the number 5 is arbitrary)

# check balance
dcast(matched_data_4to1, age_year_region ~ migrant_status_binary, value.var = 'age_cohort_entry', fun.aggregate = length)

# Turn back into dataframe
matched_cuiud_cohort_final_4to1 <- as.data.frame(matched_data_4to1)

# Rejoin matched cohort to rest of dataset
matched_cuiud_cohort_final_4to1 <- dplyr::select(matched_cuiud_cohort_final_4to1, -c(prac_region, year_cohort_entry, age_cohort_entry,migrant_status_binary))
exact_match_cuiud_cohort_final_4to1 <- left_join(matched_cuiud_cohort_final_4to1, cuiud_cohort_final_washout, by = c("patid"="patid"))

# Check 4:1 for whole cohort
exact_match_cuiud_cohort_final_4to1 %>% group_by(migrant_status) %>% count() 

#Save cohort
save(exact_match_cuiud_cohort_final_4to1, file = "cleaned_files/exact_match_cuiud_cohort_final_4to1.Rdata") 


## Cu IUD  - exact matched with washout counts -------------------------

load(file = "filepath")

cuiud_presc <- contr_presc_clean %>% filter(contrpresccat == "Copper intra-uterine device") 

## Change prescribing events that are same category on same day, to one event ---

n_distinct(cuiud_presc) == count(distinct(cuiud_presc, patid, eventdate, contrpresccat,  .keep_all = TRUE)) 
cuiudpresc_oneeventperdate <- cuiud_presc %>% distinct(patid, eventdate, contrpresccat,  .keep_all = TRUE)
count(cuiudpresc_oneeventperdate) 

## Drop events that are not in exact matched cuiud cohort entry/exit time ---

cuiudpresc_indates <- left_join(cuiudpresc_oneeventperdate, exact_match_cuiud_cohort_final_4to1,
                                by = c("patid" = "patid"))
cuiudpresc_indates_2 <- cuiudpresc_indates %>% 
  filter(eventdate >= cuiud_cohort_entry & eventdate <= cuiud_cohort_exit) 
count(cuiudpresc_indates_2) 


## Drop events that fall into pregnancy durations  ---

# Assign row ids to preg outcome episodes & convert to wide dataset
preg_episodes_pn4w_valid_vars <- dplyr::select(preg_episodes_pn4w_valid, 
                                               c(patid,eventdate,episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_pn4w_valid_eventids <- preg_episodes_pn4w_valid_vars %>% group_by(patid) %>% mutate(eventid = row_number())
summary(preg_episodes_pn4w_valid_eventids) 
preg_episodes_pn4w_valid_wide <- pivot_wider(preg_episodes_pn4w_valid_eventids, names_from = eventid, 
                                             values_from = c(eventdate, episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_pn4w_valid_wide <- as.data.frame(preg_episodes_pn4w_valid_wide)
count(preg_episodes_pn4w_valid_wide) 
count(distinct(preg_episodes_pn4w_valid_eventids)) 

# Join wide pregnancy to cu iud prescriptions 
cuiudpresc_pregpn4w <- left_join(cuiudpresc_indates_2, preg_episodes_pn4w_valid_wide , 
                                 by = c("patid" = "patid"))

# Drop cu iud presc events that fall into pregn max intervals
cuiudpresc_notinpregpn4w_max <- cuiudpresc_pregpn4w %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_max_1 & eventdate <= episode_end_max_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_max_2 & eventdate <= episode_end_max_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_max_3 & eventdate <= episode_end_max_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_max_4 & eventdate <= episode_end_max_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_max_5 & eventdate <= episode_end_max_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_max_6 & eventdate <= episode_end_max_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_max_7 & eventdate <= episode_end_max_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_max_8 & eventdate <= episode_end_max_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_max_9 & eventdate <= episode_end_max_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_max_10 & eventdate <= episode_end_max_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_max_11 & eventdate <= episode_end_max_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no") 

# Drop cu iud presc events that fall into pregn min intervals
cuiudpresc_notinpregpn4w_min <- cuiudpresc_pregpn4w %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_min_1 & eventdate <= episode_end_min_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_min_2 & eventdate <= episode_end_min_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_min_3 & eventdate <= episode_end_min_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_min_4 & eventdate <= episode_end_min_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_min_5 & eventdate <= episode_end_min_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_min_6 & eventdate <= episode_end_min_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_min_7 & eventdate <= episode_end_min_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_min_8 & eventdate <= episode_end_min_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_min_9 & eventdate <= episode_end_min_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_min_10 & eventdate <= episode_end_min_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_min_11 & eventdate <= episode_end_min_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no")


# Final cuiud presc files with pregn max duration assumption and pregn min duration assumption
cuiudpresc_pregmaxassumption <- dplyr::select(cuiudpresc_notinpregpn4w_max, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                              issueseq))
cuiudpresc_pregminassumption <- dplyr::select(cuiudpresc_notinpregpn4w_min, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                              issueseq))
count(cuiudpresc_pregmaxassumption) 
count(cuiudpresc_pregminassumption) 


## Drop events that fall into conditions with short intervals based on preg max/min assumption ---

## Drop events that fall into pid not in preg max
pid_long <- pid_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(pid_long)

pid_distinct_patid <- pid_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,5,1), times = nrow(pid_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
pid_distinct_patid_2 <- pid_distinct_patid %>%
  slice(rep(1:n(), each=5))
pid_distinct_patid_2$eventid <- eventid_variable$eventid

pid_long_expanded <-  left_join(pid_distinct_patid_2, pid_long, 
                                by = c("patid" = "patid", "eventid" = "id"))

pid_long <- as.data.frame(pid_long)
pid_wide <- spread(pid_long_expanded, eventid, eventdate)
pid_wide <- pid_wide %>% rename_with( ~ paste("pid", .x, sep = "_"))

cuiudpresc_pid <- left_join(cuiudpresc_pregmaxassumption, pid_wide, 
                            by = c("patid" = "pid_patid"))
cuiudpresc_notinpid_pregmax <- cuiudpresc_pid %>% 
  mutate(to_drop = ifelse(is.na(pid_1), "no",
                          ifelse(eventdate >= pid_1 & eventdate <= (pid_1+14), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(pid_2), "no",
                           ifelse(eventdate >= pid_2 & eventdate <= (pid_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(pid_3), "no",
                           ifelse(eventdate >= pid_3 & eventdate <= (pid_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(pid_4), "no",
                           ifelse(eventdate >= pid_4 & eventdate <= (pid_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(pid_5), "no",
                           ifelse(eventdate >= pid_5 & eventdate <= (pid_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") 
cuiudpresc_notinpid_pregmax <- dplyr::select(cuiudpresc_notinpid_pregmax,c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                           issueseq))
count(cuiudpresc_notinpid_pregmax) 

## Drop events that fall into ct pregmax durations
chlamydia_long <- chlamydia_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(chlamydia_long)

chlamydia_distinct_patid <- chlamydia_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,7,1), times = nrow(chlamydia_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
chlamydia_distinct_patid_2 <- chlamydia_distinct_patid %>%
  slice(rep(1:n(), each=7))
chlamydia_distinct_patid_2$eventid <- eventid_variable$eventid

chlamydia_long_expanded <-  left_join(chlamydia_distinct_patid_2, chlamydia_long, 
                                      by = c("patid" = "patid", "eventid" = "id"))

chlamydia_long <- as.data.frame(chlamydia_long)
chlamydia_wide <- spread(chlamydia_long_expanded, eventid, eventdate)
chlamydia_wide <- chlamydia_wide %>% rename_with( ~ paste("chlamydia", .x, sep = "_"))

cuiudpresc_chlamydia <- left_join(cuiudpresc_notinpid_pregmax, chlamydia_wide, 
                                  by = c("patid" = "chlamydia_patid"))
cuiudpresc_notinchlamydia_pregmax <- cuiudpresc_chlamydia %>% 
  mutate(to_drop = ifelse(is.na(chlamydia_1), "no",
                          ifelse(eventdate >= chlamydia_1 & eventdate <= (chlamydia_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(chlamydia_2), "no",
                           ifelse(eventdate >= chlamydia_2 & eventdate <= (chlamydia_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(chlamydia_3), "no",
                           ifelse(eventdate >= chlamydia_3 & eventdate <= (chlamydia_3+7), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(chlamydia_4), "no",
                           ifelse(eventdate >= chlamydia_4 & eventdate <= (chlamydia_4+7), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(chlamydia_5), "no",
                           ifelse(eventdate >= chlamydia_5 & eventdate <= (chlamydia_5+7), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(chlamydia_6), "no",
                           ifelse(eventdate >= chlamydia_6 & eventdate <= (chlamydia_6+7), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(chlamydia_7), "no",
                           ifelse(eventdate >= chlamydia_7 & eventdate <= (chlamydia_7+7), "yes", "no"))) %>% 
  filter(to_drop7 == "no") 

cuiudpresc_notinpidct_pregmax <- dplyr::select(cuiudpresc_notinchlamydia_pregmax, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                    issueseq))
count(cuiudpresc_notinpidct_pregmax) 

## Drop events that fall into gc durations
gonorrhoea_long <- gonorrhoea_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(gonorrhoea_long)

gonorrhoea_distinct_patid <- gonorrhoea_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,2,1), times = nrow(gonorrhoea_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
gonorrhoea_distinct_patid_2 <- gonorrhoea_distinct_patid %>%
  slice(rep(1:n(), each=2))
gonorrhoea_distinct_patid_2$eventid <- eventid_variable$eventid

gonorrhoea_long_expanded <-  left_join(gonorrhoea_distinct_patid_2, gonorrhoea_long, 
                                       by = c("patid" = "patid", "eventid" = "id"))

gonorrhoea_long <- as.data.frame(gonorrhoea_long)
gonorrhoea_wide <- spread(gonorrhoea_long_expanded, eventid, eventdate)
gonorrhoea_wide <- gonorrhoea_wide %>% rename_with( ~ paste("gonorrhoea", .x, sep = "_"))

cuiudpresc_gonorrhoea <- left_join(cuiudpresc_notinpidct_pregmax, gonorrhoea_wide, 
                                   by = c("patid" = "gonorrhoea_patid"))
cuiudpresc_notingonorrhoea_pregmax <- cuiudpresc_gonorrhoea %>% 
  mutate(to_drop = ifelse(is.na(gonorrhoea_1), "no",
                          ifelse(eventdate >= gonorrhoea_1 & eventdate <= (gonorrhoea_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(gonorrhoea_2), "no",
                           ifelse(eventdate >= gonorrhoea_2 & eventdate <= (gonorrhoea_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") 


cuiudpresc_notinpidctgc_pregmax <- dplyr::select(cuiudpresc_notingonorrhoea_pregmax, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                       issueseq))
count(cuiudpresc_notinpidctgc_pregmax) 


## Drop events that fall into pid not in preg min
pid_long <- pid_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(pid_long)

pid_distinct_patid <- pid_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,5,1), times = nrow(pid_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
pid_distinct_patid_2 <- pid_distinct_patid %>%
  slice(rep(1:n(), each=5))
pid_distinct_patid_2$eventid <- eventid_variable$eventid

pid_long_expanded <-  left_join(pid_distinct_patid_2, pid_long, 
                                by = c("patid" = "patid", "eventid" = "id"))

pid_long <- as.data.frame(pid_long)
pid_wide <- spread(pid_long_expanded, eventid, eventdate)
pid_wide <- pid_wide %>% rename_with( ~ paste("pid", .x, sep = "_"))

cuiudpresc_pid <- left_join(cuiudpresc_pregminassumption, pid_wide, 
                            by = c("patid" = "pid_patid"))
cuiudpresc_notinpid_pregmin <- cuiudpresc_pid %>% 
  mutate(to_drop = ifelse(is.na(pid_1), "no",
                          ifelse(eventdate >= pid_1 & eventdate <= (pid_1+14), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(pid_2), "no",
                           ifelse(eventdate >= pid_2 & eventdate <= (pid_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(pid_3), "no",
                           ifelse(eventdate >= pid_3 & eventdate <= (pid_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(pid_4), "no",
                           ifelse(eventdate >= pid_4 & eventdate <= (pid_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(pid_5), "no",
                           ifelse(eventdate >= pid_5 & eventdate <= (pid_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") 
cuiudpresc_notinpid_pregmin <- dplyr::select(cuiudpresc_notinpid_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                            issueseq))
count(cuiudpresc_notinpid_pregmin) 

## Drop events that fall into ct pregmin durations
chlamydia_long <- chlamydia_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(chlamydia_long)

chlamydia_distinct_patid <- chlamydia_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,7,1), times = nrow(chlamydia_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
chlamydia_distinct_patid_2 <- chlamydia_distinct_patid %>%
  slice(rep(1:n(), each=7))
chlamydia_distinct_patid_2$eventid <- eventid_variable$eventid

chlamydia_long_expanded <-  left_join(chlamydia_distinct_patid_2, chlamydia_long, 
                                      by = c("patid" = "patid", "eventid" = "id"))

chlamydia_long <- as.data.frame(chlamydia_long)
chlamydia_wide <- spread(chlamydia_long_expanded, eventid, eventdate)
chlamydia_wide <- chlamydia_wide %>% rename_with( ~ paste("chlamydia", .x, sep = "_"))

cuiudpresc_chlamydia <- left_join(cuiudpresc_notinpid_pregmin, chlamydia_wide, 
                                  by = c("patid" = "chlamydia_patid"))
cuiudpresc_notinchlamydia_pregmin <- cuiudpresc_chlamydia %>% 
  mutate(to_drop = ifelse(is.na(chlamydia_1), "no",
                          ifelse(eventdate >= chlamydia_1 & eventdate <= (chlamydia_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(chlamydia_2), "no",
                           ifelse(eventdate >= chlamydia_2 & eventdate <= (chlamydia_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(chlamydia_3), "no",
                           ifelse(eventdate >= chlamydia_3 & eventdate <= (chlamydia_3+7), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(chlamydia_4), "no",
                           ifelse(eventdate >= chlamydia_4 & eventdate <= (chlamydia_4+7), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(chlamydia_5), "no",
                           ifelse(eventdate >= chlamydia_5 & eventdate <= (chlamydia_5+7), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(chlamydia_6), "no",
                           ifelse(eventdate >= chlamydia_6 & eventdate <= (chlamydia_6+7), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(chlamydia_7), "no",
                           ifelse(eventdate >= chlamydia_7 & eventdate <= (chlamydia_7+7), "yes", "no"))) %>% 
  filter(to_drop7 == "no") 

cuiudpresc_notinpidct_pregmin <- dplyr::select(cuiudpresc_notinchlamydia_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                    issueseq))
count(cuiudpresc_notinpidct_pregmin) 

## Drop events that fall into gc pregmin durations
gonorrhoea_long <- gonorrhoea_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(gonorrhoea_long)

gonorrhoea_distinct_patid <- gonorrhoea_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,2,1), times = nrow(gonorrhoea_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
gonorrhoea_distinct_patid_2 <- gonorrhoea_distinct_patid %>%
  slice(rep(1:n(), each=2))
gonorrhoea_distinct_patid_2$eventid <- eventid_variable$eventid

gonorrhoea_long_expanded <-  left_join(gonorrhoea_distinct_patid_2, gonorrhoea_long, 
                                       by = c("patid" = "patid", "eventid" = "id"))

gonorrhoea_long <- as.data.frame(gonorrhoea_long)
gonorrhoea_wide <- spread(gonorrhoea_long_expanded, eventid, eventdate)
gonorrhoea_wide <- gonorrhoea_wide %>% rename_with( ~ paste("gonorrhoea", .x, sep = "_"))

cuiudpresc_gonorrhoea <- left_join(cuiudpresc_notinpidct_pregmin, gonorrhoea_wide, 
                                   by = c("patid" = "gonorrhoea_patid"))
cuiudpresc_notingonorrhoea_pregmin <- cuiudpresc_gonorrhoea %>% 
  mutate(to_drop = ifelse(is.na(gonorrhoea_1), "no",
                          ifelse(eventdate >= gonorrhoea_1 & eventdate <= (gonorrhoea_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(gonorrhoea_2), "no",
                           ifelse(eventdate >= gonorrhoea_2 & eventdate <= (gonorrhoea_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") 


cuiudpresc_notinpidctgc_pregmin <- dplyr::select(cuiudpresc_notingonorrhoea_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                       issueseq))
count(cuiudpresc_notinpidctgc_pregmin) 


## Create annual cuiud presc counts (preg max and min assumption) ---


# create cuiud cohort file with line for each year that patient is in cohort 
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(exact_match_cuiud_cohort_final_4to1)))
colnames(year_variable) <- 'eventyear'
cuiud_annual_cohort <- exact_match_cuiud_cohort_final_4to1 %>%
  slice(rep(1:n(), each=10))
cuiud_annual_cohort$eventyear <- year_variable$eventyear
cuiud_annual_cohort <- filter(cuiud_annual_cohort, eventyear >= year(cuiud_cohort_entry) & eventyear <= year(cuiud_cohort_exit))

# Create annual cuiud presc episodes count 

cuiudpresc_pregmaxassumption_annual_counts <- cuiudpresc_notinpidctgc_pregmax %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(cuiudpresc_n_pregmax = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, cuiudpresc_n_pregmax) %>%
  distinct()

cuiudpresc_pregminassumption_annual_counts <- cuiudpresc_notinpidctgc_pregmin %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(cuiudpresc_n_pregmin = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, cuiudpresc_n_pregmin) %>%
  distinct()

# Join annual cohort file to annual cuiud presc files, change NAs to 0

cuiudpresc_annual_counts <- cuiud_annual_cohort %>%
  left_join(cuiudpresc_pregmaxassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(cuiudpresc_pregminassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear"))
cuiudpresc_annual_counts$cuiudpresc_n_pregmax <- cuiudpresc_annual_counts$cuiudpresc_n_pregmax %>% replace_na(0)
cuiudpresc_annual_counts$cuiudpresc_n_pregmin <- cuiudpresc_annual_counts$cuiudpresc_n_pregmin %>% replace_na(0)

# Add age during event year and age category 

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

cuiudpresc_annual_counts_age <- cuiudpresc_annual_counts %>% mutate(age_date= eventyear) 
cuiudpresc_annual_counts_age$age_date <- as.character(cuiudpresc_annual_counts_age$age_date)
cuiudpresc_annual_counts_age$age_date <- paste("01-01-", cuiudpresc_annual_counts_age$age_date, sep="")
head(cuiudpresc_annual_counts_age$age_date)
cuiudpresc_annual_counts_age$age_date <- dmy(cuiudpresc_annual_counts_age$age_date)
head(cuiudpresc_annual_counts_age$age_date)
class(cuiudpresc_annual_counts_age$age_date)
cuiudpresc_annual_counts_age <- cuiudpresc_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(cuiudpresc_annual_counts_age$eventyear_age)
max(cuiudpresc_annual_counts_age$eventyear_age) 
cuiudpresc_annual_counts_age_50 <- cuiudpresc_annual_counts_age %>% filter(eventyear_age == 50)
cuiudpresc_annual_counts_age_final <- cuiudpresc_annual_counts_age %>% 
  filter(eventyear_age != 50) %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
cuiudpresc_annual_counts_age_final$eventyear_agecat <- factor(cuiudpresc_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                              labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Final file
exact_match_cuiudpresc_annual_counts_final_4to1 <- cuiudpresc_annual_counts_age_final %>% dplyr::select(-c(age_date))

## Save and load final file
save(exact_match_cuiudpresc_annual_counts_final_4to1, file = "filepath")

load(file = "filepath")

## Set up data for stratifying by time variant variable (i.e add pdays per year preg max and pregmin, duration of preg/pic/ct/gc removed per year ---

# Add year durations
cuiudpresc_annual_counts_final_extra <- exact_match_cuiudpresc_annual_counts_final_4to1  %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
cuiudpresc_annual_counts_final_extra$start_eventyear <- as_date(cuiudpresc_annual_counts_final_extra$start_eventyear)
cuiudpresc_annual_counts_final_extra$end_eventyear <- as_date(cuiudpresc_annual_counts_final_extra$end_eventyear)
cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(cuiud_cohort_entry <= start_eventyear, start_eventyear,cuiud_cohort_entry))
cuiudpresc_annual_counts_final_extra$t0_ey <- as_date(cuiudpresc_annual_counts_final_extra$t0_ey)
cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(cuiud_cohort_exit >= end_eventyear, end_eventyear,cuiud_cohort_exit))
cuiudpresc_annual_counts_final_extra$t1_ey <- as_date(cuiudpresc_annual_counts_final_extra$t1_ey)
cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) 
cuiudpresc_annual_counts_final_extra$pdays_ey <- as.numeric(cuiudpresc_annual_counts_final_extra$pdays_ey)

# Calc duration of PID/CT/GC/Pregn in each year based on preg max/min assumption

x <- preg_episodes_pn4w_valid %>% filter(year(episode_end_max) == year(episode_start_max)) %>% 
  mutate(year = year(episode_end_max), pregduration_max = (episode_end_max - episode_start_max)+1) %>% 
  dplyr::select(patid, year, pregduration_max)
x$pregduration_max <- as.numeric(x$pregduration_max)

y <- preg_episodes_pn4w_valid %>% filter(year(episode_end_max) != year(episode_start_max)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_max = ifelse(start_end == "episode_start_max", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_max", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_max)

preg_annual_durations_max <- rbind(x,y) 
preg_annual_durations_max_grouped <- preg_annual_durations_max %>% group_by(patid, year) %>% tally(pregduration_max) 
preg_annual_durations_max_grouped <- rename(preg_annual_durations_max_grouped, pregduration_max = n)

x <- preg_episodes_pn4w_valid %>% filter(year(episode_end_min) == year(episode_start_min)) %>% 
  mutate(year = year(episode_end_min), pregduration_min = (episode_end_min - episode_start_min)+1) %>% 
  dplyr::select(patid, year, pregduration_min)
x$pregduration_min <- as.numeric(x$pregduration_min)

y <- preg_episodes_pn4w_valid %>% filter(year(episode_end_min) != year(episode_start_min)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_min = ifelse(start_end == "episode_start_min", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_min", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_min)

preg_annual_durations_min <- rbind(x,y) 
preg_annual_durations_min_grouped <- preg_annual_durations_min %>% group_by(patid, year) %>% tally(pregduration_min)
preg_annual_durations_min_grouped <- rename(preg_annual_durations_min_grouped, pregduration_min = n)

pid_annual_durations_pregmax <- pid_notinpregmax %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(pidduration_max = 14*n) %>% 
  dplyr::select(patid, year, pidduration_max)

ct_annual_durations_pregmax <- chlamydia_notinpregmax  %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(ctduration_max = 7*n)  %>% 
  dplyr::select(patid, year, ctduration_max)

gc_annual_durations_pregmax <- gonorrhoea_notinpregmax %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(gcduration_max = 14*n)  %>% 
  dplyr::select(patid, year, gcduration_max)

pid_annual_durations_pregmin <- pid_notinpregmin %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(pidduration_min = 14*n)  %>% 
  dplyr::select(patid, year, pidduration_min)

ct_annual_durations_pregmin <- chlamydia_notinpregmin  %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(ctduration_min = 7*n)  %>% 
  dplyr::select(patid, year, ctduration_min)

gc_annual_durations_pregmin <- gonorrhoea_notinpregmin %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(gcduration_min = 14*n)  %>% 
  dplyr::select(patid, year, gcduration_min)

## subtract preg/pid/ct/gc duratins from pdays_ey (max and min)

cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>% 
  left_join(preg_annual_durations_max_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(pid_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(ct_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(gc_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(preg_annual_durations_min_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(pid_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(ct_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(gc_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year"))


cuiudpresc_annual_counts_final_extra$pregduration_max <- cuiudpresc_annual_counts_final_extra$pregduration_max %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$pidduration_max <- cuiudpresc_annual_counts_final_extra$pidduration_max %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$ctduration_max <- cuiudpresc_annual_counts_final_extra$ctduration_max %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$gcduration_max <- cuiudpresc_annual_counts_final_extra$gcduration_max %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$pregduration_min <- cuiudpresc_annual_counts_final_extra$pregduration_min %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$pidduration_min <- cuiudpresc_annual_counts_final_extra$pidduration_min %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$ctduration_min <- cuiudpresc_annual_counts_final_extra$ctduration_min %>% replace_na(0)
cuiudpresc_annual_counts_final_extra$gcduration_min <- cuiudpresc_annual_counts_final_extra$gcduration_min %>% replace_na(0)

cuiudpresc_annual_counts_final_extra <- cuiudpresc_annual_counts_final_extra %>% 
  mutate(pdays_ey_pregmax = pdays_ey-(pregduration_max + pidduration_max + ctduration_max + gcduration_max)) %>% 
  mutate(pdays_ey_pregmin = pdays_ey-(pregduration_min + pidduration_min + ctduration_min + gcduration_min)) %>% 
  mutate(pyears_ey_pregmax=pdays_ey_pregmax/365.25) %>% 
  mutate(pyears_ey_pregmin=pdays_ey_pregmin/365.25)


## Save & load annual counts final extra (for glms) ---

exact_match_cuiudpresc_annual_counts_final_extra_4to1 <- cuiudpresc_annual_counts_final_extra %>%dplyr::select(patid, eventyear, pracid, prac_region, dob, yob, patimd, pracimd, imd, 
                                                                                              migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                              data_start, data_end, cuiud_cohort_entry, cuiud_cohort_exit, cuiud_pdays_max, 
                                                                                              cuiud_pdays_min, cuiud_pyears_max, cuiud_pyears_min, eventyear_age, eventyear_agecat, 
                                                                                              cuiudpresc_n_pregmax, cuiudpresc_n_pregmin, pdays_ey_pregmax, pdays_ey_pregmin, 
                                                                                              pyears_ey_pregmax, pyears_ey_pregmin)

save(exact_match_cuiudpresc_annual_counts_final_extra_4to1, file = "filepath")
load(file = "filepath")

## IUS - cohort with 365D washout, 4:1 analysis -------------------------------------------------------------------------------------------


load(file = "filepath") 

## Adjust for conditions that affect cohort entry/exit ---

# dplyr::select incident cervical_ca events
count(cervical_ca_clean) == count(distinct(cervical_ca_clean, patid)) 
incidentevents_cervical_ca <- cervical_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_cervical_ca <- as.data.frame(incidentevents_cervical_ca)
count(incidentevents_cervical_ca) == count(distinct(cervical_ca_clean, patid)) 

# dplyr::select incident endometrial_ca events
count(endometrial_ca_clean) == count(distinct(endometrial_ca_clean, patid)) 
incidentevents_endometrial_ca <- endometrial_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_endometrial_ca <- as.data.frame(incidentevents_endometrial_ca)
count(incidentevents_endometrial_ca) == count(distinct(endometrial_ca_clean, patid)) 

# dplyr::select incident malignant gtd events 
malignant_gtd <- gtd_clean %>% filter(gtdcat == "Definite")
count(malignant_gtd) == count(distinct(malignant_gtd, patid))
incidentevents_malignant_gtd <- malignant_gtd %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_malignant_gtd <- as.data.frame(incidentevents_malignant_gtd)
count(incidentevents_malignant_gtd) == count(distinct(malignant_gtd, patid))  

# dplyr::select incident pelvic_tb events
count(pelvic_tb_clean) == count(distinct(pelvic_tb_clean, patid)) 
incidentevents_pelvic_tb <- pelvic_tb_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_pelvic_tb <- as.data.frame(incidentevents_pelvic_tb)
count(incidentevents_pelvic_tb) == count(distinct(pelvic_tb_clean, patid)) 

# dplyr::select incident cirrhosis + dcld events, combine and then dplyr::select incident event for those with in cirrhosis event
# to end up with incident severe decompensated cirrhosis event
count(dcld_cirrhosis_clean) == count(distinct(dcld_cirrhosis_clean, patid)) 
incidentevents_dcld_cirrhosis <- dcld_cirrhosis_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_cirrhosis <- as.data.frame(incidentevents_dcld_cirrhosis)
count(incidentevents_dcld_cirrhosis) == count(distinct(dcld_cirrhosis_clean, patid))  

count(dcld_ascites_clean) == count(distinct(dcld_ascites_clean, patid)) 
incidentevents_dcld_ascites <- dcld_ascites_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_ascites <- as.data.frame(incidentevents_dcld_ascites)
count(incidentevents_dcld_ascites) == count(distinct(dcld_ascites_clean, patid))  

count(dcld_jaundice_clean) == count(distinct(dcld_jaundice_clean, patid)) 
incidentevents_dcld_jaundice <- dcld_jaundice_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_jaundice <- as.data.frame(incidentevents_dcld_jaundice)
count(incidentevents_dcld_jaundice) == count(distinct(dcld_jaundice_clean, patid))

count(dcld_varices_clean) == count(distinct(dcld_varices_clean, patid))
incidentevents_dcld_varices <- dcld_varices_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_varices <- as.data.frame(incidentevents_dcld_varices)
count(incidentevents_dcld_varices) == count(distinct(dcld_varices_clean, patid))

dcld_all <- full_join(incidentevents_dcld_cirrhosis, incidentevents_dcld_jaundice, 
                      by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_dcld_varices, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_dcld_all <- dcld_all %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_dcld_all <- as.data.frame(incident_dcld_all)
count(incident_dcld_all) == count(distinct(incident_dcld_all))
incidentevents_dcld_all_cirrhosis <- incident_dcld_all %>% filter(incident_dcld_all$patid %in% incidentevents_dcld_cirrhosis$patid)

# dplyr::select incident hep adenoma 
count(hep_adenoma_clean) == count(distinct(hep_adenoma_clean, patid))
incidentevents_hep_adenoma <- hep_adenoma_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_adenoma <- as.data.frame(incidentevents_hep_adenoma)
count(incidentevents_hep_adenoma) == count(distinct(hep_adenoma_clean, patid)) 

# dplyr::select incident hep ca
count(hep_ca_clean) == count(distinct(hep_ca_clean, patid)) 
incidentevents_hep_ca <- hep_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_ca <- as.data.frame(incidentevents_hep_ca)
count(incidentevents_hep_ca) == count(distinct(hep_ca_clean, patid))  

# Combine patients with incident condition affecting cohort entry/exit and dplyr::select earliest date of these
conditions <- full_join(incidentevents_cervical_ca, incidentevents_endometrial_ca, 
                        by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_malignant_gtd, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_pelvic_tb, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_dcld_all_cirrhosis, 
            by = c("patid" = "patid", "eventdate" = "eventdate"))  %>% 
  full_join(incidentevents_hep_adenoma, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_hep_ca, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_conditions <- conditions %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_conditions <- as.data.frame(incident_conditions)
count(incident_conditions) == count(distinct(incident_conditions))
incident_conditions <- rename(incident_conditions, conditions_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
ius_cohort <- left_join(contraception_cohort_entry_exit_pdays_pyears, incident_conditions, 
                        by = c("patid" = "patid"))
ius_cohort_entry <-  ius_cohort %>% 
  mutate(ius_cohort_entry = ifelse(is.na(conditions_eventdate), contr_cohort_entry,
                                   ifelse(conditions_eventdate <= contr_cohort_entry, NA, contr_cohort_entry)))
summary(is.na(ius_cohort_entry$ius_cohort_entry)) 
ius_cohort_entry_eligible <- ius_cohort_entry %>% filter(!is.na(ius_cohort_entry))

# Amend cohort exit dates
ius_cohort_entry_exit <- ius_cohort_entry_eligible %>% 
  mutate(ius_cohort_exit = ifelse(is.na(conditions_eventdate), contr_cohort_exit,
                                  ifelse(conditions_eventdate < contr_cohort_exit, conditions_eventdate, contr_cohort_exit)))
summary(is.na(ius_cohort_entry_exit$contr_cohort_exit)) 

# Recalculate pdays and pyears based on those eligible after adjusting for cervical.ca.endomca/malign gtd
ius_cohort_entry_exit_pdays <- ius_cohort_entry_exit %>%
  mutate(ius_pdays = (ius_cohort_exit-ius_cohort_entry)+1)  
ius_cohort_entry_exit_pdays$pdays <- as.numeric(ius_cohort_entry_exit_pdays$pdays)

# convert to date format
ius_cohort_entry_exit_pdays$ius_cohort_entry <-  as_date(ius_cohort_entry_exit_pdays$ius_cohort_entry) 
ius_cohort_entry_exit_pdays$ius_cohort_exit <-  as_date(ius_cohort_entry_exit_pdays$ius_cohort_exit) 



## Adjust for pregnancy and postnatal intervals based on pregnancy outcomes (max and min pdays and pyears) ---

# dplyr::select pregnancy outcomes only (Drop currently pregnant, postnatal and vague timing)

pregnancy_outcomes <- pregnancy_clean %>% filter(pregcat != "Currently pregnant" & pregcat != "Pregnancy of childbirth-related terms with vague timing"
                                                 & pregcat != "Puerperium or neonatal(within 6 weeks after delivery)"
                                                 & pregcat != "Postnatal (after delivery)")

# create episode_start (max and min) and episode_end (max and min) dates for pregnancy events

pregnancy_max <- pregnancy_outcomes %>% 
  mutate(episode_start_max = ifelse(pregcat == "Abnormal product of conception", (eventdate-84),
                                    ifelse(pregcat == "Spontaneous abortion", (eventdate-168),
                                           ifelse(pregcat == "Termination of pregnancy", (eventdate-168),
                                                  ifelse(pregcat == "Unspecified abortion", (eventdate-168),
                                                         ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate-301),
                                                                ifelse(pregcat == "Ectopic pregnancy", (eventdate-84),
                                                                       ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate-301), NA))))))))

pregnancy_max <- pregnancy_max  %>% 
  mutate(episode_end_max = ifelse(pregcat == "Abnormal product of conception", (eventdate),
                                  ifelse(pregcat == "Spontaneous abortion", (eventdate),
                                         ifelse(pregcat == "Termination of pregnancy", (eventdate),
                                                ifelse(pregcat == "Unspecified abortion", (eventdate),
                                                       ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate),
                                                              ifelse(pregcat == "Ectopic pregnancy", (eventdate),
                                                                     ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate), NA))))))))

pregnancy_max_min <- pregnancy_max %>% 
  mutate(episode_start_min = ifelse(pregcat == "Abnormal product of conception", (eventdate-42),
                                    ifelse(pregcat == "Spontaneous abortion", (eventdate-28),
                                           ifelse(pregcat == "Termination of pregnancy", (eventdate-42),
                                                  ifelse(pregcat == "Unspecified abortion", (eventdate-28),
                                                         ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate-161),
                                                                ifelse(pregcat == "Ectopic pregnancy", (eventdate-42),
                                                                       ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate-100), NA))))))))


pregnancy_max_min <- pregnancy_max_min  %>% 
  mutate(episode_end_min = ifelse(pregcat == "Abnormal product of conception", (eventdate),
                                  ifelse(pregcat == "Spontaneous abortion", (eventdate),
                                         ifelse(pregcat == "Termination of pregnancy", (eventdate),
                                                ifelse(pregcat == "Unspecified abortion", (eventdate),
                                                       ifelse(pregcat == "Labour, delivery or condition at birth", (eventdate),
                                                              ifelse(pregcat == "Ectopic pregnancy", (eventdate),
                                                                     ifelse(pregcat == "Stillbirth, intrauterine or perinatal death", (eventdate), NA))))))))

# convert to date format
pregnancy_max_min  <-  pregnancy_max_min  %>%
  mutate_if(is.numeric, as_date)
pregnancy_max_min$patid  <-  as.integer(pregnancy_max_min$patid)  

# Filter for labour/del/condition at birth & stillbirth/iud/perinatal i.e. delivery after 24 weeks

preg_24plus <- pregnancy_max_min %>% filter(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death" )
preg_pre24 <- pregnancy_max_min %>% filter(pregcat == "Abnormal product of conception" | pregcat == "Spontaneous abortion" |
                                             pregcat == "Termination of pregnancy" | pregcat == "Unspecified abortion" |pregcat == "Ectopic pregnancy")


# Drop events within 175 days of preg_24plus events (21 d until earliest possible ovulation + 22 weeks from then until earliest possibledelivery after 24 weeks)
preg_24plus_lag <- preg_24plus %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
preg_24plus_episodes <- preg_24plus_lag %>% filter(eventdate > (previous+175) | is.na(previous))

# Drop events within 19 days of preg_24plus events (5 d until earliest possible ovulation + 2 weeks until earliest possible positive pregnanct test and subsequent pre 24 week outcome) 
preg_pre24_lag <- preg_pre24 %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
preg_pre24_episodes <- preg_pre24_lag %>% filter(eventdate > (previous+19) | is.na(previous)) 

# add exclusion period end date (i.e. date after event when other outcome events cannot occur) + join dataframes
preg_24plus_episodes_excl_end <- preg_24plus_episodes %>% mutate(excl_end = (eventdate+175)) 
preg_pre24_episodes_excl_end <- preg_pre24_episodes %>% mutate(excl_end = (eventdate+19)) 
preg_excl_end <- full_join(preg_24plus_episodes_excl_end, preg_pre24_episodes_excl_end, 
                           by = c("patid" = "patid", "pregcat" = "pregcat", "eventdate" = "eventdate", 
                                  "episode_start_max" = "episode_start_max","episode_end_max" = "episode_end_max",
                                  "episode_start_min" = "episode_start_min", "episode_end_min" = "episode_end_min",
                                  "previous" = "previous", "excl_end" = "excl_end"))

# add previous events exclusion end date 
preg_excl_end_prev <- preg_excl_end %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous_excl_end = lag(excl_end))

# drop episodes that fall into prev events exclusion period
preg_episodes <- preg_excl_end_prev %>% filter(eventdate > previous_excl_end | is.na(previous_excl_end))

# extend preg episode to include postnatal 4 weeks for 24 week plus pregnancy outcomes)
preg_episodes_pn4w <- preg_episodes %>% 
  mutate(episode_end_max = ifelse(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death", 
                                  episode_end_max+28, episode_end_max)) %>%
  mutate(episode_end_min = ifelse(pregcat == "Labour, delivery or condition at birth" | pregcat == "Stillbirth, intrauterine or perinatal death", 
                                  episode_end_min+28, episode_end_min)) 
preg_episodes_pn4w  <-  preg_episodes_pn4w  %>%
  mutate_if(is.numeric, as_date)
preg_episodes_pn4w$patid  <-  as.integer(preg_episodes_pn4w$patid)  

# drop episodes that are not in cohort entry/exit period for patient 
preg_episodes_pn4w_cohort <- left_join(preg_episodes_pn4w, ius_cohort_entry_exit_pdays, by = c("patid" = "patid"))
preg_episodes_pn4w_valid <- preg_episodes_pn4w_cohort %>% filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit) %>% 
  dplyr::select(c(patid, eventdate, pregcat, episode_start_max, episode_end_max, episode_start_min, episode_end_min))

# calculate episode durations 
preg_episodes_pn4w_duration_max <- preg_episodes_pn4w_valid  %>% mutate(duration_max =(episode_end_max-episode_start_max)+1) 
preg_episodes_pn4w_duration_max_min <- preg_episodes_pn4w_duration_max %>%  mutate(duration_min = (episode_end_min-episode_start_min)+1)
preg_episodes_pn4w_total_max <- preg_episodes_pn4w_duration_max_min %>% group_by(patid) %>% tally(duration_max)
preg_episodes_pn4w_total_min <- preg_episodes_pn4w_duration_max_min %>% group_by(patid) %>% tally(duration_min)
preg_episodes_pn4w_total_max <- rename(preg_episodes_pn4w_total_max, preg_total_max = n)
preg_episodes_pn4w_total_min <- rename(preg_episodes_pn4w_total_min, preg_total_min = n)

# recalculate pdays and pyears (max and min based on max and min preg durations)
ius_cohort_pregnancy_pn4w_episodes <- left_join(ius_cohort_entry_exit_pdays, preg_episodes_pn4w_total_max,
                                                by = c("patid" = "patid")) %>%
  left_join(preg_episodes_pn4w_total_min, by = c("patid" = "patid"))
ius_cohort_pregnancy_pn4w_episodes$preg_total_max <- ius_cohort_pregnancy_pn4w_episodes$preg_total_max %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes$preg_total_min <- ius_cohort_pregnancy_pn4w_episodes$preg_total_min %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays <- ius_cohort_pregnancy_pn4w_episodes %>% 
  mutate(ius_pdays_max = (ius_pdays-preg_total_max)) %>%
  mutate(ius_pdays_min = (ius_pdays-preg_total_min)) 
ius_cohort_pregnancy_pn4w_episodes_pdays$ius_pdays_max <- as.numeric(ius_cohort_pregnancy_pn4w_episodes_pdays$ius_pdays_max)
ius_cohort_pregnancy_pn4w_episodes_pdays$ius_pdays_min <- as.numeric(ius_cohort_pregnancy_pn4w_episodes_pdays$ius_pdays_min)


## Adjust for conditions that have short intervals that affect pdays/pyears ---

# dplyr::select incident events for pid (drop codes that occur within 2 weeks of another code), CT (drop codes that occur within 1 weeks of another code) , & GC (drop codes that occur within 1 weeks of another code) 
pid_lag <- pid_clean %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate)) 
pid_episodes <- pid_lag %>% 
  filter(eventdate > (previous+14) | is.na(previous))
chlamydia_lag <- chlamydia_clean %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
chlamydia_episodes <- chlamydia_lag %>% filter(eventdate > (previous+7) | is.na(previous))
gonorrhoea_lag <- gonorrhoea_clean  %>% arrange(patid,eventdate) %>% group_by(patid) %>%  
  mutate(previous = lag(eventdate))
gonorrhoea_episodes <- gonorrhoea_lag %>% filter(eventdate > (previous+7) | is.na(previous))


# Drop episodes that occur outside of cohort entry/exit 
pid_episodes_iuscohort<- left_join(pid_episodes, ius_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
pidepisodes_iusvalid <- pid_episodes_iuscohort %>% filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit)
chlamydia_episodes_iuscohort<- left_join(chlamydia_episodes, ius_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
chlamydiaepisodes_iusvalid <- chlamydia_episodes_iuscohort %>% filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit)
gonorrhoea_episodes_iuscohort <- left_join(gonorrhoea_episodes, ius_cohort_pregnancy_pn4w_episodes_pdays, by = c("patid" = "patid"))
gonorrhoeaepisodes_iusvalid <- gonorrhoea_episodes_iuscohort %>% filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit)


# create wide data of pregnancy max start date
preg_max <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_start_max))
preg_max_long <- preg_max %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_max_long)
preg_max_distinct_patid <- preg_max_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_max_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_max_distinct_patid_2 <- preg_max_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_max_distinct_patid_2$eventid <- eventid_variable$eventid
preg_max_long_expanded <-  left_join(preg_max_distinct_patid_2, preg_max_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_max_wide_start <- spread(preg_max_long_expanded , eventid, episode_start_max)
preg_max_wide_start <- preg_max_wide_start %>% rename_with( ~ paste("preg_max_start", .x, sep = "_"))

# create wide data of pregnancy max end date
preg_max <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_end_max))
preg_max_long <- preg_max %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_max_long)
preg_max_distinct_patid <- preg_max_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_max_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_max_distinct_patid_2 <- preg_max_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_max_distinct_patid_2$eventid <- eventid_variable$eventid
preg_max_long_expanded <-  left_join(preg_max_distinct_patid_2, preg_max_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_max_wide_end <- spread(preg_max_long_expanded , eventid, episode_end_max)
preg_max_wide_end <- preg_max_wide_end %>% rename_with( ~ paste("preg_max_end", .x, sep = "_"))

# Join preg max wide datasets
preg_max_wide <- full_join(preg_max_wide_start, preg_max_wide_end, by = c("preg_max_start_patid" = "preg_max_end_patid"))

# Drop pid events that fall into preg max
pid_preg_max <- left_join(pidepisodes_iusvalid, preg_max_wide, 
                          by = c("patid" = "preg_max_start_patid"))
pid_notinpregmax <- pid_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
pid_notinpregmax <- dplyr::select(pid_notinpregmax, c(patid, eventdate))
count(pid_notinpregmax) 

# Drop chlamydia events that fall into preg max
chlamydia_preg_max <- left_join(chlamydiaepisodes_iusvalid, preg_max_wide, 
                                by = c("patid" = "preg_max_start_patid"))
chlamydia_notinpregmax <- chlamydia_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
chlamydia_notinpregmax <- dplyr::select(chlamydia_notinpregmax, c(patid, eventdate))
count(chlamydia_notinpregmax) 

# Drop gonorrhoea events that fall into preg max
gonorrhoea_preg_max <- left_join(gonorrhoeaepisodes_iusvalid, preg_max_wide, 
                                 by = c("patid" = "preg_max_start_patid"))
gonorrhoea_notinpregmax <- gonorrhoea_preg_max %>% 
  mutate(to_drop = ifelse(is.na(preg_max_start_1), "no",
                          ifelse(eventdate >= preg_max_start_1 & eventdate <= preg_max_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_max_start_2), "no",
                           ifelse(eventdate >= preg_max_start_2 & eventdate <= (preg_max_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_max_start_3), "no",
                           ifelse(eventdate >= preg_max_start_3 & eventdate <= (preg_max_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_max_start_4), "no",
                           ifelse(eventdate >= preg_max_start_4 & eventdate <= (preg_max_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_max_start_5), "no",
                           ifelse(eventdate >= preg_max_start_5 & eventdate <= (preg_max_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_max_start_6), "no",
                           ifelse(eventdate >= preg_max_start_6 & eventdate <= (preg_max_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_max_start_7), "no",
                           ifelse(eventdate >= preg_max_start_7 & eventdate <= (preg_max_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_max_start_8), "no",
                           ifelse(eventdate >= preg_max_start_8 & eventdate <= (preg_max_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_max_start_9), "no",
                           ifelse(eventdate >= preg_max_start_9 & eventdate <= (preg_max_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_max_start_10), "no",
                            ifelse(eventdate >= preg_max_start_10 & eventdate <= (preg_max_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_max_start_11), "no",
                            ifelse(eventdate >= preg_max_start_11 & eventdate <= (preg_max_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
gonorrhoea_notinpregmax <- dplyr::select(gonorrhoea_notinpregmax, c(patid, eventdate))
count(gonorrhoea_notinpregmax) 


# create wide data of pregnancy min start date
preg_min <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_start_min))
preg_min_long <- preg_min %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_min_long)
preg_min_distinct_patid <- preg_min_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_min_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_min_distinct_patid_2 <- preg_min_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_min_distinct_patid_2$eventid <- eventid_variable$eventid
preg_min_long_expanded <-  left_join(preg_min_distinct_patid_2, preg_min_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_min_wide_start <- spread(preg_min_long_expanded , eventid, episode_start_min)
preg_min_wide_start <- preg_min_wide_start %>% rename_with( ~ paste("preg_min_start", .x, sep = "_"))

# create wide data of pregnancy min end date
preg_min <- dplyr::select(preg_episodes_pn4w_valid, c(patid,episode_end_min))
preg_min_long <- preg_min %>% group_by(patid) %>% mutate(id = row_number())
summary(preg_min_long)
preg_min_distinct_patid <- preg_min_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,11,1), times = nrow(preg_min_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
preg_min_distinct_patid_2 <- preg_min_distinct_patid %>%
  slice(rep(1:n(), each=11))
preg_min_distinct_patid_2$eventid <- eventid_variable$eventid
preg_min_long_expanded <-  left_join(preg_min_distinct_patid_2, preg_min_long, 
                                     by = c("patid" = "patid", "eventid" = "id"))
preg_min_wide_end <- spread(preg_min_long_expanded , eventid, episode_end_min)
preg_min_wide_end <- preg_min_wide_end %>% rename_with( ~ paste("preg_min_end", .x, sep = "_"))

# Join preg min wide datasets
preg_min_wide <- full_join(preg_min_wide_start, preg_min_wide_end, by = c("preg_min_start_patid" = "preg_min_end_patid"))

# Drop pid events that fall into preg min
pid_preg_min <- left_join(pidepisodes_iusvalid, preg_min_wide, 
                          by = c("patid" = "preg_min_start_patid"))
pid_notinpregmin <- pid_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
pid_notinpregmin <- dplyr::select(pid_notinpregmin, c(patid, eventdate))
count(pid_notinpregmin) 

# Drop chlamydia events that fall into preg min
chlamydia_preg_min <- left_join(chlamydiaepisodes_iusvalid, preg_min_wide, 
                                by = c("patid" = "preg_min_start_patid"))
chlamydia_notinpregmin <- chlamydia_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
chlamydia_notinpregmin <- dplyr::select(chlamydia_notinpregmin, c(patid, eventdate))
count(chlamydia_notinpregmin) 

# Drop gonorrhoea events that fall into preg min
gonorrhoea_preg_min <- left_join(gonorrhoeaepisodes_iusvalid, preg_min_wide, 
                                 by = c("patid" = "preg_min_start_patid"))
gonorrhoea_notinpregmin <- gonorrhoea_preg_min %>% 
  mutate(to_drop = ifelse(is.na(preg_min_start_1), "no",
                          ifelse(eventdate >= preg_min_start_1 & eventdate <= preg_min_end_1, "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(preg_min_start_2), "no",
                           ifelse(eventdate >= preg_min_start_2 & eventdate <= (preg_min_end_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(preg_min_start_3), "no",
                           ifelse(eventdate >= preg_min_start_3 & eventdate <= (preg_min_end_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(preg_min_start_4), "no",
                           ifelse(eventdate >= preg_min_start_4 & eventdate <= (preg_min_end_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(preg_min_start_5), "no",
                           ifelse(eventdate >= preg_min_start_5 & eventdate <= (preg_min_end_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(preg_min_start_6), "no",
                           ifelse(eventdate >= preg_min_start_6 & eventdate <= (preg_min_end_6+14), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(preg_min_start_7), "no",
                           ifelse(eventdate >= preg_min_start_7 & eventdate <= (preg_min_end_7+14), "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>%
  mutate(to_drop8 = ifelse(is.na(preg_min_start_8), "no",
                           ifelse(eventdate >= preg_min_start_8 & eventdate <= (preg_min_end_8+14), "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>%
  mutate(to_drop9 = ifelse(is.na(preg_min_start_9), "no",
                           ifelse(eventdate >= preg_min_start_9 & eventdate <= (preg_min_end_9+14), "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>%
  mutate(to_drop10 = ifelse(is.na(preg_min_start_10), "no",
                            ifelse(eventdate >= preg_min_start_10 & eventdate <= (preg_min_end_10+14), "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>%
  mutate(to_drop11 = ifelse(is.na(preg_min_start_11), "no",
                            ifelse(eventdate >= preg_min_start_11 & eventdate <= (preg_min_end_11+14), "yes", "no"))) %>% 
  filter(to_drop11 == "no")
gonorrhoea_notinpregmin <- dplyr::select(gonorrhoea_notinpregmin, c(patid, eventdate))
count(gonorrhoea_notinpregmin) 



# add durations for infection intervals (pid, chlamydia, gonorrhoea) 
pid_episodes_preg_max_duration <- pid_notinpregmax %>% mutate(duration = 14) 
pid_episodes_preg_max_total <-pid_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
pid_episodes_preg_max_total <- rename(pid_episodes_preg_max_total, pid_preg_max_total = n)
pid_episodes_preg_min_duration <- pid_notinpregmin %>% mutate(duration = 14) 
pid_episodes_preg_min_total <-pid_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
pid_episodes_preg_min_total <- rename(pid_episodes_preg_min_total, pid_preg_min_total = n)

chlamydia_episodes_preg_max_duration <- chlamydia_notinpregmax %>% mutate(duration = 14) 
chlamydia_episodes_preg_max_total <-chlamydia_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
chlamydia_episodes_preg_max_total <- rename(chlamydia_episodes_preg_max_total, chlamydia_preg_max_total = n)
chlamydia_episodes_preg_min_duration <- chlamydia_notinpregmin %>% mutate(duration = 14) 
chlamydia_episodes_preg_min_total <-chlamydia_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
chlamydia_episodes_preg_min_total <- rename(chlamydia_episodes_preg_min_total, chlamydia_preg_min_total = n)

gonorrhoea_episodes_preg_max_duration <- gonorrhoea_notinpregmax %>% mutate(duration = 14) 
gonorrhoea_episodes_preg_max_total <-gonorrhoea_episodes_preg_max_duration  %>% group_by(patid) %>% tally(duration)
gonorrhoea_episodes_preg_max_total <- rename(gonorrhoea_episodes_preg_max_total, gonorrhoea_preg_max_total = n)
gonorrhoea_episodes_preg_min_duration <- gonorrhoea_notinpregmin %>% mutate(duration = 14) 
gonorrhoea_episodes_preg_min_total <-gonorrhoea_episodes_preg_min_duration  %>% group_by(patid) %>% tally(duration)
gonorrhoea_episodes_preg_min_total <- rename(gonorrhoea_episodes_preg_min_total, gonorrhoea_preg_min_total = n)

# recalculate pdays 
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc <- left_join(ius_cohort_pregnancy_pn4w_episodes_pdays, pid_episodes_preg_max_total,
                                                                by = c("patid" = "patid")) %>%
  left_join(pid_episodes_preg_min_total, by = c("patid" = "patid"))  %>%
  left_join(chlamydia_episodes_preg_max_total, by = c("patid" = "patid"))  %>%
  left_join(chlamydia_episodes_preg_min_total, by = c("patid" = "patid"))  %>%
  left_join(gonorrhoea_episodes_preg_max_total, by = c("patid" = "patid"))  %>%
  left_join(gonorrhoea_episodes_preg_min_total, by = c("patid" = "patid"))
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_max_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_max_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_min_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$pid_preg_min_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_max_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_max_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_min_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$chlamydia_preg_min_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_max_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_max_total %>% replace_na(0)
ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_min_total <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc$gonorrhoea_preg_min_total %>% replace_na(0)


ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc %>% 
  mutate(ius_pdays_max = (ius_pdays_max-pid_preg_max_total)) %>% 
  mutate(ius_pdays_min = (ius_pdays_min-pid_preg_min_total)) %>% 
  mutate(ius_pdays_max = (ius_pdays_max-chlamydia_preg_max_total)) %>% 
  mutate(ius_pdays_min = (ius_pdays_min-chlamydia_preg_min_total)) %>% 
  mutate(ius_pdays_max = (ius_pdays_max-gonorrhoea_preg_max_total)) %>% 
  mutate(ius_pdays_min = (ius_pdays_min-gonorrhoea_preg_min_total))


# Convert pdays to pyears ---

ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays_pyears <- ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays %>%
  mutate(ius_pyears_max = (ius_pdays_max/365.25)) %>%
  mutate(ius_pyears_min = (ius_pdays_min/365.25)) 


# dplyr::select final variables for cohort --- 
ius_cohort_final_washout <- dplyr::select(ius_cohort_pregnancy_pn4w_episodes_pdays_pid_ct_gc_pdays_pyears, c(patid, pracid, prac_region, 
                                                                                                     dob, yob, patimd, pracimd, imd, migrant_status, migcertainty, ethnicat, ethnicat6, data_start, data_end,
                                                                                                     ius_cohort_entry, ius_cohort_exit, ius_pdays_max, ius_pdays_min, ius_pyears_max,
                                                                                                     ius_pyears_min)) 



save(ius_cohort_final_washout, file = "filepath")
load(file = "filepath") 

## IUS - exact matched cohort 4:1 with washout ----------

load(file = "filepath")

# Derive age at cohort entry
ius_cohort_final_washout <- ius_cohort_final_washout %>%
  mutate(dob = lubridate::ymd(yob,truncated=2L))

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

ius_cohort_final_washout <- ius_cohort_final_washout %>%
  mutate(age_cohort_entry= calc_age(dob, ius_cohort_entry))

# Derive year of cohort entry
ius_cohort_final_washout <- ius_cohort_final_washout %>%
  mutate(year_cohort_entry = lubridate::year(ymd(ius_cohort_entry)))

# Drop unused levels for migrant status - added earlier in script, delete after re-running rest of script
levels(ius_cohort_final_washout$migrant_status)
ius_cohort_final_washout$migrant_status <- droplevels(ius_cohort_final_washout$migrant_status)
levels(ius_cohort_final_washout$migrant_status)

# turn migrant status into binary integer
ius_cohort_final_washout <- ius_cohort_final_washout %>%
  mutate(migrant_status_binary = as.numeric(migrant_status)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 1, 0)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 2, 1))

# select variables needed
ius_cohort_final_washout_2 <- dplyr::select(ius_cohort_final_washout, c(patid, migrant_status_binary,year_cohort_entry, age_cohort_entry, prac_region))

# Sample - for testing only
# ius_cohort_final_2 <- sample_n(ius_cohort_final_2, size=500000, replace=FALSE)

# function requires data.table

library(data.table)

# Turn df into data table

setDT(ius_cohort_final_washout_2)

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
ius_cohort_final_washout_2[, age_year_region := paste0(age_cohort_entry, '-', year_cohort_entry, '-', prac_region)]

# create matched dataset

matched_data_4to1 <- smatch(ius_cohort_final_washout_2, 'migrant_status_binary', 'age_year_region') # gives a different matched set each time
# matched_data <- smatch(all_patients_alldata_2, 'migrant_status_binary', 'age_year_region', seed = 5) # gives the same matched set each time (the number 5 is arbitrary)

# check balance
dcast(matched_data_4to1, age_year_region ~ migrant_status_binary, value.var = 'age_cohort_entry', fun.aggregate = length)

# Turn back into dataframe
matched_ius_cohort_final_4to1 <- as.data.frame(matched_data_4to1)

# Rejoin matched cohort to rest of dataset
matched_ius_cohort_final_4to1 <- dplyr::select(matched_ius_cohort_final_4to1, -c(prac_region, year_cohort_entry, age_cohort_entry,migrant_status_binary))
exact_match_ius_cohort_final_4to1 <- left_join(matched_ius_cohort_final_4to1, ius_cohort_final_washout, by = c("patid"="patid"))

# Check 4:1 for whole cohort
exact_match_ius_cohort_final_4to1 %>% group_by(migrant_status) %>% count() 

#Save cohort
save(exact_match_ius_cohort_final_4to1, file = "filepath") 


## IUS - Exact matched 4:1 with washout counts file ---------------------


load(file = "filepath")

ius_presc <- contr_presc_clean %>% filter(contrpresccat == "filepath") 

## Change prescribing events that are same category on same day, to one event ---

n_distinct(ius_presc) == count(distinct(ius_presc, patid, eventdate, contrpresccat,  .keep_all = TRUE)) #
iuspresc_oneeventperdate <- ius_presc %>% distinct(patid, eventdate, contrpresccat,  .keep_all = TRUE)
count(iuspresc_oneeventperdate) 

## Drop events that are not in ius cohort entry/exit time ---

iuspresc_indates <- left_join(iuspresc_oneeventperdate, exact_match_ius_cohort_final_4to1,
                              by = c("patid" = "patid"))
iuspresc_indates_2 <- iuspresc_indates %>% 
  filter(eventdate >= ius_cohort_entry & eventdate <= ius_cohort_exit) 
count(iuspresc_indates_2)


## Drop events that fall into pregnancy durations  ---

# Assign row ids to preg outcome episodes & convert to wide dataset
preg_episodes_pn4w_valid_vars <- dplyr::select(preg_episodes_pn4w_valid, 
                                               c(patid,eventdate,episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_pn4w_valid_eventids <- preg_episodes_pn4w_valid_vars %>% group_by(patid) %>% mutate(eventid = row_number())
summary(preg_episodes_pn4w_valid_eventids) 
preg_episodes_pn4w_valid_wide <- pivot_wider(preg_episodes_pn4w_valid_eventids, names_from = eventid, 
                                             values_from = c(eventdate, episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_pn4w_valid_wide <- as.data.frame(preg_episodes_pn4w_valid_wide)
count(preg_episodes_pn4w_valid_wide) 
count(distinct(preg_episodes_pn4w_valid_eventids)) 

# Join wide pregnancy to cu iud prescriptions 
iuspresc_pregpn4w <- left_join(iuspresc_indates_2, preg_episodes_pn4w_valid_wide , 
                               by = c("patid" = "patid"))

# Drop cu iud presc events that fall into pregn max intervals
iuspresc_notinpregpn4w_max <- iuspresc_pregpn4w %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_max_1 & eventdate <= episode_end_max_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_max_2 & eventdate <= episode_end_max_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_max_3 & eventdate <= episode_end_max_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_max_4 & eventdate <= episode_end_max_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_max_5 & eventdate <= episode_end_max_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_max_6 & eventdate <= episode_end_max_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_max_7 & eventdate <= episode_end_max_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_max_8 & eventdate <= episode_end_max_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_max_9 & eventdate <= episode_end_max_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_max_10 & eventdate <= episode_end_max_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_max_11 & eventdate <= episode_end_max_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no") 

# Drop cu iud presc events that fall into pregn min intervals
iuspresc_notinpregpn4w_min <- iuspresc_pregpn4w %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_min_1 & eventdate <= episode_end_min_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_min_2 & eventdate <= episode_end_min_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_min_3 & eventdate <= episode_end_min_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_min_4 & eventdate <= episode_end_min_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_min_5 & eventdate <= episode_end_min_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_min_6 & eventdate <= episode_end_min_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_min_7 & eventdate <= episode_end_min_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_min_8 & eventdate <= episode_end_min_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_min_9 & eventdate <= episode_end_min_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_min_10 & eventdate <= episode_end_min_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_min_11 & eventdate <= episode_end_min_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no")


# Final ius presc files with pregn max duration assumption and pregn min duration assumption
iuspresc_pregmaxassumption <- dplyr::select(iuspresc_notinpregpn4w_max, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                          issueseq))
iuspresc_pregminassumption <- dplyr::select(iuspresc_notinpregpn4w_min, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                          issueseq))
count(iuspresc_pregmaxassumption)
count(iuspresc_pregminassumption) 


## Drop events that fall into conditions with short intervals based on preg max/min assumption ---

## Drop events that fall into pid not in preg max
pid_long <- pid_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(pid_long)

pid_distinct_patid <- pid_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,5,1), times = nrow(pid_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
pid_distinct_patid_2 <- pid_distinct_patid %>%
  slice(rep(1:n(), each=5))
pid_distinct_patid_2$eventid <- eventid_variable$eventid

pid_long_expanded <-  left_join(pid_distinct_patid_2, pid_long, 
                                by = c("patid" = "patid", "eventid" = "id"))

pid_long <- as.data.frame(pid_long)
pid_wide <- spread(pid_long_expanded, eventid, eventdate)
pid_wide <- pid_wide %>% rename_with( ~ paste("pid", .x, sep = "_"))

iuspresc_pid <- left_join(iuspresc_pregmaxassumption, pid_wide, 
                          by = c("patid" = "pid_patid"))
iuspresc_notinpid_pregmax <- iuspresc_pid %>% 
  mutate(to_drop = ifelse(is.na(pid_1), "no",
                          ifelse(eventdate >= pid_1 & eventdate <= (pid_1+14), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(pid_2), "no",
                           ifelse(eventdate >= pid_2 & eventdate <= (pid_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(pid_3), "no",
                           ifelse(eventdate >= pid_3 & eventdate <= (pid_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(pid_4), "no",
                           ifelse(eventdate >= pid_4 & eventdate <= (pid_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(pid_5), "no",
                           ifelse(eventdate >= pid_5 & eventdate <= (pid_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") 
iuspresc_notinpid_pregmax <- dplyr::select(iuspresc_notinpid_pregmax,c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                       issueseq))
count(iuspresc_notinpid_pregmax) 

## Drop events that fall into ct pregmax durations
chlamydia_long <- chlamydia_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(chlamydia_long)

chlamydia_distinct_patid <- chlamydia_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,7,1), times = nrow(chlamydia_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
chlamydia_distinct_patid_2 <- chlamydia_distinct_patid %>%
  slice(rep(1:n(), each=7))
chlamydia_distinct_patid_2$eventid <- eventid_variable$eventid

chlamydia_long_expanded <-  left_join(chlamydia_distinct_patid_2, chlamydia_long, 
                                      by = c("patid" = "patid", "eventid" = "id"))

chlamydia_long <- as.data.frame(chlamydia_long)
chlamydia_wide <- spread(chlamydia_long_expanded, eventid, eventdate)
chlamydia_wide <- chlamydia_wide %>% rename_with( ~ paste("chlamydia", .x, sep = "_"))

iuspresc_chlamydia <- left_join(iuspresc_notinpid_pregmax, chlamydia_wide, 
                                by = c("patid" = "chlamydia_patid"))
iuspresc_notinchlamydia_pregmax <- iuspresc_chlamydia %>% 
  mutate(to_drop = ifelse(is.na(chlamydia_1), "no",
                          ifelse(eventdate >= chlamydia_1 & eventdate <= (chlamydia_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(chlamydia_2), "no",
                           ifelse(eventdate >= chlamydia_2 & eventdate <= (chlamydia_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(chlamydia_3), "no",
                           ifelse(eventdate >= chlamydia_3 & eventdate <= (chlamydia_3+7), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(chlamydia_4), "no",
                           ifelse(eventdate >= chlamydia_4 & eventdate <= (chlamydia_4+7), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(chlamydia_5), "no",
                           ifelse(eventdate >= chlamydia_5 & eventdate <= (chlamydia_5+7), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(chlamydia_6), "no",
                           ifelse(eventdate >= chlamydia_6 & eventdate <= (chlamydia_6+7), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(chlamydia_7), "no",
                           ifelse(eventdate >= chlamydia_7 & eventdate <= (chlamydia_7+7), "yes", "no"))) %>% 
  filter(to_drop7 == "no") 

iuspresc_notinpidct_pregmax <- dplyr::select(iuspresc_notinchlamydia_pregmax, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                issueseq))
count(iuspresc_notinpidct_pregmax) 

## Drop events that fall into gc durations
gonorrhoea_long <- gonorrhoea_notinpregmax %>% group_by(patid) %>% mutate(id = row_number())
summary(gonorrhoea_long)

gonorrhoea_distinct_patid <- gonorrhoea_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,2,1), times = nrow(gonorrhoea_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
gonorrhoea_distinct_patid_2 <- gonorrhoea_distinct_patid %>%
  slice(rep(1:n(), each=2))
gonorrhoea_distinct_patid_2$eventid <- eventid_variable$eventid

gonorrhoea_long_expanded <-  left_join(gonorrhoea_distinct_patid_2, gonorrhoea_long, 
                                       by = c("patid" = "patid", "eventid" = "id"))

gonorrhoea_long <- as.data.frame(gonorrhoea_long)
gonorrhoea_wide <- spread(gonorrhoea_long_expanded, eventid, eventdate)
gonorrhoea_wide <- gonorrhoea_wide %>% rename_with( ~ paste("gonorrhoea", .x, sep = "_"))

iuspresc_gonorrhoea <- left_join(iuspresc_notinpidct_pregmax, gonorrhoea_wide, 
                                 by = c("patid" = "gonorrhoea_patid"))
iuspresc_notingonorrhoea_pregmax <- iuspresc_gonorrhoea %>% 
  mutate(to_drop = ifelse(is.na(gonorrhoea_1), "no",
                          ifelse(eventdate >= gonorrhoea_1 & eventdate <= (gonorrhoea_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(gonorrhoea_2), "no",
                           ifelse(eventdate >= gonorrhoea_2 & eventdate <= (gonorrhoea_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") 


iuspresc_notinpidctgc_pregmax <- dplyr::select(iuspresc_notingonorrhoea_pregmax, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                   issueseq))
count(iuspresc_notinpidctgc_pregmax) 


## Drop events that fall into pid not in preg min
pid_long <- pid_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(pid_long)

pid_distinct_patid <- pid_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,5,1), times = nrow(pid_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
pid_distinct_patid_2 <- pid_distinct_patid %>%
  slice(rep(1:n(), each=5))
pid_distinct_patid_2$eventid <- eventid_variable$eventid

pid_long_expanded <-  left_join(pid_distinct_patid_2, pid_long, 
                                by = c("patid" = "patid", "eventid" = "id"))

pid_long <- as.data.frame(pid_long)
pid_wide <- spread(pid_long_expanded, eventid, eventdate)
pid_wide <- pid_wide %>% rename_with( ~ paste("pid", .x, sep = "_"))

iuspresc_pid <- left_join(iuspresc_pregminassumption, pid_wide, 
                          by = c("patid" = "pid_patid"))
iuspresc_notinpid_pregmin <- iuspresc_pid %>% 
  mutate(to_drop = ifelse(is.na(pid_1), "no",
                          ifelse(eventdate >= pid_1 & eventdate <= (pid_1+14), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(pid_2), "no",
                           ifelse(eventdate >= pid_2 & eventdate <= (pid_2+14), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(pid_3), "no",
                           ifelse(eventdate >= pid_3 & eventdate <= (pid_3+14), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(pid_4), "no",
                           ifelse(eventdate >= pid_4 & eventdate <= (pid_4+14), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(pid_5), "no",
                           ifelse(eventdate >= pid_5 & eventdate <= (pid_5+14), "yes", "no"))) %>% 
  filter(to_drop5 == "no") 
iuspresc_notinpid_pregmin <- dplyr::select(iuspresc_notinpid_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                        issueseq))
count(iuspresc_notinpid_pregmin) 

## Drop events that fall into ct pregmin durations
chlamydia_long <- chlamydia_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(chlamydia_long)

chlamydia_distinct_patid <- chlamydia_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,7,1), times = nrow(chlamydia_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
chlamydia_distinct_patid_2 <- chlamydia_distinct_patid %>%
  slice(rep(1:n(), each=7))
chlamydia_distinct_patid_2$eventid <- eventid_variable$eventid

chlamydia_long_expanded <-  left_join(chlamydia_distinct_patid_2, chlamydia_long, 
                                      by = c("patid" = "patid", "eventid" = "id"))

chlamydia_long <- as.data.frame(chlamydia_long)
chlamydia_wide <- spread(chlamydia_long_expanded, eventid, eventdate)
chlamydia_wide <- chlamydia_wide %>% rename_with( ~ paste("chlamydia", .x, sep = "_"))

iuspresc_chlamydia <- left_join(iuspresc_notinpid_pregmin, chlamydia_wide, 
                                by = c("patid" = "chlamydia_patid"))
iuspresc_notinchlamydia_pregmin <- iuspresc_chlamydia %>% 
  mutate(to_drop = ifelse(is.na(chlamydia_1), "no",
                          ifelse(eventdate >= chlamydia_1 & eventdate <= (chlamydia_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(chlamydia_2), "no",
                           ifelse(eventdate >= chlamydia_2 & eventdate <= (chlamydia_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(chlamydia_3), "no",
                           ifelse(eventdate >= chlamydia_3 & eventdate <= (chlamydia_3+7), "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>%
  mutate(to_drop4 = ifelse(is.na(chlamydia_4), "no",
                           ifelse(eventdate >= chlamydia_4 & eventdate <= (chlamydia_4+7), "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>%
  mutate(to_drop5 = ifelse(is.na(chlamydia_5), "no",
                           ifelse(eventdate >= chlamydia_5 & eventdate <= (chlamydia_5+7), "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>%
  mutate(to_drop6 = ifelse(is.na(chlamydia_6), "no",
                           ifelse(eventdate >= chlamydia_6 & eventdate <= (chlamydia_6+7), "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>%
  mutate(to_drop7 = ifelse(is.na(chlamydia_7), "no",
                           ifelse(eventdate >= chlamydia_7 & eventdate <= (chlamydia_7+7), "yes", "no"))) %>% 
  filter(to_drop7 == "no") 

iuspresc_notinpidct_pregmin <- dplyr::select(iuspresc_notinchlamydia_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                issueseq))
count(iuspresc_notinpidct_pregmin) 

## Drop events that fall into gc pregmin durations
gonorrhoea_long <- gonorrhoea_notinpregmin %>% group_by(patid) %>% mutate(id = row_number())
summary(gonorrhoea_long)

gonorrhoea_distinct_patid <- gonorrhoea_long %>% distinct(patid)
eventid_variable <- as.data.frame(rep(seq(1,2,1), times = nrow(gonorrhoea_distinct_patid)))
colnames(eventid_variable) <- 'eventid'
gonorrhoea_distinct_patid_2 <- gonorrhoea_distinct_patid %>%
  slice(rep(1:n(), each=2))
gonorrhoea_distinct_patid_2$eventid <- eventid_variable$eventid

gonorrhoea_long_expanded <-  left_join(gonorrhoea_distinct_patid_2, gonorrhoea_long, 
                                       by = c("patid" = "patid", "eventid" = "id"))

gonorrhoea_long <- as.data.frame(gonorrhoea_long)
gonorrhoea_wide <- spread(gonorrhoea_long_expanded, eventid, eventdate)
gonorrhoea_wide <- gonorrhoea_wide %>% rename_with( ~ paste("gonorrhoea", .x, sep = "_"))

iuspresc_gonorrhoea <- left_join(iuspresc_notinpidct_pregmin, gonorrhoea_wide, 
                                 by = c("patid" = "gonorrhoea_patid"))
iuspresc_notingonorrhoea_pregmin <- iuspresc_gonorrhoea %>% 
  mutate(to_drop = ifelse(is.na(gonorrhoea_1), "no",
                          ifelse(eventdate >= gonorrhoea_1 & eventdate <= (gonorrhoea_1+7), "yes", "no"))) %>% 
  filter(to_drop == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(gonorrhoea_2), "no",
                           ifelse(eventdate >= gonorrhoea_2 & eventdate <= (gonorrhoea_2+7), "yes", "no"))) %>% 
  filter(to_drop2 == "no") 


iuspresc_notinpidctgc_pregmin <- dplyr::select(iuspresc_notingonorrhoea_pregmin, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                                   issueseq))
count(iuspresc_notinpidctgc_pregmin) 


## Create annual ius presc counts (preg max and min assumption) ---


# create ius cohort file with line for each year that patient is in cohort 
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(exact_match_ius_cohort_final_4to1)))
colnames(year_variable) <- 'eventyear'
ius_annual_cohort <- exact_match_ius_cohort_final_4to1 %>%
  slice(rep(1:n(), each=10))
ius_annual_cohort$eventyear <- year_variable$eventyear
ius_annual_cohort <- filter(ius_annual_cohort, eventyear >= year(ius_cohort_entry) & eventyear <= year(ius_cohort_exit))

# Create annual ius presc episodes count 

iuspresc_pregmaxassumption_annual_counts <- iuspresc_notinpidctgc_pregmax %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(iuspresc_n_pregmax = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, iuspresc_n_pregmax) %>%
  distinct()

iuspresc_pregminassumption_annual_counts <- iuspresc_notinpidctgc_pregmin %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(iuspresc_n_pregmin = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, iuspresc_n_pregmin) %>%
  distinct()

# Join annual cohort file to annual ius presc files, change NAs to 0

iuspresc_annual_counts <- ius_annual_cohort %>%
  left_join(iuspresc_pregmaxassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(iuspresc_pregminassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear"))
iuspresc_annual_counts$iuspresc_n_pregmax <- iuspresc_annual_counts$iuspresc_n_pregmax %>% replace_na(0)
iuspresc_annual_counts$iuspresc_n_pregmin <- iuspresc_annual_counts$iuspresc_n_pregmin %>% replace_na(0)

# Add age during event year and age category 

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

iuspresc_annual_counts_age <- iuspresc_annual_counts %>% mutate(age_date= eventyear) 
iuspresc_annual_counts_age$age_date <- as.character(iuspresc_annual_counts_age$age_date)
iuspresc_annual_counts_age$age_date <- paste("01-01-", iuspresc_annual_counts_age$age_date, sep="")
head(iuspresc_annual_counts_age$age_date)
iuspresc_annual_counts_age$age_date <- dmy(iuspresc_annual_counts_age$age_date)
head(iuspresc_annual_counts_age$age_date)
class(iuspresc_annual_counts_age$age_date)
iuspresc_annual_counts_age <- iuspresc_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(iuspresc_annual_counts_age$eventyear_age)
max(iuspresc_annual_counts_age$eventyear_age) 
iuspresc_annual_counts_age_50 <- iuspresc_annual_counts_age %>% filter(eventyear_age == 50) 
iuspresc_annual_counts_age_final <- iuspresc_annual_counts_age %>% 
  filter(eventyear_age != 50) %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
iuspresc_annual_counts_age_final$eventyear_agecat <- factor(iuspresc_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                            labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Final file
exact_match_iuspresc_annual_counts_final_4to1 <- iuspresc_annual_counts_age_final %>% dplyr::select(-c(age_date))

## Save and load final file
save(exact_match_iuspresc_annual_counts_final_4to1, file = "filepath")
load(file = "filepath")

## Set up data for stratifying by time variant variable (i.e add pdays per year preg max and pregmin, duration of preg/pic/ct/gc removed per year ---

# Add year durations
iuspresc_annual_counts_final_extra <- exact_match_iuspresc_annual_counts_final_4to1 %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
iuspresc_annual_counts_final_extra$start_eventyear <- as_date(iuspresc_annual_counts_final_extra$start_eventyear)
iuspresc_annual_counts_final_extra$end_eventyear <- as_date(iuspresc_annual_counts_final_extra$end_eventyear)
iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(ius_cohort_entry <= start_eventyear, start_eventyear,ius_cohort_entry))
iuspresc_annual_counts_final_extra$t0_ey <- as_date(iuspresc_annual_counts_final_extra$t0_ey)
iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(ius_cohort_exit >= end_eventyear, end_eventyear,ius_cohort_exit))
iuspresc_annual_counts_final_extra$t1_ey <- as_date(iuspresc_annual_counts_final_extra$t1_ey)
iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) 
iuspresc_annual_counts_final_extra$pdays_ey <- as.numeric(iuspresc_annual_counts_final_extra$pdays_ey)

# Calc duration of PID/CT/GC/Pregn in each year based on preg max/min assumption

x <- preg_episodes_pn4w_valid %>% filter(year(episode_end_max) == year(episode_start_max)) %>% 
  mutate(year = year(episode_end_max), pregduration_max = (episode_end_max - episode_start_max)+1) %>% 
  dplyr::select(patid, year, pregduration_max)
x$pregduration_max <- as.numeric(x$pregduration_max)

y <- preg_episodes_pn4w_valid %>% filter(year(episode_end_max) != year(episode_start_max)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_max = ifelse(start_end == "episode_start_max", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_max", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_max)

preg_annual_durations_max <- rbind(x,y) 
preg_annual_durations_max_grouped <- preg_annual_durations_max %>% group_by(patid, year) %>% tally(pregduration_max) 
preg_annual_durations_max_grouped <- rename(preg_annual_durations_max_grouped, pregduration_max = n)

x <- preg_episodes_pn4w_valid %>% filter(year(episode_end_min) == year(episode_start_min)) %>% 
  mutate(year = year(episode_end_min), pregduration_min = (episode_end_min - episode_start_min)+1) %>% 
  dplyr::select(patid, year, pregduration_min)
x$pregduration_min <- as.numeric(x$pregduration_min)

y <- preg_episodes_pn4w_valid %>% filter(year(episode_end_min) != year(episode_start_min)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_min = ifelse(start_end == "episode_start_min", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_min", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_min)

preg_annual_durations_min <- rbind(x,y) 
preg_annual_durations_min_grouped <- preg_annual_durations_min %>% group_by(patid, year) %>% tally(pregduration_min) 
preg_annual_durations_min_grouped <- rename(preg_annual_durations_min_grouped, pregduration_min = n)

pid_annual_durations_pregmax <- pid_notinpregmax %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(pidduration_max = 14*n) %>% 
  dplyr::select(patid, year, pidduration_max)

ct_annual_durations_pregmax <- chlamydia_notinpregmax  %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(ctduration_max = 7*n)  %>% 
  dplyr::select(patid, year, ctduration_max)

gc_annual_durations_pregmax <- gonorrhoea_notinpregmax %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(gcduration_max = 14*n)  %>% 
  dplyr::select(patid, year, gcduration_max)

pid_annual_durations_pregmin <- pid_notinpregmin %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(pidduration_min = 14*n)  %>% 
  dplyr::select(patid, year, pidduration_min)

ct_annual_durations_pregmin <- chlamydia_notinpregmin  %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(ctduration_min = 7*n)  %>% 
  dplyr::select(patid, year, ctduration_min)

gc_annual_durations_pregmin <- gonorrhoea_notinpregmin %>% mutate(year = year(eventdate)) %>% 
  group_by(patid,year) %>% 
  count() %>% 
  arrange(patid,year) %>% 
  mutate(gcduration_min = 14*n)  %>% 
  dplyr::select(patid, year, gcduration_min)

## subtract preg/pid/ct/gc duratins from pdays_ey (max and min)

iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>% 
  left_join(preg_annual_durations_max_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(pid_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(ct_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(gc_annual_durations_pregmax, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(preg_annual_durations_min_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(pid_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(ct_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year")) %>% 
  left_join(gc_annual_durations_pregmin, by = c("patid" = "patid", "eventyear" = "year"))


iuspresc_annual_counts_final_extra$pregduration_max <- iuspresc_annual_counts_final_extra$pregduration_max %>% replace_na(0)
iuspresc_annual_counts_final_extra$pidduration_max <- iuspresc_annual_counts_final_extra$pidduration_max %>% replace_na(0)
iuspresc_annual_counts_final_extra$ctduration_max <- iuspresc_annual_counts_final_extra$ctduration_max %>% replace_na(0)
iuspresc_annual_counts_final_extra$gcduration_max <- iuspresc_annual_counts_final_extra$gcduration_max %>% replace_na(0)
iuspresc_annual_counts_final_extra$pregduration_min <- iuspresc_annual_counts_final_extra$pregduration_min %>% replace_na(0)
iuspresc_annual_counts_final_extra$pidduration_min <- iuspresc_annual_counts_final_extra$pidduration_min %>% replace_na(0)
iuspresc_annual_counts_final_extra$ctduration_min <- iuspresc_annual_counts_final_extra$ctduration_min %>% replace_na(0)
iuspresc_annual_counts_final_extra$gcduration_min <- iuspresc_annual_counts_final_extra$gcduration_min %>% replace_na(0)

iuspresc_annual_counts_final_extra <- iuspresc_annual_counts_final_extra %>% 
  mutate(pdays_ey_pregmax = pdays_ey-(pregduration_max + pidduration_max + ctduration_max + gcduration_max)) %>% 
  mutate(pdays_ey_pregmin = pdays_ey-(pregduration_min + pidduration_min + ctduration_min + gcduration_min)) %>% 
  mutate(pyears_ey_pregmax=pdays_ey_pregmax/365.25) %>% 
  mutate(pyears_ey_pregmin=pdays_ey_pregmin/365.25)


## Save & load annual counts final extra (for glms) ---

exact_match_iuspresc_annual_counts_final_extra_4to1 <- iuspresc_annual_counts_final_extra %>%dplyr::select(patid, eventyear, pracid, prac_region, dob, yob, patimd, pracimd, imd, 
                                                                                          migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                          data_start, data_end, ius_cohort_entry, ius_cohort_exit, ius_pdays_max, 
                                                                                          ius_pdays_min, ius_pyears_min, ius_pyears_min, eventyear_age, eventyear_agecat, 
                                                                                          iuspresc_n_pregmax, iuspresc_n_pregmin, pdays_ey_pregmax, pdays_ey_pregmin, 
                                                                                          pyears_ey_pregmax, pyears_ey_pregmin)





save(exact_match_iuspresc_annual_counts_final_extra_4to1, file = "filepath")
load(file = "filepath")


## SDI - cohort with 365D washout -------------------------------------------------------------------------------------------

load(file = "cleaned_files/contraception_cohort_final_washout.Rdata") 


## Adjust for conditions that affect cohort entry/exit ---

# dplyr::select incident breast ca events
count(breast_ca_clean) == count(distinct(breast_ca_clean, patid))
incidentevents_breast_ca <- breast_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_breast_ca <- as.data.frame(incidentevents_breast_ca)
count(incidentevents_breast_ca) == count(distinct(breast_ca_clean, patid)) 

# dplyr::select incident cirrhosis + dcld events, combine and then dplyr::select incident event for those with in cirrhosis event
# to end up with incident severe decompensated cirrhosis event
count(dcld_cirrhosis_clean) == count(distinct(dcld_cirrhosis_clean, patid)) 
incidentevents_dcld_cirrhosis <- dcld_cirrhosis_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_cirrhosis <- as.data.frame(incidentevents_dcld_cirrhosis)
count(incidentevents_dcld_cirrhosis) == count(distinct(dcld_cirrhosis_clean, patid)) 

count(dcld_ascites_clean) == count(distinct(dcld_ascites_clean, patid))
incidentevents_dcld_ascites <- dcld_ascites_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_ascites <- as.data.frame(incidentevents_dcld_ascites)
count(incidentevents_dcld_ascites) == count(distinct(dcld_ascites_clean, patid)) 

count(dcld_jaundice_clean) == count(distinct(dcld_jaundice_clean, patid))
incidentevents_dcld_jaundice <- dcld_jaundice_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_jaundice <- as.data.frame(incidentevents_dcld_jaundice)
count(incidentevents_dcld_jaundice) == count(distinct(dcld_jaundice_clean, patid))

count(dcld_varices_clean) == count(distinct(dcld_varices_clean, patid)) 
incidentevents_dcld_varices <- dcld_varices_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_dcld_varices <- as.data.frame(incidentevents_dcld_varices)
count(incidentevents_dcld_varices) == count(distinct(dcld_varices_clean, patid))  

dcld_all <- full_join(incidentevents_dcld_cirrhosis, incidentevents_dcld_jaundice, 
                      by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_dcld_varices, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_dcld_all <- dcld_all %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_dcld_all <- as.data.frame(incident_dcld_all)
count(incident_dcld_all) == count(distinct(incident_dcld_all)) 
incidentevents_dcld_all_cirrhosis <- incident_dcld_all %>% filter(incident_dcld_all$patid %in% incidentevents_dcld_cirrhosis$patid)

# dplyr::select incident hep adenoma 
count(hep_adenoma_clean) == count(distinct(hep_adenoma_clean, patid)) 
incidentevents_hep_adenoma <- hep_adenoma_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_adenoma <- as.data.frame(incidentevents_hep_adenoma)
count(incidentevents_hep_adenoma) == count(distinct(hep_adenoma_clean, patid))

# dplyr::select incident hep ca
count(hep_ca_clean) == count(distinct(hep_ca_clean, patid))
incidentevents_hep_ca <- hep_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_ca <- as.data.frame(incidentevents_hep_ca)
count(incidentevents_hep_ca) == count(distinct(hep_ca_clean, patid)) 

# Combine patients with incident condition affecting cohort entry/exit and dplyr::select earliest date of these
conditions <- full_join(incidentevents_breast_ca, incidentevents_dcld_cirrhosis, 
                        by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_pelvic_tb, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_dcld_all_cirrhosis, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_hep_adenoma, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_hep_ca, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_conditions <- conditions %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_conditions <- as.data.frame(incident_conditions)
count(incident_conditions) == count(distinct(incident_conditions)) 
incident_conditions <- rename(incident_conditions, conditions_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
sdi_cohort <- left_join(contraception_cohort_final_washout, incident_conditions, 
                        by = c("patid" = "patid"))
sdi_cohort_entry <-  sdi_cohort %>% 
  mutate(sdi_cohort_entry = ifelse(is.na(conditions_eventdate), contr_cohort_entry,
                                   ifelse(conditions_eventdate <= contr_cohort_entry, NA, contr_cohort_entry)))
summary(is.na(sdi_cohort_entry$sdi_cohort_entry)) 
sdi_cohort_entry_eligible <- sdi_cohort_entry %>% filter(!is.na(sdi_cohort_entry))

# Amend cohort exit dates
sdi_cohort_entry_exit <- sdi_cohort_entry_eligible %>% 
  mutate(sdi_cohort_exit = ifelse(is.na(conditions_eventdate), contr_cohort_exit,
                                  ifelse(conditions_eventdate < contr_cohort_exit, conditions_eventdate, contr_cohort_exit)))
summary(is.na(sdi_cohort_entry_exit$contr_cohort_exit)) 

# Recalculate pdays and pyears based on those eligible after adjusting for cervical.ca.endomca/malign gtd
sdi_cohort_entry_exit_pdays <- sdi_cohort_entry_exit %>%
  mutate(sdi_pdays = (sdi_cohort_exit-sdi_cohort_entry)+1)  
sdi_cohort_entry_exit_pdays$pdays <- as.numeric(sdi_cohort_entry_exit_pdays$pdays)

# convert to date format
sdi_cohort_entry_exit_pdays$sdi_cohort_entry <-  as_date(sdi_cohort_entry_exit_pdays$sdi_cohort_entry) 
sdi_cohort_entry_exit_pdays$sdi_cohort_exit <-  as_date(sdi_cohort_entry_exit_pdays$sdi_cohort_exit) 


## Adjust for pregnancy intervals based on pregnancy outcomes (max and min pdays and pyears) ---

# drop episodes that are not in cohort entry/exit period for patient 
preg_episodes_cohort <- left_join(preg_episodes, sdi_cohort_entry_exit_pdays, by = c("patid" = "patid"))
preg_episodes_valid <-preg_episodes_cohort %>% filter(eventdate >= sdi_cohort_entry & eventdate <= sdi_cohort_exit) %>% 
  dplyr::select(c(patid, eventdate, pregcat, episode_start_max, episode_end_max, episode_start_min, episode_end_min))

# calculate episode durations 
preg_episodes_duration_max <- preg_episodes_valid  %>% mutate(duration_max =(episode_end_max-episode_start_max)+1) 
preg_episodes_duration_max_min <- preg_episodes_duration_max %>%  mutate(duration_min = (episode_end_min-episode_start_min)+1)
preg_episodes_total_max <- preg_episodes_duration_max_min %>% group_by(patid) %>% tally(duration_max)
preg_episodes_total_min <- preg_episodes_duration_max_min %>% group_by(patid) %>% tally(duration_min)
preg_episodes_total_max <- rename(preg_episodes_total_max, preg_total_max = n)
preg_episodes_total_min <- rename(preg_episodes_total_min, preg_total_min = n)

# recalculate pdays and pyears (max and min based on max and min preg durations)
sdi_cohort_pregnancy_episodes <- left_join(sdi_cohort_entry_exit_pdays, preg_episodes_total_max,
                                           by = c("patid" = "patid")) %>%
  left_join(preg_episodes_total_min, by = c("patid" = "patid"))
sdi_cohort_pregnancy_episodes$preg_total_max <- sdi_cohort_pregnancy_episodes$preg_total_max %>% replace_na(0)
sdi_cohort_pregnancy_episodes$preg_total_min <- sdi_cohort_pregnancy_episodes$preg_total_min %>% replace_na(0)
sdi_cohort_pregnancy_episodes_pdays_pyears <- sdi_cohort_pregnancy_episodes %>% 
  mutate(sdi_pdays_max = (sdi_pdays-preg_total_max)) %>%
  mutate(sdi_pdays_min = (sdi_pdays-preg_total_min)) %>%
  mutate(sdi_pyears_max = (sdi_pdays_max/365.25)) %>% 
  mutate(sdi_pyears_min = (sdi_pdays_min/365.25))
sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pyears_max <- as.numeric(sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pyears_max)
sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pyears_min <- as.numeric(sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pyears_min)
sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pdays_max <- as.numeric(sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pdays_max)
sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pdays_min <- as.numeric(sdi_cohort_pregnancy_episodes_pdays_pyears$sdi_pdays_min)


## Next step not applicable to SDI 
## Adjust for conditions that have short intervals that affect pdays/pyears ---

# dplyr::select final variables for cohort --- 
sdi_cohort_final_washout <- dplyr::select(sdi_cohort_pregnancy_episodes_pdays_pyears, c(patid, pracid, prac_region, 
                                                                                dob, yob, patimd, pracimd, imd, migrant_status, migcertainty, ethnicat, ethnicat6, data_start, data_end,
                                                                                sdi_cohort_entry, sdi_cohort_exit, sdi_pdays_max,sdi_pyears_max, sdi_pdays_min, sdi_pyears_min )) 

save(sdi_cohort_final_washout, file = "filepath")

## SDI - Exact matched cohort 4:1 ----------

load(file = "filepath")

# Derive age at cohort entry
sdi_cohort_final_washout <- sdi_cohort_final_washout %>%
  mutate(dob = lubridate::ymd(yob,truncated=2L))

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

sdi_cohort_final_washout <- sdi_cohort_final_washout %>%
  mutate(age_cohort_entry= calc_age(dob, sdi_cohort_entry))

# Derive year of cohort entry
sdi_cohort_final_washout <- sdi_cohort_final_washout %>%
  mutate(year_cohort_entry = lubridate::year(ymd(sdi_cohort_entry)))

# Drop unused levels for migrant status - added earlier in script, delete after re-running rest of script
levels(sdi_cohort_final_washout$migrant_status)
sdi_cohort_final_washout$migrant_status <- droplevels(sdi_cohort_final_washout$migrant_status)
levels(sdi_cohort_final_washout$migrant_status)

# turn migrant status into binary integer
sdi_cohort_final_washout <- sdi_cohort_final_washout %>%
  mutate(migrant_status_binary = as.numeric(migrant_status)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 1, 0)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 2, 1))

# select variables needed
sdi_cohort_final_washout_2 <- dplyr::select(sdi_cohort_final_washout, c(patid, migrant_status_binary,year_cohort_entry, age_cohort_entry, prac_region))

# Sample - for testing only
# sdi_cohort_final_2 <- sample_n(sdi_cohort_final_2, size=500000, replace=FALSE)

# function requires data.table

library(data.table)

# Turn df into data table

setDT(sdi_cohort_final_washout_2)

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
sdi_cohort_final_washout_2[, age_year_region := paste0(age_cohort_entry, '-', year_cohort_entry, '-', prac_region)]

# create matched dataset

matched_data_4to1 <- smatch(sdi_cohort_final_washout_2, 'migrant_status_binary', 'age_year_region')

# check balance
dcast(matched_data_4to1, age_year_region ~ migrant_status_binary, value.var = 'age_cohort_entry', fun.aggregate = length)

# Turn back into dataframe
matched_sdi_cohort_final_4to1 <- as.data.frame(matched_data_4to1)

# Rejoin matched cohort to rest of dataset
matched_sdi_cohort_final_4to1 <- dplyr::select(matched_sdi_cohort_final_4to1, -c(prac_region, year_cohort_entry, age_cohort_entry,migrant_status_binary))
exact_match_sdi_cohort_final_4to1 <- left_join(matched_sdi_cohort_final_4to1, sdi_cohort_final_washout, by = c("patid"="patid"))

# Check 4:1 for whole cohort
exact_match_sdi_cohort_final_4to1 %>% group_by(migrant_status) %>% count() 

#Save cohort
save(exact_match_sdi_cohort_final_4to1, file = "filepath") 


## SDI - Exact matched 4:1 counts file ---------------------

load(file = "filepath")

sdi_presc <- contr_presc_clean %>% filter(contrpresccat == "Implant") 

## Change prescribing events that are same category on same day, to one event ---

n_distinct(sdi_presc) == count(distinct(sdi_presc, patid, eventdate, contrpresccat,  .keep_all = TRUE))  
sdipresc_oneeventperdate <- sdi_presc %>% distinct(patid, eventdate, contrpresccat,  .keep_all = TRUE)
count(sdipresc_oneeventperdate) 

## Drop events that are not in sdi cohort entry/exit time ---

sdipresc_indates <- left_join(sdipresc_oneeventperdate, exact_match_sdi_cohort_final_4to1,
                              by = c("patid" = "patid"))
sdipresc_indates_2 <- sdipresc_indates %>% 
  filter(eventdate >= sdi_cohort_entry & eventdate <= sdi_cohort_exit) 
count(sdipresc_indates_2) 


## Drop events that fall into pregnancy durations  ---

# Assign row ids to preg outcome episodes & convert to wide dataset
preg_episodes_valid_vars <- dplyr::select(preg_episodes_valid, 
                                          c(patid,eventdate,episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_valid_eventids <- preg_episodes_valid_vars %>% group_by(patid) %>% mutate(eventid = row_number())
summary(preg_episodes_valid_eventids) 
preg_episodes_valid_wide <- pivot_wider(preg_episodes_valid_eventids, names_from = eventid, 
                                        values_from = c(eventdate, episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_valid_wide <- as.data.frame(preg_episodes_valid_wide)
count(preg_episodes_valid_wide) 
count(distinct(preg_episodes_valid_eventids)) 



# Join wide pregnancy to cu iud prescriptions 
sdipresc_preg <- left_join(sdipresc_indates_2, preg_episodes_valid_wide , 
                           by = c("patid" = "patid"))

# Drop cu iud presc events that fall into pregn max intervals
sdipresc_notinpreg_max <- sdipresc_preg %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_max_1 & eventdate <= episode_end_max_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_max_2 & eventdate <= episode_end_max_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_max_3 & eventdate <= episode_end_max_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_max_4 & eventdate <= episode_end_max_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_max_5 & eventdate <= episode_end_max_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_max_6 & eventdate <= episode_end_max_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_max_7 & eventdate <= episode_end_max_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_max_8 & eventdate <= episode_end_max_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_max_9 & eventdate <= episode_end_max_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_max_10 & eventdate <= episode_end_max_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_max_11 & eventdate <= episode_end_max_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no") 

# Drop cu iud presc events that fall into pregn min intervals
sdipresc_notinpreg_min <- sdipresc_preg %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_min_1 & eventdate <= episode_end_min_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_min_2 & eventdate <= episode_end_min_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_min_3 & eventdate <= episode_end_min_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_min_4 & eventdate <= episode_end_min_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_min_5 & eventdate <= episode_end_min_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_min_6 & eventdate <= episode_end_min_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_min_7 & eventdate <= episode_end_min_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_min_8 & eventdate <= episode_end_min_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_min_9 & eventdate <= episode_end_min_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_min_10 & eventdate <= episode_end_min_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_min_11 & eventdate <= episode_end_min_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no")


# Final sdi presc files with pregn max duration assumption and pregn min duration assumption
sdipresc_pregmaxassumption <- dplyr::select(sdipresc_notinpreg_max, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                      issueseq))
sdipresc_pregminassumption <- dplyr::select(sdipresc_notinpreg_min, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                      issueseq))
count(sdipresc_pregmaxassumption) 
count(sdipresc_pregminassumption) 


## Drop events that fall into conditions with short intervals based on preg max/min assumption ---
# step not applicable to SDI

## Create annual sdi presc counts (preg max and min assumption) ---


# create sdi cohort file with line for each year that patient is in cohort 
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(exact_match_sdi_cohort_final_4to1)))
colnames(year_variable) <- 'eventyear'
sdi_annual_cohort <- exact_match_sdi_cohort_final_4to1 %>%
  slice(rep(1:n(), each=10))
sdi_annual_cohort$eventyear <- year_variable$eventyear
sdi_annual_cohort <- filter(sdi_annual_cohort, eventyear >= year(sdi_cohort_entry) & eventyear <= year(sdi_cohort_exit))

# Create annual sdi presc episodes count 

sdipresc_pregmaxassumption_annual_counts <- sdipresc_pregmaxassumption %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(sdipresc_n_pregmax = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, sdipresc_n_pregmax) %>%
  distinct()

sdipresc_pregminassumption_annual_counts <- sdipresc_pregminassumption %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(sdipresc_n_pregmin = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, sdipresc_n_pregmin) %>%
  distinct()

# Join annual cohort file to annual sdi presc files, change NAs to 0

sdipresc_annual_counts <- sdi_annual_cohort %>%
  left_join(sdipresc_pregmaxassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(sdipresc_pregminassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear"))
sdipresc_annual_counts$sdipresc_n_pregmax <- sdipresc_annual_counts$sdipresc_n_pregmax %>% replace_na(0)
sdipresc_annual_counts$sdipresc_n_pregmin <- sdipresc_annual_counts$sdipresc_n_pregmin %>% replace_na(0)

# Add age during event year and age category 

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

sdipresc_annual_counts_age <- sdipresc_annual_counts %>% mutate(age_date= eventyear) 
sdipresc_annual_counts_age$age_date <- as.character(sdipresc_annual_counts_age$age_date)
sdipresc_annual_counts_age$age_date <- paste("01-01-", sdipresc_annual_counts_age$age_date, sep="")
head(sdipresc_annual_counts_age$age_date)
sdipresc_annual_counts_age$age_date <- dmy(sdipresc_annual_counts_age$age_date)
head(sdipresc_annual_counts_age$age_date)
class(sdipresc_annual_counts_age$age_date)
sdipresc_annual_counts_age <- sdipresc_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(sdipresc_annual_counts_age$eventyear_age)
max(sdipresc_annual_counts_age$eventyear_age) 
sdipresc_annual_counts_age_50 <- sdipresc_annual_counts_age %>% filter(eventyear_age == 50)
sdipresc_annual_counts_age_final <- sdipresc_annual_counts_age %>% 
  filter(eventyear_age != 50) %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
sdipresc_annual_counts_age_final$eventyear_agecat <- factor(sdipresc_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                            labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Final file
exact_match_sdipresc_annual_counts_final_4to1 <- sdipresc_annual_counts_age_final %>% dplyr::select(-c(age_date))

## Save and load final file
save(exact_match_sdipresc_annual_counts_final_4to1, file = "filepath")
load(file = "filepath")


## Set up data for stratifying by time variant variable (i.e add pdays per year max and min with  preg max and pregmin removed per year) ---

# Add year durations
sdipresc_annual_counts_final_extra <- exact_match_sdipresc_annual_counts_final_4to1 %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
sdipresc_annual_counts_final_extra$start_eventyear <- as_date(sdipresc_annual_counts_final_extra$start_eventyear)
sdipresc_annual_counts_final_extra$end_eventyear <- as_date(sdipresc_annual_counts_final_extra$end_eventyear)
sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(sdi_cohort_entry <= start_eventyear, start_eventyear,sdi_cohort_entry))
sdipresc_annual_counts_final_extra$t0_ey <- as_date(sdipresc_annual_counts_final_extra$t0_ey)
sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(sdi_cohort_exit >= end_eventyear, end_eventyear,sdi_cohort_exit))
sdipresc_annual_counts_final_extra$t1_ey <- as_date(sdipresc_annual_counts_final_extra$t1_ey)
sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) 
sdipresc_annual_counts_final_extra$pdays_ey <- as.numeric(sdipresc_annual_counts_final_extra$pdays_ey)

# Calc duration of Pregn in each year based on preg max/min assumption

x <- preg_episodes_valid %>% filter(year(episode_end_max) == year(episode_start_max)) %>% 
  mutate(year = year(episode_end_max), pregduration_max = (episode_end_max - episode_start_max)+1) %>% 
  dplyr::select(patid, year, pregduration_max)
x$pregduration_max <- as.numeric(x$pregduration_max)

y <- preg_episodes_valid %>% filter(year(episode_end_max) != year(episode_start_max)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_max = ifelse(start_end == "episode_start_max", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_max", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_max)

preg_annual_durations_max <- rbind(x,y) 
preg_annual_durations_max_grouped <- preg_annual_durations_max %>% group_by(patid, year) %>% tally(pregduration_max)
preg_annual_durations_max_grouped <- rename(preg_annual_durations_max_grouped, pregduration_max = n)

x <- preg_episodes_valid %>% filter(year(episode_end_min) == year(episode_start_min)) %>% 
  mutate(year = year(episode_end_min), pregduration_min = (episode_end_min - episode_start_min)+1) %>% 
  dplyr::select(patid, year, pregduration_min)
x$pregduration_min <- as.numeric(x$pregduration_min)

y <- preg_episodes_valid %>% filter(year(episode_end_min) != year(episode_start_min)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_min = ifelse(start_end == "episode_start_min", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_min", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_min)

preg_annual_durations_min <- rbind(x,y) 
preg_annual_durations_min_grouped <- preg_annual_durations_min %>% group_by(patid, year) %>% tally(pregduration_min) 
preg_annual_durations_min_grouped <- rename(preg_annual_durations_min_grouped, pregduration_min = n)

## subtract preg duratins from pdays_ey (max and min)

sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>% 
  left_join(preg_annual_durations_max_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>%  
  left_join(preg_annual_durations_min_grouped, by = c("patid" = "patid", "eventyear" = "year")) 


sdipresc_annual_counts_final_extra$pregduration_max <- sdipresc_annual_counts_final_extra$pregduration_max %>% replace_na(0)
sdipresc_annual_counts_final_extra$pregduration_min <- sdipresc_annual_counts_final_extra$pregduration_min %>% replace_na(0)

sdipresc_annual_counts_final_extra <- sdipresc_annual_counts_final_extra %>% 
  mutate(pdays_ey_pregmax = pdays_ey-(pregduration_max)) %>% 
  mutate(pdays_ey_pregmin = pdays_ey-(pregduration_min)) %>% 
  mutate(pyears_ey_pregmax=pdays_ey_pregmax/365.25) %>% 
  mutate(pyears_ey_pregmin=pdays_ey_pregmin/365.25)


## Save & load annual counts final extra (for glms) ---

exact_match_sdipresc_annual_counts_final_extra_4to1 <- sdipresc_annual_counts_final_extra %>% dplyr::select(patid, eventyear, pracid, prac_region, dob, yob, patimd, pracimd, imd, 
                                                                                           migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                           data_start, data_end, sdi_cohort_entry, sdi_cohort_exit, sdi_pdays_max, 
                                                                                           sdi_pdays_min, sdi_pyears_min, sdi_pyears_min, eventyear_age, eventyear_agecat, 
                                                                                           sdipresc_n_pregmax, sdipresc_n_pregmin, pdays_ey_pregmax, pdays_ey_pregmin, 
                                                                                           pyears_ey_pregmax, pyears_ey_pregmin)

save(exact_match_sdipresc_annual_counts_final_extra_4to1, file = "filepath")
load(file = "filepath")

## POI - cohort with 365D washout  -------------------------------------------------------------------------------------------

load(file = "filepath") 

## Adjust for conditions that affect cohort entry/exit ---

# dplyr::select incident breast ca events
count(breast_ca_clean) == count(distinct(breast_ca_clean, patid))
incidentevents_breast_ca <- breast_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_breast_ca <- as.data.frame(incidentevents_breast_ca)
count(incidentevents_breast_ca) == count(distinct(breast_ca_clean, patid)) 

# dplyr::select incident vascular dx events
count(vasculardx_clean) == count(distinct(vasculardx_clean, patid)) 
incidentevents_vasculardx <- vasculardx_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_vasculardx <- as.data.frame(incidentevents_vasculardx)
count(incidentevents_vasculardx) == count(distinct(vasculardx_clean, patid)) 

# dplyr::select incident IHD chd events
count(IHD_chd_clean) == count(distinct(IHD_chd_clean, patid))
incidentevents_IHD_chd <- IHD_chd_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_IHD_chd <- as.data.frame(incidentevents_IHD_chd)
count(incidentevents_IHD_chd) == count(distinct(IHD_chd_clean, patid)) 

# dplyr::select incident IHD epicardial events
count(IHD_epicardial_clean) == count(distinct(IHD_epicardial_clean, patid)) 
incidentevents_IHD_epicardial <- IHD_epicardial_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_IHD_epicardial <- as.data.frame(incidentevents_IHD_epicardial)
count(incidentevents_IHD_epicardial) == count(distinct(IHD_epicardial_clean, patid)) 

# dplyr::select stroke ich events
count(stroke_ich_clean) == count(distinct(stroke_ich_clean, patid)) 
incidentevents_stroke_ich <- stroke_ich_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_stroke_ich <- as.data.frame(incidentevents_stroke_ich)
count(incidentevents_stroke_ich) == count(distinct(stroke_ich_clean, patid))

# dplyr::select stroke isch events
count(stroke_isch_clean) == count(distinct(stroke_isch_clean, patid))
incidentevents_stroke_isch <- stroke_isch_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_stroke_isch <- as.data.frame(incidentevents_stroke_isch)
count(incidentevents_stroke_isch) == count(distinct(stroke_isch_clean, patid)) 

# dplyr::select stroke nos events
count(stroke_nos_clean) == count(distinct(stroke_nos_clean, patid))
incidentevents_stroke_nos <- stroke_nos_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_stroke_nos <- as.data.frame(incidentevents_stroke_nos)
count(incidentevents_stroke_nos) == count(distinct(stroke_nos_clean, patid)) 

# dplyr::select stroke nos events
count(stroke_TIA_clean) == count(distinct(stroke_TIA_clean, patid)) 
incidentevents_stroke_TIA <- stroke_TIA_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_stroke_TIA <- as.data.frame(incidentevents_stroke_TIA)
count(incidentevents_stroke_TIA) == count(distinct(stroke_TIA_clean, patid)) 

# dplyr::select incident hep adenoma 
count(hep_adenoma_clean) == count(distinct(hep_adenoma_clean, patid)) 
incidentevents_hep_adenoma <- hep_adenoma_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_adenoma <- as.data.frame(incidentevents_hep_adenoma)
count(incidentevents_hep_adenoma) == count(distinct(hep_adenoma_clean, patid)) 

# dplyr::select incident hep ca
count(hep_ca_clean) == count(distinct(hep_ca_clean, patid)) 
incidentevents_hep_ca <- hep_ca_clean %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incidentevents_hep_ca <- as.data.frame(incidentevents_hep_ca)
count(incidentevents_hep_ca) == count(distinct(hep_ca_clean, patid))

# Combine patients with incident condition affecting cohort entry/exit and dplyr::select earliest date of these
conditions <- full_join(incidentevents_breast_ca, incidentevents_vasculardx, 
                        by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_IHD_chd, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_IHD_epicardial, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_stroke_ich, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_stroke_isch, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_stroke_nos, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) %>% 
  full_join(incidentevents_stroke_TIA, 
            by = c("patid" = "patid", "eventdate" = "eventdate")) 
incident_conditions <- conditions %>% arrange(patid, eventdate) %>%
  group_by(patid) %>% 
  slice(1L)
incident_conditions <- as.data.frame(incident_conditions)
count(incident_conditions) == count(distinct(incident_conditions)) 
incident_conditions <- rename(incident_conditions, conditions_eventdate = eventdate)

# Amend cohort entry date and drop patients who never enter cohort
poi_cohort <- left_join(contraception_cohort_final_washout, incident_conditions, 
                        by = c("patid" = "patid"))
poi_cohort_entry <-  poi_cohort %>% 
  mutate(poi_cohort_entry = ifelse(is.na(conditions_eventdate), contr_cohort_entry,
                                   ifelse(conditions_eventdate <= contr_cohort_entry, NA, contr_cohort_entry)))
summary(is.na(poi_cohort_entry$poi_cohort_entry)) 
poi_cohort_entry_eligible <- poi_cohort_entry %>% filter(!is.na(poi_cohort_entry))

# Amend cohort exit dates
poi_cohort_entry_exit <- poi_cohort_entry_eligible %>% 
  mutate(poi_cohort_exit = ifelse(is.na(conditions_eventdate), contr_cohort_exit,
                                  ifelse(conditions_eventdate < contr_cohort_exit, conditions_eventdate, contr_cohort_exit)))
summary(is.na(poi_cohort_entry_exit$contr_cohort_exit)) 

# Recalculate pdays and pyears based on those eligible after adjusting for cervical.ca.endomca/malign gtd
poi_cohort_entry_exit_pdays <- poi_cohort_entry_exit %>%
  mutate(poi_pdays = (poi_cohort_exit-poi_cohort_entry)+1) 
poi_cohort_entry_exit_pdays$pdays <- as.numeric(poi_cohort_entry_exit_pdays$pdays)

# convert to date format
poi_cohort_entry_exit_pdays$poi_cohort_entry <-  as_date(poi_cohort_entry_exit_pdays$poi_cohort_entry) 
poi_cohort_entry_exit_pdays$poi_cohort_exit <-  as_date(poi_cohort_entry_exit_pdays$poi_cohort_exit) 


## Adjust for pregnancy intervals based on pregnancy outcomes (max and min pdays and pyears) ---

# drop episodes that are not in cohort entry/exit period for patient 
preg_episodes_cohort <- left_join(preg_episodes, poi_cohort_entry_exit_pdays, by = c("patid" = "patid"))
preg_episodes_valid <-preg_episodes_cohort %>% filter(eventdate >= poi_cohort_entry & eventdate <= poi_cohort_exit) %>% 
  dplyr::select(c(patid, eventdate, pregcat, episode_start_max, episode_end_max, episode_start_min, episode_end_min))

# calculate episode durations 
preg_episodes_duration_max <- preg_episodes_valid  %>% mutate(duration_max =(episode_end_max-episode_start_max)+1) 
preg_episodes_duration_max_min <- preg_episodes_duration_max %>%  mutate(duration_min = (episode_end_min-episode_start_min)+1)
preg_episodes_total_max <- preg_episodes_duration_max_min %>% group_by(patid) %>% tally(duration_max)
preg_episodes_total_min <- preg_episodes_duration_max_min %>% group_by(patid) %>% tally(duration_min)
preg_episodes_total_max <- rename(preg_episodes_total_max, preg_total_max = n)
preg_episodes_total_min <- rename(preg_episodes_total_min, preg_total_min = n)

# recalculate pdays and pyears (max and min based on max and min preg durations)
poi_cohort_pregnancy_episodes <- left_join(poi_cohort_entry_exit_pdays, preg_episodes_total_max,
                                           by = c("patid" = "patid")) %>%
  left_join(preg_episodes_total_min, by = c("patid" = "patid"))
poi_cohort_pregnancy_episodes$preg_total_max <- poi_cohort_pregnancy_episodes$preg_total_max %>% replace_na(0)
poi_cohort_pregnancy_episodes$preg_total_min <- poi_cohort_pregnancy_episodes$preg_total_min %>% replace_na(0)
poi_cohort_pregnancy_episodes_pdays_pyears <- poi_cohort_pregnancy_episodes %>% 
  mutate(poi_pdays_max = (poi_pdays-preg_total_max)) %>%
  mutate(poi_pdays_min = (poi_pdays-preg_total_min)) %>%
  mutate(poi_pyears_max = (poi_pdays_max/365.25)) %>% 
  mutate(poi_pyears_min = (poi_pdays_min/365.25))
poi_cohort_pregnancy_episodes_pdays_pyears$poi_pyears_max <- as.numeric(poi_cohort_pregnancy_episodes_pdays_pyears$poi_pyears_max)
poi_cohort_pregnancy_episodes_pdays_pyears$poi_pyears_min <- as.numeric(poi_cohort_pregnancy_episodes_pdays_pyears$poi_pyears_min)
poi_cohort_pregnancy_episodes_pdays_pyears$poi_pdays_max <- as.numeric(poi_cohort_pregnancy_episodes_pdays_pyears$poi_pdays_max)
poi_cohort_pregnancy_episodes_pdays_pyears$poi_pdays_min <- as.numeric(poi_cohort_pregnancy_episodes_pdays_pyears$poi_pdays_min)


## Next step not applicable to POI
## Adjust for conditions that have short intervals that affect pdays/pyears ---


# dplyr::select final variables for cohort --- 
poi_cohort_final_washout <- dplyr::select(poi_cohort_pregnancy_episodes_pdays_pyears, c(patid, pracid, prac_region,
                                                                                dob, yob, patimd, pracimd, imd, migrant_status, migcertainty, ethnicat, ethnicat6, data_start, data_end,
                                                                                poi_cohort_entry, poi_cohort_exit, poi_pdays_max, poi_pdays_min, poi_pyears_max, poi_pyears_min )) 




save(poi_cohort_final_washout, file = "filepath")

## POI- exact matched cohort 4:1 ----------

load(file = "filepath")

# Derive age at cohort entry
poi_cohort_final_washout <- poi_cohort_final_washout %>%
  mutate(dob = lubridate::ymd(yob,truncated=2L))

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

poi_cohort_final_washout <- poi_cohort_final_washout %>%
  mutate(age_cohort_entry= calc_age(dob, poi_cohort_entry))

# Derive year of cohort entry
poi_cohort_final_washout <- poi_cohort_final_washout %>%
  mutate(year_cohort_entry = lubridate::year(ymd(poi_cohort_entry)))

# Drop unused levels for migrant status - added earlier in script, delete after re-running rest of script
levels(poi_cohort_final_washout$migrant_status)
poi_cohort_final_washout$migrant_status <- droplevels(poi_cohort_final_washout$migrant_status)
levels(poi_cohort_final_washout$migrant_status)

# turn migrant status into binary integer
poi_cohort_final_washout <- poi_cohort_final_washout %>%
  mutate(migrant_status_binary = as.numeric(migrant_status)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 1, 0)) %>%
  mutate(migrant_status_binary = replace(migrant_status_binary, migrant_status_binary == 2, 1))

# select variables needed
poi_cohort_final_washout_2 <- dplyr::select(poi_cohort_final_washout, c(patid, migrant_status_binary,year_cohort_entry, age_cohort_entry, prac_region))

# Sample - for testing only
# poi_cohort_final_2 <- sample_n(poi_cohort_final_2, size=500000, replace=FALSE)

# function requires data.table

library(data.table)

# Turn df into data table

setDT(poi_cohort_final_washout_2)

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
poi_cohort_final_washout_2[, age_year_region := paste0(age_cohort_entry, '-', year_cohort_entry, '-', prac_region)]

# create matched dataset

matched_data_4to1 <- smatch(poi_cohort_final_washout_2, 'migrant_status_binary', 'age_year_region')

# check balance
dcast(matched_data_4to1, age_year_region ~ migrant_status_binary, value.var = 'age_cohort_entry', fun.aggregate = length)

# Turn back into dataframe
matched_poi_cohort_final_4to1 <- as.data.frame(matched_data_4to1)

# Rejoin matched cohort to rest of dataset
matched_poi_cohort_final_4to1 <- dplyr::select(matched_poi_cohort_final_4to1, -c(prac_region, year_cohort_entry, age_cohort_entry,migrant_status_binary))
exact_match_poi_cohort_final_4to1 <- left_join(matched_poi_cohort_final_4to1, poi_cohort_final_washout, by = c("patid"="patid"))

# Check 4:1 for whole cohort
exact_match_poi_cohort_final_4to1 %>% group_by(migrant_status) %>% count() #true: nm 271296,  m 67824

#Save cohort
save(exact_match_poi_cohort_final_4to1, file = "cleaned_files/exact_match_poi_cohort_final_4to1.Rdata") #  446985 obs of 25 vars


## POI - Exact matched 4:1 counts file ---------------------

load(file = "cleaned_files/exact_match_poi_cohort_final_4to1.Rdata")

poi_presc <- contr_presc_clean %>% filter(contrpresccat == "Progesterone-only injectable contraceptive") 


## Change prescribing events that are same category on same day, to one event ---

n_distinct(poi_presc) == count(distinct(poi_presc, patid, eventdate, contrpresccat,  .keep_all = TRUE))
poipresc_oneeventperdate <- poi_presc %>% distinct(patid, eventdate, contrpresccat,  .keep_all = TRUE)
count(poipresc_oneeventperdate) 

## Drop events that are not in poi cohort entry/exit time ---

poipresc_indates <- left_join(poipresc_oneeventperdate, exact_match_poi_cohort_final_4to1,
                              by = c("patid" = "patid"))
poipresc_indates_2 <- poipresc_indates %>% 
  filter(eventdate >= poi_cohort_entry & eventdate <= poi_cohort_exit) 
count(poipresc_indates_2) 


## Drop events that fall into pregnancy durations  ---

# Assign row ids to preg outcome episodes & convert to wide dataset
preg_episodes_valid_vars <- dplyr::select(preg_episodes_valid, 
                                          c(patid,eventdate,episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_valid_eventids <- preg_episodes_valid_vars %>% group_by(patid) %>% mutate(eventid = row_number())
summary(preg_episodes_valid_eventids) 
preg_episodes_valid_wide <- pivot_wider(preg_episodes_valid_eventids, names_from = eventid, 
                                        values_from = c(eventdate, episode_start_max, episode_end_max, episode_start_min, episode_end_min))
preg_episodes_valid_wide <- as.data.frame(preg_episodes_valid_wide)
count(preg_episodes_valid_wide)
count(distinct(preg_episodes_valid_eventids))  



# Join wide pregnancy to cu iud prescriptions 
poipresc_preg <- left_join(poipresc_indates_2, preg_episodes_valid_wide , 
                           by = c("patid" = "patid"))

# Drop cu iud presc events that fall into pregn max intervals
poipresc_notinpreg_max <- poipresc_preg %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_max_1 & eventdate <= episode_end_max_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_max_2 & eventdate <= episode_end_max_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_max_3 & eventdate <= episode_end_max_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_max_4 & eventdate <= episode_end_max_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_max_5 & eventdate <= episode_end_max_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_max_6 & eventdate <= episode_end_max_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_max_7 & eventdate <= episode_end_max_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_max_8 & eventdate <= episode_end_max_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_max_9 & eventdate <= episode_end_max_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_max_10 & eventdate <= episode_end_max_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_max_11 & eventdate <= episode_end_max_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no") 

# Drop cu iud presc events that fall into pregn min intervals
poipresc_notinpreg_min <- poipresc_preg %>% 
  mutate(to_drop1 = ifelse(is.na(eventdate_1), "no",
                           ifelse(eventdate >= episode_start_min_1 & eventdate <= episode_end_min_1, "yes", "no"))) %>% 
  filter(to_drop1 == "no") %>% 
  mutate(to_drop2 = ifelse(is.na(eventdate_2), "no",
                           ifelse(eventdate >= episode_start_min_2 & eventdate <= episode_end_min_2, "yes", "no"))) %>% 
  filter(to_drop2 == "no") %>% 
  mutate(to_drop3 = ifelse(is.na(eventdate_3), "no",
                           ifelse(eventdate >= episode_start_min_3 & eventdate <= episode_end_min_3, "yes", "no"))) %>% 
  filter(to_drop3 == "no") %>% 
  mutate(to_drop4 = ifelse(is.na(eventdate_4), "no",
                           ifelse(eventdate >= episode_start_min_4 & eventdate <= episode_end_min_4, "yes", "no"))) %>% 
  filter(to_drop4 == "no") %>% 
  mutate(to_drop5 = ifelse(is.na(eventdate_5), "no",
                           ifelse(eventdate >= episode_start_min_5 & eventdate <= episode_end_min_5, "yes", "no"))) %>% 
  filter(to_drop5 == "no") %>% 
  mutate(to_drop6 = ifelse(is.na(eventdate_6), "no",
                           ifelse(eventdate >= episode_start_min_6 & eventdate <= episode_end_min_6, "yes", "no"))) %>% 
  filter(to_drop6 == "no") %>% 
  mutate(to_drop7 = ifelse(is.na(eventdate_7), "no",
                           ifelse(eventdate >= episode_start_min_7 & eventdate <= episode_end_min_7, "yes", "no"))) %>% 
  filter(to_drop7 == "no") %>% 
  mutate(to_drop8 = ifelse(is.na(eventdate_8), "no",
                           ifelse(eventdate >= episode_start_min_8 & eventdate <= episode_end_min_8, "yes", "no"))) %>% 
  filter(to_drop8 == "no") %>% 
  mutate(to_drop9 = ifelse(is.na(eventdate_9), "no",
                           ifelse(eventdate >= episode_start_min_9 & eventdate <= episode_end_min_9, "yes", "no"))) %>% 
  filter(to_drop9 == "no") %>% 
  mutate(to_drop10 = ifelse(is.na(eventdate_10), "no",
                            ifelse(eventdate >= episode_start_min_10 & eventdate <= episode_end_min_10, "yes", "no"))) %>% 
  filter(to_drop10 == "no") %>% 
  mutate(to_drop11 = ifelse(is.na(eventdate_11), "no",
                            ifelse(eventdate >= episode_start_min_11 & eventdate <= episode_end_min_11, "yes", "no"))) %>% 
  filter(to_drop11 == "no")


# Final poi presc files with pregn max duration assumption and pregn min duration assumption
poipresc_pregmaxassumption <- dplyr::select(poipresc_notinpreg_max, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                      issueseq))
poipresc_pregminassumption <- dplyr::select(poipresc_notinpreg_min, c(patid, eventdate, prodcode, qty, numdays, numpacks, 
                                                                      issueseq))
count(poipresc_pregmaxassumption) 
count(poipresc_pregminassumption) 


## Drop events that fall into conditions with short intervals based on preg max/min assumption ---
# step not applicable to poi

## Create annual poi presc counts (preg max and min assumption) ---


# create poi cohort file with line for each year that patient is in cohort 
year_variable <- as.data.frame(rep(seq(2009,2018,1), times = nrow(exact_match_poi_cohort_final_4to1)))
colnames(year_variable) <- 'eventyear'
poi_annual_cohort <- exact_match_poi_cohort_final_4to1 %>%
  slice(rep(1:n(), each=10))
poi_annual_cohort$eventyear <- year_variable$eventyear
poi_annual_cohort <- filter(poi_annual_cohort, eventyear >= year(poi_cohort_entry) & eventyear <= year(poi_cohort_exit))

# Create annual poi presc episodes count 

poipresc_pregmaxassumption_annual_counts <- poipresc_pregmaxassumption %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(poipresc_n_pregmax = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, poipresc_n_pregmax) %>%
  distinct()

poipresc_pregminassumption_annual_counts <- poipresc_pregminassumption %>%
  mutate(eventyear = year(eventdate)) %>%
  group_by(eventyear) %>%
  add_count(patid) %>%
  rename(poipresc_n_pregmin = n) %>%
  arrange(patid) %>% 
  dplyr::select(patid, eventyear, poipresc_n_pregmin) %>%
  distinct()

# Join annual cohort file to annual poi presc files, change NAs to 0

poipresc_annual_counts <- poi_annual_cohort %>%
  left_join(poipresc_pregmaxassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear")) %>%
  left_join(poipresc_pregminassumption_annual_counts, by = c("patid" = "patid", "eventyear" = "eventyear"))
poipresc_annual_counts$poipresc_n_pregmax <- poipresc_annual_counts$poipresc_n_pregmax %>% replace_na(0)
poipresc_annual_counts$poipresc_n_pregmin <- poipresc_annual_counts$poipresc_n_pregmin %>% replace_na(0)

# Add age during event year and age category 

calc_age <- function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period <- as.period(interval(birthDate, refDate),unit = "year")
  period$year
}

poipresc_annual_counts_age <- poipresc_annual_counts %>% mutate(age_date= eventyear) 
poipresc_annual_counts_age$age_date <- as.character(poipresc_annual_counts_age$age_date)
poipresc_annual_counts_age$age_date <- paste("01-01-", poipresc_annual_counts_age$age_date, sep="")
head(poipresc_annual_counts_age$age_date)
poipresc_annual_counts_age$age_date <- dmy(poipresc_annual_counts_age$age_date)
head(poipresc_annual_counts_age$age_date)
class(poipresc_annual_counts_age$age_date)
poipresc_annual_counts_age <- poipresc_annual_counts_age %>%
  mutate(eventyear_age = calc_age(dob, age_date))
min(poipresc_annual_counts_age$eventyear_age)
max(poipresc_annual_counts_age$eventyear_age) # check if 50 then do below if necessary 
poipresc_annual_counts_age_50 <- poipresc_annual_counts_age %>% filter(eventyear_age == 50)#  drop because DOB is 01 01 and turns exactly 50 that year 
poipresc_annual_counts_age_final <- poipresc_annual_counts_age %>% 
  filter(eventyear_age != 50) %>% 
  mutate(eventyear_agecat = ifelse(eventyear_age <= 19 & eventyear_age >=15, 0,
                                   ifelse(eventyear_age <=24 & eventyear_age >= 20, 1,
                                          ifelse(eventyear_age <= 29 & eventyear_age >= 25, 2,
                                                 ifelse(eventyear_age <= 34 & eventyear_age >= 30, 3,
                                                        ifelse(eventyear_age <= 39 & eventyear_age >= 35, 4,
                                                               ifelse(eventyear_age <= 44 & eventyear_age >= 40, 5, 
                                                                      ifelse(eventyear_age <= 49 & eventyear_age >= 45, 6, NA))))))))
poipresc_annual_counts_age_final$eventyear_agecat <- factor(poipresc_annual_counts_age_final$eventyear_agecat, levels = 0:6,
                                                            labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))


# Final file
exact_match_poipresc_annual_counts_final_4to1 <- poipresc_annual_counts_age_final %>% dplyr::select(-c(age_date))

## Save and load final file
save(exact_match_poipresc_annual_counts_final_4to1, file = "cleaned_files/exact_match_poipresc_annual_counts_final_4to1.Rdata")
load(file = "cleaned_files/exact_match_poipresc_annual_counts_final_4to1.Rdata")


## Set up data for stratifying by time variant variable (i.e add pdays per year max and min with  preg max and pregmin removed per year) ---

# Add year durations
poipresc_annual_counts_final_extra <- exact_match_poipresc_annual_counts_final_4to1 %>% 
  mutate(start_eventyear = paste(eventyear, "-01-01", sep = "")) %>% 
  mutate(end_eventyear = paste(eventyear, "-12-31", sep=""))
poipresc_annual_counts_final_extra$start_eventyear <- as_date(poipresc_annual_counts_final_extra$start_eventyear)
poipresc_annual_counts_final_extra$end_eventyear <- as_date(poipresc_annual_counts_final_extra$end_eventyear)
poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>% 
  mutate(t0_ey = ifelse(poi_cohort_entry <= start_eventyear, start_eventyear,poi_cohort_entry))
poipresc_annual_counts_final_extra$t0_ey <- as_date(poipresc_annual_counts_final_extra$t0_ey)
poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>% 
  mutate(t1_ey = ifelse(poi_cohort_exit >= end_eventyear, end_eventyear,poi_cohort_exit))
poipresc_annual_counts_final_extra$t1_ey <- as_date(poipresc_annual_counts_final_extra$t1_ey)
poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>%
  mutate(pdays_ey = (t1_ey-t0_ey+1)) 
poipresc_annual_counts_final_extra$pdays_ey <- as.numeric(poipresc_annual_counts_final_extra$pdays_ey)

# Calc duration of Pregn in each year based on preg max/min assumption

x <- preg_episodes_valid %>% filter(year(episode_end_max) == year(episode_start_max)) %>% 
  mutate(year = year(episode_end_max), pregduration_max = (episode_end_max - episode_start_max)+1) %>% 
  dplyr::select(patid, year, pregduration_max)
x$pregduration_max <- as.numeric(x$pregduration_max)

y <- preg_episodes_valid %>% filter(year(episode_end_max) != year(episode_start_max)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_max = ifelse(start_end == "episode_start_max", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_max", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_max)

preg_annual_durations_max <- rbind(x,y) 
preg_annual_durations_max_grouped <- preg_annual_durations_max %>% group_by(patid, year) %>% tally(pregduration_max) 
preg_annual_durations_max_grouped <- rename(preg_annual_durations_max_grouped, pregduration_max = n)

x <- preg_episodes_valid %>% filter(year(episode_end_min) == year(episode_start_min)) %>% 
  mutate(year = year(episode_end_min), pregduration_min = (episode_end_min - episode_start_min)+1) %>% 
  dplyr::select(patid, year, pregduration_min)
x$pregduration_min <- as.numeric(x$pregduration_min)

y <- preg_episodes_valid %>% filter(year(episode_end_min) != year(episode_start_min)) %>%
  gather(., key = "start_end", value="date", -patid, -eventdate, -pregcat, -episode_start_min, -episode_end_min ) 
y  <- y %>% mutate(year = year(date)) %>% 
  mutate(pregduration_min = ifelse(start_end == "episode_start_min", (ymd(paste(year+1,"-01-01")) - date), 
                                   ifelse(start_end == "episode_end_min", (date - ymd(paste(year,"-01-01"))), NA))) %>% 
  dplyr::select(patid, year, pregduration_min)

preg_annual_durations_min <- rbind(x,y)
preg_annual_durations_min_grouped <- preg_annual_durations_min %>% group_by(patid, year) %>% tally(pregduration_min) 
preg_annual_durations_min_grouped <- rename(preg_annual_durations_min_grouped, pregduration_min = n)

## subtract preg duratins from pdays_ey (max and min)

poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>% 
  left_join(preg_annual_durations_max_grouped, by = c("patid" = "patid", "eventyear" = "year")) %>%  
  left_join(preg_annual_durations_min_grouped, by = c("patid" = "patid", "eventyear" = "year")) 


poipresc_annual_counts_final_extra$pregduration_max <- poipresc_annual_counts_final_extra$pregduration_max %>% replace_na(0)
poipresc_annual_counts_final_extra$pregduration_min <- poipresc_annual_counts_final_extra$pregduration_min %>% replace_na(0)

poipresc_annual_counts_final_extra <- poipresc_annual_counts_final_extra %>% 
  mutate(pdays_ey_pregmax = pdays_ey-(pregduration_max)) %>% 
  mutate(pdays_ey_pregmin = pdays_ey-(pregduration_min)) %>% 
  mutate(pyears_ey_pregmax=pdays_ey_pregmax/365.25) %>% 
  mutate(pyears_ey_pregmin=pdays_ey_pregmin/365.25)


## Save & load annual counts final extra (for glms) ---

exact_match_poipresc_annual_counts_final_extra_4to1 <- poipresc_annual_counts_final_extra %>% dplyr::select(patid, eventyear, pracid, prac_region, dob, yob, patimd, pracimd, imd, 
                                                                                           migrant_status, migcertainty, ethnicat, ethnicat6, 
                                                                                           data_start, data_end, poi_cohort_entry, poi_cohort_exit, poi_pdays_max, 
                                                                                           poi_pdays_min, poi_pyears_max, poi_pyears_min, eventyear_age, eventyear_agecat, 
                                                                                           poipresc_n_pregmax, poipresc_n_pregmin, pdays_ey_pregmax, pdays_ey_pregmin, 
                                                                                           pyears_ey_pregmax, pyears_ey_pregmin)





save(exact_match_poipresc_annual_counts_final_extra_4to1, file = "cleaned_files/exact_match_poipresc_annual_counts_final_extra_4to1.Rdata")
load(file = "cleaned_files/exact_match_poipresc_annual_counts_final_extra_4to1.Rdata")










