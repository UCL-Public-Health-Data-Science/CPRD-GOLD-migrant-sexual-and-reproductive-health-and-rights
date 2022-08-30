
####---- Description -------------------------------------------------------------------------

## forest plots for Neha's overall SRHRH and contraception prescribing chapters in females of reproductive age (15-49yo) 
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
library(ggplot2)

## Set working directory ------------------------------------------------------------------

setwd("filepath")


## FP - main analysis -----------------

load(file ="filepath")
cons_glm_adj_unadj <- cons_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("All-cause", "")) 

load(file ="filepath")
abortion_glm_adj_unadj <- abortion_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Abortion", "")) 

load(file = "filepath")
ec_glm_adj_unadj <- ec_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Emergency contraception", "")) 

load(file = "filepath")
dva_glm_adj_unadj <- dva_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Domestic violence and abuse", "")) 

load(file = "filepath")
chlamydia_test_glm_adj_unadj <- chlamydia_test_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Chlamydia testing", "")) 

load(file = "filepath")
infertility_management_glm_adj_unadj <- infertility_management_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Management of fertility problems", "")) 

load(file = "filepath")
cervical_screening_glm_adj_unadj <- cervical_screening_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Cervical screening", "")) 



## Join all subroups 

main_plot_data <- full_join(cons_glm_adj_unadj, abortion_glm_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper",
                                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(ec_glm_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                    "lower" = "lower", "upper"= "upper",
                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(dva_glm_adj_unadj , by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(chlamydia_test_glm_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                "lower" = "lower", "upper"= "upper",
                                                "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(infertility_management_glm_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                        "lower" = "lower", "upper"= "upper",
                                                        "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%     
  full_join(cervical_screening_glm_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                    "lower" = "lower", "upper"= "upper",
                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%   
  add_row(.before = 3) %>%   
  add_row(.before = 6) %>%   
  add_row(.before = 9)  %>%   
  add_row(.before = 12)  %>%   
  add_row(.before = 15)  %>%   
  add_row(.before = 18)

main_plot_data$names <- factor(main_plot_data$names,  c("Migrant", "Migrant(adj)", "Probable"))
main_plot_data$names <- recode(main_plot_data$names,
                                       "Migrant" = "Migrants (unadjusted)", 
                                       "Migrant(adj)" = "Migrants (adjusted)" )                                       
main_plot_data$names <- as.character(main_plot_data$names)
main_plot_data_extrarow <- main_plot_data %>% add_row( .before = 1)



## make forest plot using forest plot
tabletext <- cbind(c("Consultations outcome", main_plot_data$outcome),
                   c("Analysis", main_plot_data$names),
                   c("RR", main_plot_data$estimate),
                   c("95% CI", main_plot_data$ci))
dev.new()
png(file = "filepath", width = 3000, height = 3000)
irrs_main_alloutcomes <- forestplot(tabletext, mean = main_plot_data_extrarow$estimate, lower=main_plot_data_extrarow$lower, upper=main_plot_data_extrarow$upper,
                                            graph.pos = (ncol(tabletext)-1),
                                            hrzl_lines = list("2" = gpar(lty=2)),
                                            clip = c(-2,2),
                                            zero = 1, 
                                            col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                            xlab = "Rate Ratios",
                                            txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                            xticks = c(0,0.5,1,1.5,2),
                                            ci.vertices = TRUE,
                                            ci.vertices.height = 0.20,
                                            boxsize = 0.15,
                                            title = "All outcomes",
                                            graphwidth = unit(300, 'mm'))
dev.off()


## FP - certainty of migration status ---------------------


## Join cons main + certainty, add subgroup name (consultations)
load(file = "filepath")
load(file = "filepath")

cons_glm_adj <- cons_glm_adj_unadj %>% filter(names=="Migrant(adj)")
cons_glm_dp_adj <-  cons_glm_dp_adj_unadj %>% slice_tail(n=2)
cons_migcertainty <- full_join(cons_glm_adj, cons_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                      "lower" = "lower", "upper"= "upper",
                                                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("All-cause", "", "")) 
  

## Join abortion  main + certainty, add subgroup name (abortion)
load(file = "filepath")
load(file = "filepath")

abortion_glm_adj <- abortion_glm_adj_unadj %>% filter(names=="Migrant(adj)")
abortion_glm_dp_adj <-  abortion_glm_dp_adj_unadj %>% slice_tail(n=2)
abortion_migcertainty <- full_join(abortion_glm_adj, abortion_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                     "lower" = "lower", "upper"= "upper",
                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Abortion", "", "")) 

## Join ec main + certainty, add subgroup name (ec)
load(file = "filepath")
load(file = "filepath")

ec_glm_adj <- ec_glm_adj_unadj %>% filter(names=="Migrant(adj)")
ec_glm_dp_adj <-  ec_glm_dp_adj_unadj %>% slice_tail(n=2)
ec_migcertainty <- full_join(ec_glm_adj, ec_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                                 "lower" = "lower", "upper"= "upper",
                                                                                 "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Emergency contraception", "", "")) 

## Join dva main + certainty, add subgroup name (dva)
load(file = "filepath")
load(file = "filepath")


dva_glm_adj <- dva_glm_adj_unadj %>% filter(names=="Migrant(adj)")
dva_glm_dp_adj <-  dva_glm_dp_adj_unadj %>% slice_tail(n=2)
dva_migcertainty <- full_join(dva_glm_adj, dva_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                               "lower" = "lower", "upper"= "upper",
                                                               "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Domestic violence and abuse", "", ""))

## Join ct main + certainty, add subgroup name (ct)
load(file = "filepath")
load(file = "filepath")


chlamydia_test_glm_adj <- chlamydia_test_glm_adj_unadj %>% filter(names=="Migrant(adj)")
chlamydia_test_glm_dp_adj <-  chlamydia_test_glm_dp_adj_unadj %>% slice_tail(n=2)
chlamydia_test_migcertainty <- full_join(chlamydia_test_glm_adj, chlamydia_test_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                  "lower" = "lower", "upper"= "upper",
                                                                  "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Chlamydia testing", "", ""))

## Join fert mx main + certainty, add subgroup name (fert mx)
load(file = "filepath")
load(file = "filepath")


infertility_management_glm_adj <- infertility_management_glm_adj_unadj %>% filter(names=="Migrant(adj)")
infertility_management_glm_dp_adj <-  infertility_management_glm_dp_adj_unadj %>% slice_tail(n=2)
infertility_management_migcertainty <- full_join(infertility_management_glm_adj, infertility_management_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                                                   "lower" = "lower", "upper"= "upper",
                                                                                                   "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Management of fertility problems", "", ""))


## Join cerv screen main + certainty, add subgroup name (cerv screen)
load(file = "filepath")
load(file = "filepath")

cervical_screening_glm_adj <- cervical_screening_glm_adj_unadj %>% filter(names=="Migrant(adj)")
cervical_screening_glm_dp_adj <-  cervical_screening_glm_dp_adj_unadj %>% slice_tail(n=2)
cervical_screening_migcertainty <- full_join(cervical_screening_glm_adj, cervical_screening_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                                                                           "lower" = "lower", "upper"= "upper",
                                                                                                                           "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Cervical screening", "", ""))


## Join all subroups 

migcertainty_plot_data <- full_join(cons_migcertainty, abortion_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper",
                                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
full_join(ec_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                  "lower" = "lower", "upper"= "upper",
                                  "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
full_join(dva_migcertainty , by = c("names" = "names", "estimate" = "estimate",
                                    "lower" = "lower", "upper"= "upper",
                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
full_join(chlamydia_test_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
full_join(infertility_management_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                                "lower" = "lower", "upper"= "upper",
                                                "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%     
full_join(cervical_screening_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                                        "lower" = "lower", "upper"= "upper",
                                                        "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%   
  add_row(.before = 4) %>%   
  add_row(.before = 8) %>%   
  add_row(.before = 12)  %>%   
  add_row(.before = 16)  %>%   
  add_row(.before = 20)  %>%   
  add_row(.before = 24)

migcertainty_plot_data$names <- factor(migcertainty_plot_data$names,  c("Migrant(adj)", "Definite", "Probable"))
migcertainty_plot_data$names <- recode(migcertainty_plot_data$names,
                                       "Migrant(adj)" = "Migrants", 
                                       "Definite" = "Definite Migrants", 
                                       "Probable"  = "Probable Migrants")                                         
migcertainty_plot_data$names <- as.character(migcertainty_plot_data$names)
migcertainty_plot_data_extrarow <- migcertainty_plot_data %>% add_row( .before = 1)
                                            


## make forest plot using forest plot
tabletext <- cbind(c("Consultations outcome", migcertainty_plot_data$outcome),
                   c("Subgroup", migcertainty_plot_data$names),
                   c("RR", migcertainty_plot_data$estimate),
                   c("95% CI", migcertainty_plot_data$ci))
dev.new()
png(file = "filepath", width = 3000, height = 3000)
irrs_migcertainty_alloutcomes <- forestplot(tabletext, mean = migcertainty_plot_data_extrarow$estimate, lower=migcertainty_plot_data_extrarow$lower, upper=migcertainty_plot_data_extrarow$upper,
                                graph.pos = (ncol(tabletext)-1),
                                hrzl_lines = list("2" = gpar(lty=2)),
                                clip = c(-2,2),
                                zero = 1, 
                                col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                xlab = "Multivariable Adjusted Rate Ratios",
                                txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                xticks = c(0,0.5,1,1.5,2),
                                ci.vertices = TRUE,
                                ci.vertices.height = 0.20,
                                boxsize = 0.15,
                                title = "All outcomes",
                                graphwidth = unit(300, 'mm'))
dev.off()

## FP - ethnicity ----------------------------------

## Join cons main + certainty, add subgroup name (consultations)
load(file = "filepath")
load(file = "filepath")

cons_glm_adj <- cons_glm_adj_unadj %>% filter(names=="Migrant(adj)")
cons_glm_adj_ethnicity <-  cons_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                              names == "White Non-British ethnicity - Migrant(adj)" |  
                                                              names == "Mixed ethnicity - Migrant(adj)" | 
                                                              names == "Asian ethnicity - Migrant(adj)" | 
                                                              names == "Black ethnicity - Migrant(adj)" | 
                                                              names == "Other ethnicity - Migrant(adj)" )
cons_ethnicity <- full_join(cons_glm_adj, cons_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                     "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("All-cause", "", "", "", "", "", ""))

## Join abortion  main + certainty, add subgroup name (abortion)
load(file = "filepath")
load(file = "filepath")

abortion_glm_adj <- abortion_glm_adj_unadj %>% filter(names=="Migrant(adj)")
abortion_glm_adj_ethnicity <-  abortion_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                     names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                     names == "Mixed ethnicity - Migrant(adj)" | 
                                                                     names == "Asian ethnicity - Migrant(adj)" | 
                                                                     names == "Black ethnicity - Migrant(adj)" | 
                                                                     names == "Other ethnicity - Migrant(adj)" )
abortion_ethnicity <- full_join(abortion_glm_adj, abortion_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                                 "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Abortion", "", "", "", "", "", "")) 

## Join ec main + certainty, add subgroup name (ec)
load(file = "filepath")
load(file = "filepath")

ec_glm_adj <- ec_glm_adj_unadj %>% filter(names=="Migrant(adj)")
ec_glm_adj_ethnicity <-  ec_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                             names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                             names == "Mixed ethnicity - Migrant(adj)" | 
                                                                             names == "Asian ethnicity - Migrant(adj)" | 
                                                                             names == "Black ethnicity - Migrant(adj)" | 
                                                                             names == "Other ethnicity - Migrant(adj)" )
ec_ethnicity <- full_join(ec_glm_adj, ec_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Emergency contraception", "", "", "", "", "", "")) 

## Join dva main + certainty, add subgroup name (dva)
load(file = "filepath")
load(file = "filepath")

dva_glm_adj <- dva_glm_adj_unadj %>% filter(names=="Migrant(adj)")
dva_glm_adj_ethnicity <-  dva_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                             names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                             names == "Mixed ethnicity - Migrant(adj)" | 
                                                                             names == "Asian ethnicity - Migrant(adj)" | 
                                                                             names == "Black ethnicity - Migrant(adj)" | 
                                                                             names == "Other ethnicity - Migrant(adj)" )
dva_ethnicity <- full_join(dva_glm_adj, dva_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Domestic violence and abuse", "", "", "", "", "", "")) 

## Join ct main + certainty, add subgroup name (ct)
load(file = "filepath")
load(file = "filepath")

chlamydia_test_glm_adj <- chlamydia_test_glm_adj_unadj %>% filter(names=="Migrant(adj)")
chlamydia_test_glm_adj_ethnicity <-  chlamydia_test_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                             names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                             names == "Mixed ethnicity - Migrant(adj)" | 
                                                                             names == "Asian ethnicity - Migrant(adj)" | 
                                                                             names == "Black ethnicity - Migrant(adj)" | 
                                                                             names == "Other ethnicity - Migrant(adj)" )
chlamydia_test_ethnicity <- full_join(chlamydia_test_glm_adj, chlamydia_test_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Chlamydia testing", "", "", "", "", "", "")) 

## Join fert mx main + certainty, add subgroup name (fert mx)
load(file = "filepath")
load(file = "filepath")

infertility_management_glm_adj <- infertility_management_glm_adj_unadj %>% filter(names=="Migrant(adj)")
infertility_management_glm_adj_ethnicity <-  infertility_management_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                             names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                             names == "Mixed ethnicity - Migrant(adj)" | 
                                                                             names == "Asian ethnicity - Migrant(adj)" | 
                                                                             names == "Black ethnicity - Migrant(adj)" | 
                                                                             names == "Other ethnicity - Migrant(adj)" )
infertility_management_ethnicity <- full_join(infertility_management_glm_adj, infertility_management_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Management of fertility problems", "", "", "", "", "", "")) 


## Join cerv screen main + certainty, add subgroup name (cerv screen)
load(file = "filepath")
load(file = "filepath")

cervical_screening_glm_adj <- cervical_screening_glm_adj_unadj %>% filter(names=="Migrant(adj)")
cervical_screening_glm_adj_ethnicity <-  cervical_screening_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                             names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                             names == "Mixed ethnicity - Migrant(adj)" | 
                                                                             names == "Asian ethnicity - Migrant(adj)" | 
                                                                             names == "Black ethnicity - Migrant(adj)" | 
                                                                             names == "Other ethnicity - Migrant(adj)" )
cervical_screening_ethnicity <- full_join(cervical_screening_glm_adj, cervical_screening_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Cervical screening", "", "", "", "", "", "")) 


## Join all subgroups 

ethnicity_plot_data <- full_join(cons_ethnicity, abortion_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper",
                                                                                     "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(ec_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                    "lower" = "lower", "upper"= "upper",
                                   "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(dva_ethnicity , by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                       "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(chlamydia_test_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                "lower" = "lower", "upper"= "upper",
                                               "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(infertility_management_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                        "lower" = "lower", "upper"= "upper",
                                                         "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%     
  full_join(cervical_screening_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                    "lower" = "lower", "upper"= "upper",
                                                   "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%   
  add_row(.before = 8) %>%   
  add_row(.before = 16) %>%   
  add_row(.before = 24)  %>%   
  add_row(.before = 32)  %>%   
  add_row(.before = 40)  %>%   
  add_row(.before = 48)



ethnicity_plot_data$names <- factor(ethnicity_plot_data$names,  c("Migrant(adj)", "White British ethnicity - Migrant(adj)","White Non-British ethnicity - Migrant(adj)",
                                                                  "Mixed ethnicity - Migrant(adj)" , "Asian ethnicity - Migrant(adj)", "Black ethnicity - Migrant(adj)" ,
                                                                  "Other ethnicity - Migrant(adj)"))
ethnicity_plot_data$names <- recode(ethnicity_plot_data$names,
                                    "Migrant(adj)" = "Migrants", "White British ethnicity - Migrant(adj)" = "Migrants - White British ethnicity",  
                                    "White Non-British ethnicity - Migrant(adj)" = "Migrants - White Non-British ethnicity", 
                                     "Mixed ethnicity - Migrant(adj)" = "Migrants - Mixed ethnicity", 
                                     "Asian ethnicity - Migrant(adj)" = "Migrants - Asian/Asian British ethnicity",
                                      "Black ethnicity - Migrant(adj)" = "Migrants - Black/Black British ethnicity",
                                    "Other ethnicity - Migrant(adj)" = "Migrants - Other ethnicity")
ethnicity_plot_data$names <- as.character(ethnicity_plot_data$names)
ethnicity_plot_data_extrarow <- ethnicity_plot_data %>% add_row( .before = 1)



## make forest plot using forest plot
tabletext <- cbind(c("Consultations outcome", ethnicity_plot_data$outcome),
                   c("Subgroup", ethnicity_plot_data$names),
                   c("RR", ethnicity_plot_data$estimate),
                   c("95% CI", ethnicity_plot_data$ci))
dev.new()
png(file = "filepath", width = 3500, height = 5000)
irrs_ethnicity_alloutcomes <- forestplot(tabletext, mean = ethnicity_plot_data_extrarow$estimate, lower=ethnicity_plot_data_extrarow$lower, upper=ethnicity_plot_data_extrarow$upper,
                                            graph.pos = (ncol(tabletext)-1),
                                            hrzl_lines = list("2" = gpar(lty=2)),
                                            clip = c(-2,2),
                                            zero = 1, 
                                            col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                            xlab = "Multivariable Adjusted Rate Ratios",
                                            txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                            xticks = c(0,0.5,1,1.5,2),
                                            ci.vertices = TRUE,
                                            ci.vertices.height = 0.20,
                                            boxsize = 0.15,
                                            title = "All outcomes",
                                            graphwidth = unit(300, 'mm'))
dev.off()


## FP - exact matched cohort main ----------------

## Join cons main whole cohort and exact matched cohort, add sensitivity, add outcome name (consultations)
load(file = "filepath")
load(file = "filepath")

cons_glm_adj <- cons_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
cons_glm_adj_em <- cons_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
cons_main <- full_join(cons_glm_adj, cons_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                     "lower" = "lower", "upper"= "upper",
                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("All-cause", ""))

## Join abortion main whole cohort and exact matched cohort, add sensitivity, add outcome name (abortion)
load(file = "filepath")
load(file = "filepath")

abortion_glm_adj <- abortion_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
abortion_glm_adj_em <- abortion_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
abortion_main <- full_join(abortion_glm_adj, abortion_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                             "lower" = "lower", "upper"= "upper",
                                                             "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Abortion", ""))

## Join ec main whole cohort and exact matched cohort, add sensitivity, add outcome name (ec)
load(file = "filepath")
load(file = "filepath")

ec_glm_adj <- ec_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
ec_glm_adj_em <- ec_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
ec_main <- full_join(ec_glm_adj, ec_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                         "lower" = "lower", "upper"= "upper",
                                                                         "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Emergency contraception",""))

## Join dva main whole cohort and exact matched cohort, add sensitivity, add outcome name (dva)
load(file = "filepath")
load(file = "filepath")

dva_glm_adj <- dva_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
dva_glm_adj_em <- dva_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
dva_main <- full_join(dva_glm_adj, dva_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                         "lower" = "lower", "upper"= "upper",
                                                                         "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Domestic violence and abuse",""))

## Join chlamydia_test main whole cohort and exact matched cohort, add sensitivity, add outcome name (chlamydia_test)
load(file = "filepath")
load(file = "filepath")

chlamydia_test_glm_adj <- chlamydia_test_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
chlamydia_test_glm_adj_em <- chlamydia_test_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
chlamydia_test_main <- full_join(chlamydia_test_glm_adj, chlamydia_test_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                         "lower" = "lower", "upper"= "upper",
                                                                         "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Chlamydia testing",""))

## Join infertility_management main whole cohort and exact matched cohort, add sensitivity, add outcome name (infertility_management)
load(file = "filepath")
load(file = "filepath")

infertility_management_glm_adj <- infertility_management_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
infertility_management_glm_adj_em <- infertility_management_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
infertility_management_main <- full_join(infertility_management_glm_adj, infertility_management_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                         "lower" = "lower", "upper"= "upper",
                                                                         "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Management of fertility problems",""))

## Join cervical_screening main whole cohort and exact matched cohort, add sensitivity, add outcome name (cervical_screening)
load(file = "filepath")
load(file = "filepath")

cervical_screening_glm_adj <- cervical_screening_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
cervical_screening_glm_adj_em <- cervical_screening_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
cervical_screening_main <- full_join(cervical_screening_glm_adj, cervical_screening_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                         "lower" = "lower", "upper"= "upper",
                                                                         "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Cervical screening",""))


## Join all subgroups 

main_plot_data <- full_join(cons_main, abortion_main, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper",
                                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(ec_main, by = c("names" = "names", "estimate" = "estimate",
                                    "lower" = "lower", "upper"= "upper",
                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(dva_main , by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(chlamydia_test_main, by = c("names" = "names", "estimate" = "estimate",
                                                "lower" = "lower", "upper"= "upper",
                                                "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(infertility_management_main, by = c("names" = "names", "estimate" = "estimate",
                                                        "lower" = "lower", "upper"= "upper",
                                                        "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>%     
  full_join(cervical_screening_main, by = c("names" = "names", "estimate" = "estimate",
                                                    "lower" = "lower", "upper"= "upper",
                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>%   
  add_row(.before = 3) %>%   
  add_row(.before = 6) %>%   
  add_row(.before = 9)  %>%   
  add_row(.before = 12)  %>%   
  add_row(.before = 15)  %>%   
  add_row(.before = 18)

main_plot_data_extrarow <- main_plot_data %>% add_row( .before = 1)



## make forest plot
tabletext <- cbind(c("Consultations outcome", main_plot_data$outcome),
                   c("Sensitivity", main_plot_data$sensitivity),
                   c("RR", main_plot_data$estimate),
                   c("95% CI", main_plot_data$ci))
dev.new()
png(file = "filepath", width = 3500, height = 3000)
irrs_exactmatch_alloutcomes <- forestplot(tabletext, mean = main_plot_data_extrarow$estimate, lower=main_plot_data_extrarow$lower, upper=main_plot_data_extrarow$upper,
                                         graph.pos = (ncol(tabletext)-1),
                                         hrzl_lines = list("2" = gpar(lty=2)),
                                         clip = c(-2,2),
                                         zero = 1, 
                                         col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                         xlab = "Multivariable Adjusted Rate Ratios",
                                         txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                         xticks = c(0,0.5,1,1.5,2),
                                         ci.vertices = TRUE,
                                         ci.vertices.height = 0.20,
                                         boxsize = 0.15,
                                         title = "All outcomes",
                                         graphwidth = unit(300, 'mm'))
dev.off()


## FP - exact matched cohort migcertainty  ----------------

## Join cons main + certainty, add sensitivity
load(file = "filepath")
load(file = "filepath")

cons_glm_dp_adj <-  cons_glm_dp_adj_unadj %>% slice_tail(n=2) %>% mutate(sensitivity = "Whole CPRD cohort")
cons_glm_dp_adj_em <-  cons_glm_dp_adj_unadj_em %>% slice_tail(n=2)  %>% mutate(sensitivity = "Exact matched cohort")
cons_migcertainty <- full_join(cons_glm_dp_adj, cons_glm_dp_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                     "lower" = "lower", "upper"= "upper",
                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("All-cause","", "", "")) %>% arrange(names)

## Join abortion main + certainty, add sensitivity
load(file = "filepath")
load(file = "filepath")

abortion_glm_dp_adj <-  abortion_glm_dp_adj_unadj %>% slice_tail(n=2) %>% mutate(sensitivity = "Whole CPRD cohort")
abortion_glm_dp_adj_em <-  abortion_glm_dp_adj_unadj_em %>% slice_tail(n=2)  %>% mutate(sensitivity = "Exact matched cohort")
abortion_migcertainty <- full_join(abortion_glm_dp_adj, abortion_glm_dp_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                     "lower" = "lower", "upper"= "upper",
                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Abortion","", "", "")) %>% arrange(names)

## Join ec main + certainty, add sensitivity
load(file = "filepath")
load(file = "filepath")

ec_glm_dp_adj <-  ec_glm_dp_adj_unadj %>% slice_tail(n=2) %>% mutate(sensitivity = "Whole CPRD cohort")
ec_glm_dp_adj_em <-  ec_glm_dp_adj_unadj_em %>% slice_tail(n=2)  %>% mutate(sensitivity = "Exact matched cohort")
ec_migcertainty <- full_join(ec_glm_dp_adj, ec_glm_dp_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                                    "lower" = "lower", "upper"= "upper",
                                                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Emergency contraception","", "", "")) %>% arrange(names)

## Join dva main + certainty, add sensitivity
load(file = "filepath")
load(file = "filepath")

dva_glm_dp_adj <-  dva_glm_dp_adj_unadj %>% slice_tail(n=2) %>% mutate(sensitivity = "Whole CPRD cohort")
dva_glm_dp_adj_em <-  dva_glm_dp_adj_unadj_em %>% slice_tail(n=2)  %>% mutate(sensitivity = "Exact matched cohort")
dva_migcertainty <- full_join(dva_glm_dp_adj, dva_glm_dp_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                                    "lower" = "lower", "upper"= "upper",
                                                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Domestic violence and abuse","", "", "")) %>% arrange(names)

## Join chlamydia_test main + certainty, add sensitivity
load(file = "filepath")
load(file = "filepath")

chlamydia_test_glm_dp_adj <-  chlamydia_test_glm_dp_adj_unadj %>% slice_tail(n=2) %>% mutate(sensitivity = "Whole CPRD cohort")
chlamydia_test_glm_dp_adj_em <-  chlamydia_test_glm_dp_adj_unadj_em %>% slice_tail(n=2)  %>% mutate(sensitivity = "Exact matched cohort")
chlamydia_test_migcertainty <- full_join(chlamydia_test_glm_dp_adj, chlamydia_test_glm_dp_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                     "lower" = "lower", "upper"= "upper",
                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Chlamydia testing","", "", "")) %>% arrange(names)

## Join infertility_management main + certainty, add sensitivity
load(file = "filepath")
load(file = "filepath")

infertility_management_glm_dp_adj <-  infertility_management_glm_dp_adj_unadj %>% slice_tail(n=2) %>% mutate(sensitivity = "Whole CPRD cohort")
infertility_management_glm_dp_adj_em <-  infertility_management_glm_dp_adj_unadj_em %>% slice_tail(n=2)  %>% mutate(sensitivity = "Exact matched cohort")
infertility_management_migcertainty <- full_join(infertility_management_glm_dp_adj, infertility_management_glm_dp_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                     "lower" = "lower", "upper"= "upper",
                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Management of fertility problems","", "", "")) %>% arrange(names)

## Join cervical_screening main + certainty, add sensitivity
load(file = "filepath")
load(file = "filepath")

cervical_screening_glm_dp_adj <-  cervical_screening_glm_dp_adj_unadj %>% slice_tail(n=2) %>% mutate(sensitivity = "Whole CPRD cohort")
cervical_screening_glm_dp_adj_em <-  cervical_screening_glm_dp_adj_unadj_em %>% slice_tail(n=2)  %>% mutate(sensitivity = "Exact matched cohort")
cervical_screening_migcertainty <- full_join(cervical_screening_glm_dp_adj, cervical_screening_glm_dp_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                     "lower" = "lower", "upper"= "upper",
                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Cervical screening","", "", "")) %>% arrange(names)



## Join all subgroups 

migcertainty_plot_data <- full_join(cons_migcertainty, abortion_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper",
                                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity",
                                                                                     "outcome" = "outcome")) %>% 
  full_join(ec_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                    "lower" = "lower", "upper"= "upper",
                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci",  "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(dva_migcertainty , by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(chlamydia_test_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                                "lower" = "lower", "upper"= "upper",
                                                "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity",  "outcome" = "outcome")) %>% 
  full_join(infertility_management_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                                        "lower" = "lower", "upper"= "upper",
                                                        "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>%     
  full_join(cervical_screening_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                                    "lower" = "lower", "upper"= "upper",
                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>%   
  add_row(.before = 5) %>%   
  add_row(.before = 10) %>%   
  add_row(.before = 15)  %>%   
  add_row(.before = 20)  %>%   
  add_row(.before = 25)  %>%   
  add_row(.before = 30)

migcertainty_plot_data$names <- factor(migcertainty_plot_data$names,  c("Definite", "Probable"))
migcertainty_plot_data$names <- recode(migcertainty_plot_data$names,
                                       "Definite" = "Definite Migrants", 
                                       "Probable"  = "Probable Migrants")                                         
migcertainty_plot_data$names <- as.character(migcertainty_plot_data$names)
migcertainty_plot_data_extrarow <- migcertainty_plot_data %>% add_row( .before = 1)


## make forest plot 
tabletext <- cbind(c("Consultations outcome", migcertainty_plot_data$outcome),
                   c("Sensitivity", migcertainty_plot_data$sensitivity),
                   c("Subgroup", migcertainty_plot_data$names),
                   c("RR", migcertainty_plot_data$estimate),
                   c("95% CI", migcertainty_plot_data$ci))
dev.new()
png(file = "filepath", width = 4000, height = 4000)
irrs_migcertainty_alloutcomes_em <- forestplot(tabletext, mean = migcertainty_plot_data_extrarow$estimate, lower=migcertainty_plot_data_extrarow$lower, upper=migcertainty_plot_data_extrarow$upper,
                                            graph.pos = (ncol(tabletext)-1),
                                            hrzl_lines = list("2" = gpar(lty=2)),
                                            clip = c(-2,2),
                                            zero = 1, 
                                            col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                            xlab = "Multivariable Adjusted Rate Ratios",
                                            txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                            xticks = c(0,0.5,1,1.5,2),
                                            ci.vertices = TRUE,
                                            ci.vertices.height = 0.20,
                                            boxsize = 0.15,
                                            title = "All outcomes",
                                            graphwidth = unit(300, 'mm'))
dev.off()


## FP - exact matched cohort ethnicity  ----------------

## Join cons main + certainty, add subgroup name (consultations)
load(file = "filepath")
load(file = "filepath")

cons_glm_adj_ethnicity <-  cons_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                     names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                     names == "Mixed ethnicity - Migrant(adj)" | 
                                                                     names == "Asian ethnicity - Migrant(adj)" | 
                                                                     names == "Black ethnicity - Migrant(adj)" | 
                                                                     names == "Other ethnicity - Migrant(adj)" ) %>%
  mutate(sensitivity = "Whole CPRD cohort")
cons_glm_adj_ethnicity_em <-  cons_glm_adj_unadj_ethnicity_em %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                     names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                     names == "Mixed ethnicity - Migrant(adj)" | 
                                                                     names == "Asian ethnicity - Migrant(adj)" | 
                                                                     names == "Black ethnicity - Migrant(adj)" | 
                                                                     names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = "Exact matched cohort")
cons_ethnicity <- full_join(cons_glm_adj_ethnicity,cons_glm_adj_ethnicity_em, by = c("names" = "names", "estimate" = "estimate",
                                                                         "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("All-cause", "", "", "", "", "", "", "", "", "", "", "")) %>%
           arrange(names)

## Join abortion main + certainty, add subgroup name (abortion)
load(file = "filepath")
load(file = "filepath")

abortion_glm_adj_ethnicity <-  abortion_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                     names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                     names == "Mixed ethnicity - Migrant(adj)" | 
                                                                     names == "Asian ethnicity - Migrant(adj)" | 
                                                                     names == "Black ethnicity - Migrant(adj)" | 
                                                                     names == "Other ethnicity - Migrant(adj)" ) %>%
  mutate(sensitivity = "Whole CPRD cohort")
abortion_glm_adj_ethnicity_em <-  abortion_glm_adj_unadj_ethnicity_em %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                        names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                        names == "Mixed ethnicity - Migrant(adj)" | 
                                                                        names == "Asian ethnicity - Migrant(adj)" | 
                                                                        names == "Black ethnicity - Migrant(adj)" | 
                                                                        names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = "Exact matched cohort")
abortion_ethnicity <- full_join(abortion_glm_adj_ethnicity,abortion_glm_adj_ethnicity_em, by = c("names" = "names", "estimate" = "estimate",
                                                                                     "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Abortion", "", "", "", "", "", "", "", "", "", "", "")) %>%
  arrange(names)

## Join ec main + certainty, add subgroup name (ec)
load(file = "filepath")
load(file = "filepath")

ec_glm_adj_ethnicity <-  ec_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                             names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                             names == "Mixed ethnicity - Migrant(adj)" | 
                                                                             names == "Asian ethnicity - Migrant(adj)" | 
                                                                             names == "Black ethnicity - Migrant(adj)" | 
                                                                             names == "Other ethnicity - Migrant(adj)" ) %>%
  mutate(sensitivity = "Whole CPRD cohort")
ec_glm_adj_ethnicity_em <-  ec_glm_adj_unadj_ethnicity_em %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                                names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                                names == "Mixed ethnicity - Migrant(adj)" | 
                                                                                names == "Asian ethnicity - Migrant(adj)" | 
                                                                                names == "Black ethnicity - Migrant(adj)" | 
                                                                                names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = "Exact matched cohort")
ec_ethnicity <- full_join(ec_glm_adj_ethnicity,ec_glm_adj_ethnicity_em, by = c("names" = "names", "estimate" = "estimate",
                                                                                                 "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Emergency contraception", "", "", "", "", "", "", "", "", "", "", "")) %>%
  arrange(names)

## Join dva main + certainty, add subgroup name (dva)
load(file = "filepath")
load(file = "filepath")

dva_glm_adj_ethnicity <-  dva_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                             names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                             names == "Mixed ethnicity - Migrant(adj)" | 
                                                                             names == "Asian ethnicity - Migrant(adj)" | 
                                                                             names == "Black ethnicity - Migrant(adj)" | 
                                                                             names == "Other ethnicity - Migrant(adj)" ) %>%
  mutate(sensitivity = "Whole CPRD cohort")
dva_glm_adj_ethnicity_em <-  dva_glm_adj_unadj_ethnicity_em %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                                names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                                names == "Mixed ethnicity - Migrant(adj)" | 
                                                                                names == "Asian ethnicity - Migrant(adj)" | 
                                                                                names == "Black ethnicity - Migrant(adj)" | 
                                                                                names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = "Exact matched cohort")
dva_ethnicity <- full_join(dva_glm_adj_ethnicity,dva_glm_adj_ethnicity_em, by = c("names" = "names", "estimate" = "estimate",
                                                                                                 "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Domestic violence and abuse", "", "", "", "", "", "", "", "", "", "", "")) %>%
  arrange(names)

## Join chlamydia_test main + certainty, add subgroup name (chlamydia_test)
load(file = "filepath")
load(file = "filepath")

chlamydia_test_glm_adj_ethnicity <-  chlamydia_test_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                             names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                             names == "Mixed ethnicity - Migrant(adj)" | 
                                                                             names == "Asian ethnicity - Migrant(adj)" | 
                                                                             names == "Black ethnicity - Migrant(adj)" | 
                                                                             names == "Other ethnicity - Migrant(adj)" ) %>%
  mutate(sensitivity = "Whole CPRD cohort")
chlamydia_test_glm_adj_ethnicity_em <-  chlamydia_test_glm_adj_unadj_ethnicity_em %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                                names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                                names == "Mixed ethnicity - Migrant(adj)" | 
                                                                                names == "Asian ethnicity - Migrant(adj)" | 
                                                                                names == "Black ethnicity - Migrant(adj)" | 
                                                                                names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = "Exact matched cohort")
chlamydia_test_ethnicity <- full_join(chlamydia_test_glm_adj_ethnicity,chlamydia_test_glm_adj_ethnicity_em, by = c("names" = "names", "estimate" = "estimate",
                                                                                                 "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Chlamydia testing", "", "", "", "", "", "", "", "", "", "", "")) %>%
  arrange(names)

## Join infertility_management main + certainty, add subgroup name (infertility_management)
load(file = "filepath")
load(file = "filepath")

infertility_management_glm_adj_ethnicity <-  infertility_management_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                             names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                             names == "Mixed ethnicity - Migrant(adj)" | 
                                                                             names == "Asian ethnicity - Migrant(adj)" | 
                                                                             names == "Black ethnicity - Migrant(adj)" | 
                                                                             names == "Other ethnicity - Migrant(adj)" ) %>%
  mutate(sensitivity = "Whole CPRD cohort")
infertility_management_glm_adj_ethnicity_em <-  infertility_management_glm_adj_unadj_ethnicity_em %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                                names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                                names == "Mixed ethnicity - Migrant(adj)" | 
                                                                                names == "Asian ethnicity - Migrant(adj)" | 
                                                                                names == "Black ethnicity - Migrant(adj)" | 
                                                                                names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = "Exact matched cohort")
infertility_management_ethnicity <- full_join(infertility_management_glm_adj_ethnicity,infertility_management_glm_adj_ethnicity_em, by = c("names" = "names", "estimate" = "estimate",
                                                                                                 "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Management of fertility problems", "", "", "", "", "", "", "", "", "", "", "")) %>%
  arrange(names)

## Join cervical_screening main + certainty, add subgroup name (cervical_screening)
load(file = "filepath")
load(file = "filepath")

cervical_screening_glm_adj_ethnicity <-  cervical_screening_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                                                         names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                                                         names == "Mixed ethnicity - Migrant(adj)" | 
                                                                                                         names == "Asian ethnicity - Migrant(adj)" | 
                                                                                                         names == "Black ethnicity - Migrant(adj)" | 
                                                                                                         names == "Other ethnicity - Migrant(adj)" ) %>%
  mutate(sensitivity = "Whole CPRD cohort")
cervical_screening_glm_adj_ethnicity_em <-  cervical_screening_glm_adj_unadj_ethnicity_em %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                                                            names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                                                            names == "Mixed ethnicity - Migrant(adj)" | 
                                                                                                            names == "Asian ethnicity - Migrant(adj)" | 
                                                                                                            names == "Black ethnicity - Migrant(adj)" | 
                                                                                                            names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = "Exact matched cohort")
cervical_screening_ethnicity <- full_join(cervical_screening_glm_adj_ethnicity,cervical_screening_glm_adj_ethnicity_em, by = c("names" = "names", "estimate" = "estimate",
                                                                                                                                           "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Cervical screening", "", "", "", "", "", "", "", "", "", "", "")) %>%
  arrange(names)



## Join all subgroups 

ethnicity_plot_data <- full_join(cons_ethnicity, abortion_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                            "lower" = "lower", "upper"= "upper",
                                                                            "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity",  "outcome" = "outcome")) %>% 
  full_join(ec_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                 "lower" = "lower", "upper"= "upper",
                                 "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(dva_ethnicity , by = c("names" = "names", "estimate" = "estimate",
                                   "lower" = "lower", "upper"= "upper",
                                   "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(chlamydia_test_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                             "lower" = "lower", "upper"= "upper",
                                             "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(infertility_management_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                     "lower" = "lower", "upper"= "upper",
                                                     "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>%     
  full_join(cervical_screening_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                 "lower" = "lower", "upper"= "upper",
                                                 "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>%   
  add_row(.before = 13) %>%   
  add_row(.before = 26) %>%   
  add_row(.before = 39)  %>%   
  add_row(.before = 52)  %>%   
  add_row(.before = 65)  %>%   
  add_row(.before = 78)



ethnicity_plot_data$names <- factor(ethnicity_plot_data$names,  c("White British ethnicity - Migrant(adj)","White Non-British ethnicity - Migrant(adj)",
                                                                  "Mixed ethnicity - Migrant(adj)" , "Asian ethnicity - Migrant(adj)", "Black ethnicity - Migrant(adj)" ,
                                                                  "Other ethnicity - Migrant(adj)"))
ethnicity_plot_data$names <- recode(ethnicity_plot_data$names,
                                   "White British ethnicity - Migrant(adj)" = "Migrants - White British ethnicity",  
                                    "White Non-British ethnicity - Migrant(adj)" = "Migrants - White Non-British ethnicity", 
                                    "Mixed ethnicity - Migrant(adj)" = "Migrants - Mixed ethnicity", 
                                    "Asian ethnicity - Migrant(adj)" = "Migrants - Asian/Asian British ethnicity",
                                    "Black ethnicity - Migrant(adj)" = "Migrants - Black/Black British ethnicity",
                                    "Other ethnicity - Migrant(adj)" = "Migrants - Other ethnicity")
ethnicity_plot_data$names <- as.character(ethnicity_plot_data$names)
ethnicity_plot_data_extrarow <- ethnicity_plot_data %>% add_row( .before = 1)

## make forest plot
tabletext <- cbind(c("Consultations outcome", ethnicity_plot_data$outcome),
                   c("Sensitivity", ethnicity_plot_data$sensitivity),
                   c("Subgroup", ethnicity_plot_data$names),
                   c("RR", ethnicity_plot_data$estimate),
                   c("95% CI", ethnicity_plot_data$ci))
dev.new()
png(file = "filepath", width = 4500, height = 6500)
irrs_ethnicity_alloutcomes <- forestplot(tabletext, mean = ethnicity_plot_data_extrarow$estimate, lower=ethnicity_plot_data_extrarow$lower, upper=ethnicity_plot_data_extrarow$upper,
                                         graph.pos = (ncol(tabletext)-1),
                                         hrzl_lines = list("2" = gpar(lty=2)),
                                         clip = c(-2,2),
                                         zero = 1, 
                                         col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                         xlab = "Multivariable Adjusted Rate Ratios",
                                         txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                         xticks = c(0,0.5,1,1.5,2),
                                         ci.vertices = TRUE,
                                         ci.vertices.height = 0.20,
                                         boxsize = 0.15,
                                         title = "All outcomes",
                                         graphwidth = unit(300, 'mm'))
dev.off()




## Forest plot - contraception prescribing main analysis -------------------------------


load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")


cuiud_glm_adj_unadj <- cuiud_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Copper intrauterine device", "")) 
ius_glm_adj_unadj <- ius_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Intrauterine system", "")) 
sdi_glm_adj_unadj <- sdi_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Subdermal implant", "")) 
poi_glm_adj_unadj <- poi_glm_adj_unadj %>% filter(names=="Migrant" | names=="Migrant(adj)") %>%   mutate(outcome = c("Progestogen-only injection", "")) 


glm_adj_unadj <- full_join(cuiud_glm_adj_unadj, ius_glm_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                                                   "lower" = "lower", "upper"= "upper",
                                                                                   "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(sdi_glm_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                             "lower" = "lower", "upper"= "upper",
                                             "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%
  full_join(poi_glm_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                 "lower" = "lower", "upper"= "upper",
                                                 "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%   
  add_row(.before = 3) %>%   
  add_row(.before = 6) %>%   
  add_row(.before = 9)  


glm_adj_unadj$names <- factor(glm_adj_unadj$names,  c("Migrant", "Migrant(adj)"))
glm_adj_unadj$names <- recode(glm_adj_unadj$names,
                               "Migrant" = "Migrants (unadjusted)", 
                               "Migrant(adj)" = "Migrants (adjusted)" )                                       
glm_adj_unadj$names <- as.character(glm_adj_unadj$names)
glm_adj_unadj_extrarow <- glm_adj_unadj %>% add_row( .before = 1)


tabletext <- cbind(c("LARC prescriptions outcome", glm_adj_unadj$outcome),
                   c("Analysis", glm_adj_unadj$names),
                   c("RR", glm_adj_unadj$estimate),
                   c("95% CI", glm_adj_unadj$ci))



dev.new()
png(file = "filepath", width = 3000, height = 2000)
irrs_main_alloutcomes <- forestplot(tabletext, mean = glm_adj_unadj_extrarow$estimate, lower=glm_adj_unadj_extrarow$lower, upper=glm_adj_unadj_extrarow$upper,
                                    graph.pos = (ncol(tabletext)-1),
                                    hrzl_lines = list("2" = gpar(lty=2)),
                                    clip = c(-2,2),
                                    zero = 1, 
                                    col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                    xlab = "Rate Ratios",
                                    txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                    xticks = c(0,0.5,1,1.5,2),
                                    ci.vertices = TRUE,
                                    ci.vertices.height = 0.20,
                                    boxsize = 0.15,
                                    title = "LARC prescribing",
                                    graphwidth = unit(300, 'mm'))
dev.off()





## Forest plot - contraception prescribing mig certaintyanalysis -------------------------------



load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")

## Join cuiud main + certainty, add subgroup name (consultations)
cuiud_glm_adj_unadj <- cuiud_glm_adj_unadj%>% filter(names=="Migrant(adj)")
cuiud_glm_dp_adj_unadj <-  cuiud_glm_dp_adj_unadj %>% slice_tail(n=2)
cuiud_migcertainty <- full_join(cuiud_glm_adj_unadj, cuiud_glm_dp_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                                     "lower" = "lower", "upper"= "upper",
                                                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Copper intrauterine device", "", "")) 


## Join ius main + certainty, add subgroup name (consultations)
ius_glm_adj_unadj <- ius_glm_adj_unadj%>% filter(names=="Migrant(adj)")
ius_glm_dp_adj_unadj <-  ius_glm_dp_adj_unadj %>% slice_tail(n=2)
ius_migcertainty <- full_join(ius_glm_adj_unadj, ius_glm_dp_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                                                    "lower" = "lower", "upper"= "upper",
                                                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Intrauterine system", "", "")) 


## Join sdi main + certainty, add subgroup name (consultations)
sdi_glm_adj_unadj <- sdi_glm_adj_unadj%>% filter(names=="Migrant(adj)")
sdi_glm_dp_adj_unadj <-  sdi_glm_dp_adj_unadj %>% slice_tail(n=2)
sdi_migcertainty <- full_join(sdi_glm_adj_unadj, sdi_glm_dp_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                                                    "lower" = "lower", "upper"= "upper",
                                                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Subdermal implant", "", "")) 

## Join poi main + certainty, add subgroup name (consultations)
poi_glm_adj_unadj <- poi_glm_adj_unadj%>% filter(names=="Migrant(adj)")
poi_glm_dp_adj_unadj <-  poi_glm_dp_adj_unadj %>% slice_tail(n=2)
poi_migcertainty <- full_join(poi_glm_adj_unadj, poi_glm_dp_adj_unadj, by = c("names" = "names", "estimate" = "estimate",
                                                                                    "lower" = "lower", "upper"= "upper",
                                                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  mutate(outcome = c("Progestogen-only injection", "", "")) 



larc_migcertainty <- full_join(cuiud_migcertainty, ius_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                                                         "lower" = "lower", "upper"= "upper",
                                                                         "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(sdi_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%
  full_join(poi_migcertainty, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%   
  add_row(.before = 4) %>%   
  add_row(.before = 8) %>%   
  add_row(.before = 12)  



larc_migcertainty$names <- factor(larc_migcertainty$names,  c("Migrant(adj)", "Definite Migrants(adj)", "Probable Migrants(adj)"))
larc_migcertainty$names <- recode(larc_migcertainty$names,
                                       "Migrant(adj)" = "Migrants", 
                                  "Definite Migrants(adj)" = "Definite Migrants", 
                                  "Probable Migrants(adj)" = "Probable Migrants")                                         
larc_migcertainty$names <- as.character(larc_migcertainty$names)
larc_migcertainty_extrarow <- larc_migcertainty %>% add_row( .before = 1)



## make forest plot using forest plot
tabletext <- cbind(c("LARC prescriptions outcome", larc_migcertainty$outcome),
                   c("Subgroup", larc_migcertainty$names),
                   c("RR", larc_migcertainty$estimate),
                   c("95% CI", larc_migcertainty$ci))
dev.new()
png(file = "filepath", width = 3000, height = 3000)
irrs_migcertainty_alloutcomes <- forestplot(tabletext, mean = larc_migcertainty_extrarow$estimate, lower=larc_migcertainty_extrarow$lower, upper=larc_migcertainty_extrarow$upper,
                                            graph.pos = (ncol(tabletext)-1),
                                            hrzl_lines = list("2" = gpar(lty=2)),
                                            clip = c(-2,2),
                                            zero = 1, 
                                            col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                            xlab = "Multivariable Adjusted Rate Ratios",
                                            txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                            xticks = c(0,0.5,1,1.5,2),
                                            ci.vertices = TRUE,
                                            ci.vertices.height = 0.20,
                                            boxsize = 0.15,
                                            title = "LARC prescribing",
                                            graphwidth = unit(300, 'mm'))
dev.off()








## Forest plot - contraception prescribing ethnicity analysis -------------------------------


load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")


load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")


## Join cuiud main + certainty, add subgroup name (consultations)
cuiud_glm_adj <- cuiud_glm_adj_unadj %>% filter(names=="Migrant(adj)")
cuiud_glm_adj_ethnicity <-  cuiud_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                     names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                     names == "Mixed ethnicity - Migrant(adj)" | 
                                                                     names == "Asian ethnicity - Migrant(adj)" | 
                                                                     names == "Black ethnicity - Migrant(adj)" | 
                                                                     names == "Other ethnicity - Migrant(adj)" )
cuiud_ethnicity <- full_join(cuiud_glm_adj, cuiud_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                         "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci","p" = "p")) %>% 
  mutate(outcome = c("Copper intrauterine device", "", "", "", "", "", ""))


## Join ius main + certainty, add subgroup name (consultations)
ius_glm_adj <- ius_glm_adj_unadj %>% filter(names=="Migrant(adj)")
ius_glm_adj_ethnicity <-  ius_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                       names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                       names == "Mixed ethnicity - Migrant(adj)" | 
                                                                       names == "Asian ethnicity - Migrant(adj)" | 
                                                                       names == "Black ethnicity - Migrant(adj)" | 
                                                                       names == "Other ethnicity - Migrant(adj)" )
ius_ethnicity <- full_join(ius_glm_adj, ius_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                            "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci","p" = "p")) %>% 
  mutate(outcome = c("Intrauterine system", "", "", "", "", "", ""))

## Join sdi main + certainty, add subgroup name (consultations)
sdi_glm_adj <- sdi_glm_adj_unadj %>% filter(names=="Migrant(adj)")
sdi_glm_adj_ethnicity <-  sdi_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                       names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                       names == "Mixed ethnicity - Migrant(adj)" | 
                                                                       names == "Asian ethnicity - Migrant(adj)" | 
                                                                       names == "Black ethnicity - Migrant(adj)" | 
                                                                       names == "Other ethnicity - Migrant(adj)" )
sdi_ethnicity <- full_join(sdi_glm_adj, sdi_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                            "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci","p" = "p")) %>% 
  mutate(outcome = c("Subdermal implant", "", "", "", "", "", ""))


## Join poi main + certainty, add subgroup name (consultations)
poi_glm_adj <- poi_glm_adj_unadj %>% filter(names=="Migrant(adj)")
poi_glm_adj_ethnicity <-  poi_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                       names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                       names == "Mixed ethnicity - Migrant(adj)" | 
                                                                       names == "Asian ethnicity - Migrant(adj)" | 
                                                                       names == "Black ethnicity - Migrant(adj)" | 
                                                                       names == "Other ethnicity - Migrant(adj)" )
poi_ethnicity <- full_join(poi_glm_adj, poi_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                            "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci","p" = "p")) %>% 
  mutate(outcome = c("Progestogen only injection", "", "", "", "", "", ""))





larc_ethnicity <- full_join(cuiud_ethnicity, ius_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                           "lower" = "lower", "upper"= "upper",
                                                                           "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  full_join(sdi_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                         "lower" = "lower", "upper"= "upper",
                                         "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>%
  full_join(poi_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                         "lower" = "lower", "upper"= "upper",
                                         "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "outcome" = "outcome")) %>% 
  add_row(.before = 8) %>%   
  add_row(.before = 16) %>%   
  add_row(.before = 24)


larc_ethnicity$names <- factor(larc_ethnicity$names,  c("Migrant(adj)", "White British ethnicity - Migrant(adj)","White Non-British ethnicity - Migrant(adj)",
                                                                  "Mixed ethnicity - Migrant(adj)" , "Asian ethnicity - Migrant(adj)", "Black ethnicity - Migrant(adj)" ,
                                                                  "Other ethnicity - Migrant(adj)"))
larc_ethnicity$names <- recode(larc_ethnicity$names,
                                    "Migrant(adj)" = "Migrants", "White British ethnicity - Migrant(adj)" = "Migrants - White British ethnicity",  
                                    "White Non-British ethnicity - Migrant(adj)" = "Migrants - White Non-British ethnicity", 
                                    "Mixed ethnicity - Migrant(adj)" = "Migrants - Mixed ethnicity", 
                                    "Asian ethnicity - Migrant(adj)" = "Migrants - Asian/Asian British ethnicity",
                                    "Black ethnicity - Migrant(adj)" = "Migrants - Black/Black British ethnicity",
                                    "Other ethnicity - Migrant(adj)" = "Migrants - Other ethnicity")
larc_ethnicity$names <- as.character(larc_ethnicity$names)
larc_ethnicity_extrarow <- larc_ethnicity %>% add_row( .before = 1)



## make forest plot using forest plot
tabletext <- cbind(c("LARC prescriptions outcome", larc_ethnicity$outcome),
                   c("Subgroup", larc_ethnicity$names),
                   c("RR", larc_ethnicity$estimate),
                   c("95% CI", larc_ethnicity$ci))
dev.new()
png(file = "filepath", width = 3500, height = 4500)
irrs_ethnicity_alloutcomes <- forestplot(tabletext, mean = larc_ethnicity_extrarow$estimate, lower=larc_ethnicity_extrarow$lower, upper=larc_ethnicity_extrarow$upper,
                                         graph.pos = (ncol(tabletext)-1),
                                         hrzl_lines = list("2" = gpar(lty=2)),
                                         clip = c(-2,2),
                                         zero = 1, 
                                         col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                         xlab = "Multivariable Adjusted Rate Ratios",
                                         txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                         xticks = c(0,0.5,1,1.5,2),
                                         ci.vertices = TRUE,
                                         ci.vertices.height = 0.20,
                                         boxsize = 0.15,
                                         title = "LARC prescribing",
                                         graphwidth = unit(300, 'mm'))
dev.off()


## Forest plot - contraception prescribing pregmin and exact matched versus main analyses -------------------------------

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


## Join cuiud main whole cohort and exact matched cohort, add sensitivity, add outcome name (consultations)
cuiud_glm_adj <- cuiud_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
cuiud_glm_adj_em <- cuiud_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
cuiud_glm_adj_pregmin <- cuiud_glm_adj_unadj_pregmin %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - minimum pregnancy durations")
cuiud_sens <- full_join(cuiud_glm_adj , cuiud_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                             "lower" = "lower", "upper"= "upper",
                                                             "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  full_join(cuiud_glm_adj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                          "lower" = "lower", "upper"= "upper",
                                          "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Copper intrauterine device", "", ""))

## Join ius main whole cohort and exact matched cohort, add sensitivity, add outcome name (consultations)
ius_glm_adj <- ius_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
ius_glm_adj_em <- ius_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
ius_glm_adj_pregmin <- ius_glm_adj_unadj_pregmin %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - minimum pregnancy durations")
ius_sens <- full_join(ius_glm_adj , ius_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                 "lower" = "lower", "upper"= "upper",
                                                                 "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  full_join(ius_glm_adj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                          "lower" = "lower", "upper"= "upper",
                                          "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Intrauterine system", "", ""))

## Join sdi main whole cohort and exact matched cohort, add sensitivity, add outcome name (consultations)
sdi_glm_adj <- sdi_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
sdi_glm_adj_em <- sdi_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
sdi_glm_adj_pregmin <- sdi_glm_adj_unadj_pregmin %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - minimum pregnancy durations")
sdi_sens <- full_join(sdi_glm_adj , sdi_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                 "lower" = "lower", "upper"= "upper",
                                                                 "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  full_join(sdi_glm_adj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                          "lower" = "lower", "upper"= "upper",
                                          "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Subdermal implant", "", ""))

## Join poi main whole cohort and exact matched cohort, add sensitivity, add outcome name (consultations)
poi_glm_adj <- poi_glm_adj_unadj %>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
poi_glm_adj_em <- poi_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
poi_glm_adj_pregmin <- poi_glm_adj_unadj_pregmin %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - minimum pregnancy durations")
poi_sens <- full_join(poi_glm_adj , poi_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                 "lower" = "lower", "upper"= "upper",
                                                                 "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  full_join(poi_glm_adj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                          "lower" = "lower", "upper"= "upper",
                                          "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  mutate(outcome = c("Progestogen-only injection", "", ""))





glm_adj_unadj<- full_join(cuiud_glm_adj_unadj_pregmin, ius_glm_adj_unadj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                                                         "lower" = "lower", "upper"= "upper",
                                                                         "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>% 
  full_join(sdi_glm_adj_unadj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci")) %>%
  full_join(poi_glm_adj_unadj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                      "lower" = "lower", "upper"= "upper",
                                      "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci"))



## Join all subgroups 

larc_sensitivity_main <- full_join(cuiud_sens, ius_sens, by = c("names" = "names", "estimate" = "estimate",
                                                             "lower" = "lower", "upper"= "upper",
                                                             "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(sdi_sens, by = c("names" = "names", "estimate" = "estimate",
                            "lower" = "lower", "upper"= "upper",
                            "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome")) %>% 
  full_join(poi_sens , by = c("names" = "names", "estimate" = "estimate",
                              "lower" = "lower", "upper"= "upper",
                              "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity", "outcome" = "outcome"))  %>%   
  add_row(.before = 4) %>%   
  add_row(.before = 8) %>%   
  add_row(.before = 12) 

larc_sensitivity_main_extrarow <- larc_sensitivity_main %>% add_row( .before = 1)


## make forest plot
tabletext <- cbind(c("LARC prescriptions outcome", larc_sensitivity_main$outcome),
                   c("Sensitivity", larc_sensitivity_main$sensitivity),
                   c("RR", larc_sensitivity_main$estimate),
                   c("95% CI", larc_sensitivity_main$ci))
dev.new()
png(file = "filepath", width = 3500, height = 2500)
irrs_sens_alloutcomes <- forestplot(tabletext, mean = larc_sensitivity_main_extrarow$estimate, lower=larc_sensitivity_main_extrarow$lower, upper=larc_sensitivity_main_extrarow$upper,
                                          graph.pos = (ncol(tabletext)-1),
                                          hrzl_lines = list("2" = gpar(lty=2)),
                                          clip = c(-2,2),
                                          zero = 1, 
                                          col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                          xlab = "Multivariable Adjusted Rate Ratios",
                                          txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                          xticks = c(0,0.5,1,1.5,2),
                                          ci.vertices = TRUE,
                                          ci.vertices.height = 0.20,
                                          boxsize = 0.15,
                                          title = "All outcomes",
                                          graphwidth = unit(300, 'mm'))
dev.off()



## Forest plot - Cu IUD subgroup and sensitivity ----------------------

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")

## Join cuiud main + certainty, add subgroup name (consultations)
cuiud_glm_adj <- cuiud_glm_adj_unadj%>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
cuiud_glm_dp_adj <-  cuiud_glm_dp_adj_unadj %>% slice_tail(n=2)  %>% mutate(sensitivity = c("Stratification by certainty of migration status", ""))
cuiud_glm_adj_ethnicity <-  cuiud_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                       names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                       names == "Mixed ethnicity - Migrant(adj)" | 
                                                                       names == "Asian ethnicity - Migrant(adj)" | 
                                                                       names == "Black ethnicity - Migrant(adj)" | 
                                                                       names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = c("Stratification by ethnicity", "", "", "", "", ""))
cuiud_glm_adj_em <- cuiud_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
cuiud_glm_adj_pregmin <- cuiud_glm_adj_unadj_pregmin %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - minimum pregnancy durations")



cuiud_sub_sens <- full_join(cuiud_glm_adj, cuiud_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                                    "lower" = "lower", "upper"= "upper",
                                                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>%
full_join(cuiud_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                                                            "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci","p" = "p", "sensitivity" = "sensitivity")) %>% 
full_join(cuiud_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                                                 "lower" = "lower", "upper"= "upper",
                                                                 "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  full_join(cuiud_glm_adj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                          "lower" = "lower", "upper"= "upper",
                                          "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>%   
  add_row(.before = 2) %>%   
  add_row(.before = 5) %>%   
  add_row(.before = 12) %>%   
  add_row(.before = 14) 


cuiud_sub_sens$names <- factor(cuiud_sub_sens$names,  c("Migrant(adj)", "Definite Migrants(adj)", "Probable Migrants(adj)",
                                                        "White British ethnicity - Migrant(adj)", "White Non-British ethnicity - Migrant(adj)",
                                                        "Mixed ethnicity - Migrant(adj)",  "Asian ethnicity - Migrant(adj)",
                                                        "Black ethnicity - Migrant(adj)",  "Other ethnicity - Migrant(adj)"))
cuiud_sub_sens$names <- recode(cuiud_sub_sens$names,
                                  "Migrant(adj)" = "Migrants", 
                                  "Definite Migrants(adj)" = "Definite Migrants", 
                                  "Probable Migrants(adj)" = "Probable Migrants",
                               "White British ethnicity - Migrant(adj)" = "Migrants - White British ethnicity",  
                               "White Non-British ethnicity - Migrant(adj)" = "Migrants - White Non-British ethnicity", 
                               "Mixed ethnicity - Migrant(adj)" = "Migrants - Mixed ethnicity", 
                               "Asian ethnicity - Migrant(adj)" = "Migrants - Asian/Asian British ethnicity",
                               "Black ethnicity - Migrant(adj)" = "Migrants - Black/Black British ethnicity",
                               "Other ethnicity - Migrant(adj)" = "Migrants - Other ethnicity")
cuiud_sub_sens$names <- as.character(cuiud_sub_sens$names)




cuiud_sub_sens_extrarow <- cuiud_sub_sens %>% add_row( .before = 1)

## make forest plot
tabletext <- cbind(c("Analysis", cuiud_sub_sens$sensitivity),
                   c("Subgroup", cuiud_sub_sens$names),
                   c("RR", cuiud_sub_sens$estimate),
                   c("95% CI", cuiud_sub_sens$ci))
dev.new()
png(file = "filepath", width = 5000, height = 2500)
cuiud_sub_sens_fp <- forestplot(tabletext, mean = cuiud_sub_sens_extrarow$estimate, lower=cuiud_sub_sens_extrarow$lower, upper=cuiud_sub_sens_extrarow$upper,
                                    graph.pos = (ncol(tabletext)-1),
                                    hrzl_lines = list("2" = gpar(lty=2)),
                                    clip = c(-2,2),
                                    zero = 1, 
                                    col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                    xlab = "Multivariable Adjusted Rate Ratios",
                                    txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                    xticks = c(0,0.5,1,1.5,2),
                                    ci.vertices = TRUE,
                                    ci.vertices.height = 0.20,
                                    boxsize = 0.15,
                                    title = "Cu-IUD Subgroup and Sensitivity Analyses",
                                    graphwidth = unit(300, 'mm'))
dev.off()

## Forest plot - IUS subgroup and sensitivity ----------------------

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")


## Join ius main + certainty, add subgroup name (consultations)
ius_glm_adj <- ius_glm_adj_unadj%>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
ius_glm_dp_adj <-  ius_glm_dp_adj_unadj %>% slice_tail(n=2)  %>% mutate(sensitivity = c("Stratification by certainty of migration status", ""))
ius_glm_adj_ethnicity <-  ius_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                       names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                       names == "Mixed ethnicity - Migrant(adj)" | 
                                                                       names == "Asian ethnicity - Migrant(adj)" | 
                                                                       names == "Black ethnicity - Migrant(adj)" | 
                                                                       names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = c("Stratification by ethnicity", "", "", "", "", ""))
ius_glm_adj_em <- ius_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
ius_glm_adj_pregmin <- ius_glm_adj_unadj_pregmin %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - minimum pregnancy durations")



ius_sub_sens <- full_join(ius_glm_adj, ius_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                    "lower" = "lower", "upper"= "upper",
                                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>%
  full_join(ius_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                            "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci","p" = "p", "sensitivity" = "sensitivity")) %>% 
  full_join(ius_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                     "lower" = "lower", "upper"= "upper",
                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  full_join(ius_glm_adj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                          "lower" = "lower", "upper"= "upper",
                                          "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>%   
  add_row(.before = 2) %>%   
  add_row(.before = 5) %>%   
  add_row(.before = 12) %>%   
  add_row(.before = 14) 


ius_sub_sens$names <- factor(ius_sub_sens$names,  c("Migrant(adj)", "Definite Migrants(adj)", "Probable Migrants(adj)",
                                                        "White British ethnicity - Migrant(adj)", "White Non-British ethnicity - Migrant(adj)",
                                                        "Mixed ethnicity - Migrant(adj)",  "Asian ethnicity - Migrant(adj)",
                                                        "Black ethnicity - Migrant(adj)",  "Other ethnicity - Migrant(adj)"))
ius_sub_sens$names <- recode(ius_sub_sens$names,
                               "Migrant(adj)" = "Migrants", 
                               "Definite Migrants(adj)" = "Definite Migrants", 
                               "Probable Migrants(adj)" = "Probable Migrants",
                               "White British ethnicity - Migrant(adj)" = "Migrants - White British ethnicity",  
                               "White Non-British ethnicity - Migrant(adj)" = "Migrants - White Non-British ethnicity", 
                               "Mixed ethnicity - Migrant(adj)" = "Migrants - Mixed ethnicity", 
                               "Asian ethnicity - Migrant(adj)" = "Migrants - Asian/Asian British ethnicity",
                               "Black ethnicity - Migrant(adj)" = "Migrants - Black/Black British ethnicity",
                               "Other ethnicity - Migrant(adj)" = "Migrants - Other ethnicity")
ius_sub_sens$names <- as.character(ius_sub_sens$names)


ius_sub_sens_extrarow <- ius_sub_sens %>% add_row( .before = 1)

## make forest plot
tabletext <- cbind(c("Analysis", ius_sub_sens$sensitivity),
                   c("Subgroup", ius_sub_sens$names),
                   c("RR", ius_sub_sens$estimate),
                   c("95% CI", ius_sub_sens$ci))
dev.new()
png(file = "filepath", width = 5000, height = 2500)
ius_sub_sens_fp <- forestplot(tabletext, mean = ius_sub_sens_extrarow$estimate, lower=ius_sub_sens_extrarow$lower, upper=ius_sub_sens_extrarow$upper,
                                graph.pos = (ncol(tabletext)-1),
                                hrzl_lines = list("2" = gpar(lty=2)),
                                clip = c(-2,2),
                                zero = 1, 
                                col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                xlab = "Multivariable Adjusted Rate Ratios",
                                txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                xticks = c(0,0.5,1,1.5,2),
                                ci.vertices = TRUE,
                                ci.vertices.height = 0.20,
                                boxsize = 0.15,
                                title = "IUS Subgroup and Sensitivity Analyses",
                                graphwidth = unit(300, 'mm'))
dev.off()

## Forest plot - SDI subgroup and sensitivity ----------------------

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")


## Join sdi main + certainty, add subgroup name (consultations)
sdi_glm_adj <- sdi_glm_adj_unadj%>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
sdi_glm_dp_adj <-  sdi_glm_dp_adj_unadj %>% slice_tail(n=2)  %>% mutate(sensitivity = c("Stratification by certainty of migration status", ""))
sdi_glm_adj_ethnicity <-  sdi_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                       names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                       names == "Mixed ethnicity - Migrant(adj)" | 
                                                                       names == "Asian ethnicity - Migrant(adj)" | 
                                                                       names == "Black ethnicity - Migrant(adj)" | 
                                                                       names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = c("Stratification by ethnicity", "", "", "", "", ""))
sdi_glm_adj_em <- sdi_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
sdi_glm_adj_pregmin <- sdi_glm_adj_unadj_pregmin %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - minimum pregnancy durations")



sdi_sub_sens <- full_join(sdi_glm_adj, sdi_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                    "lower" = "lower", "upper"= "upper",
                                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>%
  full_join(sdi_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                            "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci","p" = "p", "sensitivity" = "sensitivity")) %>% 
  full_join(sdi_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                     "lower" = "lower", "upper"= "upper",
                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  full_join(sdi_glm_adj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                          "lower" = "lower", "upper"= "upper",
                                          "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>%   
  add_row(.before = 2) %>%   
  add_row(.before = 5) %>%   
  add_row(.before = 12) %>%   
  add_row(.before = 14) 

sdi_sub_sens$names <- factor(sdi_sub_sens$names,  c("Migrant(adj)", "Definite Migrants(adj)", "Probable Migrants(adj)",
                                                        "White British ethnicity - Migrant(adj)", "White Non-British ethnicity - Migrant(adj)",
                                                        "Mixed ethnicity - Migrant(adj)",  "Asian ethnicity - Migrant(adj)",
                                                        "Black ethnicity - Migrant(adj)",  "Other ethnicity - Migrant(adj)"))
sdi_sub_sens$names <- recode(sdi_sub_sens$names,
                               "Migrant(adj)" = "Migrants", 
                               "Definite Migrants(adj)" = "Definite Migrants", 
                               "Probable Migrants(adj)" = "Probable Migrants",
                               "White British ethnicity - Migrant(adj)" = "Migrants - White British ethnicity",  
                               "White Non-British ethnicity - Migrant(adj)" = "Migrants - White Non-British ethnicity", 
                               "Mixed ethnicity - Migrant(adj)" = "Migrants - Mixed ethnicity", 
                               "Asian ethnicity - Migrant(adj)" = "Migrants - Asian/Asian British ethnicity",
                               "Black ethnicity - Migrant(adj)" = "Migrants - Black/Black British ethnicity",
                               "Other ethnicity - Migrant(adj)" = "Migrants - Other ethnicity")
sdi_sub_sens$names <- as.character(sdi_sub_sens$names)


sdi_sub_sens_extrarow <- sdi_sub_sens %>% add_row( .before = 1)

## make forest plot
tabletext <- cbind(c("Analysis", sdi_sub_sens$sensitivity),
                   c("Subgroup", sdi_sub_sens$names),
                   c("RR", sdi_sub_sens$estimate),
                   c("95% CI", sdi_sub_sens$ci))
dev.new()
png(file = "filepath", width = 5000, height = 2500)
sdi_sub_sens_fp <- forestplot(tabletext, mean = sdi_sub_sens_extrarow$estimate, lower=sdi_sub_sens_extrarow$lower, upper=sdi_sub_sens_extrarow$upper,
                                graph.pos = (ncol(tabletext)-1),
                                hrzl_lines = list("2" = gpar(lty=2)),
                                clip = c(-2,2),
                                zero = 1, 
                                col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                xlab = "Multivariable Adjusted Rate Ratios",
                                txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                xticks = c(0,0.5,1,1.5,2),
                                ci.vertices = TRUE,
                                ci.vertices.height = 0.20,
                                boxsize = 0.15,
                                title = "SDI Subgroup and Sensitivity Analyses",
                                graphwidth = unit(300, 'mm'))
dev.off()

## Forest plot - POI subgroup and sensitivity ----------------------

load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")
load(file = "filepath")



## Join poi main + certainty, add subgroup name (consultations)
poi_glm_adj <- poi_glm_adj_unadj%>% filter(names=="Migrant(adj)") %>% mutate(sensitivity = "Migrants - whole CPRD cohort")
poi_glm_dp_adj <-  poi_glm_dp_adj_unadj %>% slice_tail(n=2)  %>% mutate(sensitivity = c("Stratification by certainty of migration status", ""))
poi_glm_adj_ethnicity <-  poi_glm_adj_unadj_ethnicity %>% filter(names == "White British ethnicity - Migrant(adj)" | 
                                                                       names == "White Non-British ethnicity - Migrant(adj)" |  
                                                                       names == "Mixed ethnicity - Migrant(adj)" | 
                                                                       names == "Asian ethnicity - Migrant(adj)" | 
                                                                       names == "Black ethnicity - Migrant(adj)" | 
                                                                       names == "Other ethnicity - Migrant(adj)" ) %>% 
  mutate(sensitivity = c("Stratification by ethnicity", "", "", "", "", ""))
poi_glm_adj_em <- poi_glm_adj_unadj_em %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - exact matched cohort")
poi_glm_adj_pregmin <- poi_glm_adj_unadj_pregmin %>% filter(names=="Migrant(adj)")  %>% mutate(sensitivity = "Migrants - minimum pregnancy durations")



poi_sub_sens <- full_join(poi_glm_adj, poi_glm_dp_adj, by = c("names" = "names", "estimate" = "estimate",
                                                                    "lower" = "lower", "upper"= "upper",
                                                                    "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>%
  full_join(poi_glm_adj_ethnicity, by = c("names" = "names", "estimate" = "estimate",
                                            "lower" = "lower", "upper"= "upper", "ci" = "ci", "irr_ci" = "irr_ci","p" = "p", "sensitivity" = "sensitivity")) %>% 
  full_join(poi_glm_adj_em, by = c("names" = "names", "estimate" = "estimate",
                                     "lower" = "lower", "upper"= "upper",
                                     "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>% 
  full_join(poi_glm_adj_pregmin, by = c("names" = "names", "estimate" = "estimate",
                                          "lower" = "lower", "upper"= "upper",
                                          "p" = "p", "ci" = "ci", "irr_ci" = "irr_ci", "sensitivity" = "sensitivity")) %>%   
  add_row(.before = 2) %>%   
  add_row(.before = 5) %>%   
  add_row(.before = 12) %>%   
  add_row(.before = 14) 

poi_sub_sens$names <- factor(poi_sub_sens$names,  c("Migrant(adj)", "Definite Migrants(adj)", "Probable Migrants(adj)",
                                                        "White British ethnicity - Migrant(adj)", "White Non-British ethnicity - Migrant(adj)",
                                                        "Mixed ethnicity - Migrant(adj)",  "Asian ethnicity - Migrant(adj)",
                                                        "Black ethnicity - Migrant(adj)",  "Other ethnicity - Migrant(adj)"))
poi_sub_sens$names <- recode(poi_sub_sens$names,
                               "Migrant(adj)" = "Migrants", 
                               "Definite Migrants(adj)" = "Definite Migrants", 
                               "Probable Migrants(adj)" = "Probable Migrants",
                               "White British ethnicity - Migrant(adj)" = "Migrants - White British ethnicity",  
                               "White Non-British ethnicity - Migrant(adj)" = "Migrants - White Non-British ethnicity", 
                               "Mixed ethnicity - Migrant(adj)" = "Migrants - Mixed ethnicity", 
                               "Asian ethnicity - Migrant(adj)" = "Migrants - Asian/Asian British ethnicity",
                               "Black ethnicity - Migrant(adj)" = "Migrants - Black/Black British ethnicity",
                               "Other ethnicity - Migrant(adj)" = "Migrants - Other ethnicity")
poi_sub_sens$names <- as.character(poi_sub_sens$names)


poi_sub_sens_extrarow <- poi_sub_sens %>% add_row( .before = 1)

## make forest plot
tabletext <- cbind(c("Analysis", poi_sub_sens$sensitivity),
                   c("Subgroup", poi_sub_sens$names),
                   c("RR", poi_sub_sens$estimate),
                   c("95% CI", poi_sub_sens$ci))
dev.new()
png(file = "filepath", width = 5000, height = 2500)
poi_sub_sens_fp <- forestplot(tabletext, mean = poi_sub_sens_extrarow$estimate, lower=poi_sub_sens_extrarow$lower, upper=poi_sub_sens_extrarow$upper,
                                graph.pos = (ncol(tabletext)-1),
                                hrzl_lines = list("2" = gpar(lty=2)),
                                clip = c(-2,2),
                                zero = 1, 
                                col = fpColors(box="mediumblue", lines = "black", zero="black", hrz_lines = "black"),
                                xlab = "Multivariable Adjusted Rate Ratios",
                                txt_gp = fpTxtGp(ticks=gpar(cex=4), xlab=gpar(cex=4), cex = 4),
                                xticks = c(0,0.5,1,1.5,2),
                                ci.vertices = TRUE,
                                ci.vertices.height = 0.20,
                                boxsize = 0.15,
                                title = "POI Subgroup and Sensitivity Analyses",
                                graphwidth = unit(300, 'mm'))
dev.off()
