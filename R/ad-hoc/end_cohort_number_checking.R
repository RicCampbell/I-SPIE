## Script for quick checking that stats and DM get same numbers to be used for final analysis
## Removing unwanted IE admissions that are left in analysis dataset

## Starting from IE cohort then merging with invasive procedures as easier and might highlight difference better

library(data.table)
library(lubridate)


####~~~~~~~~~~~~~~
  
  ie_invasive <- fread("D:/I-SPIEDM/I-SPIEDM/data/data_for_stats/ie_analysis_all_ie_cases2022-02-14-170649.csv")

  
## Keep broad definition only
  
  ie_cohort_broad <- ie_invasive[ie_condition == "Infective Endocarditis broad"]
  
  
## Keep only first index admission
  
  ie_cohort_broad_first <- ie_cohort_broad[ie_index_admission_number == 1L]
  

## Keep only non-elective admissions
  
  ie_cohort_broad_first_non_elective <- ie_cohort_broad_first[ie_non_elective_admission == TRUE]
  

## Keep only those who stayed over 2 nights or died
  
  ie_cohort_broad_first_non_elective_stay_length <- ie_cohort_broad_first_non_elective[(ie_length_of_stay > 2L | ie_length_of_stay < 0L |  is.na(ie_length_of_stay)) 
                                                                                       | (ie_discharged_alive == FALSE | is.na(ie_discharged_alive))]
  
  
  
## [ie_length_of_stay >=0 & ie_length_of_stay <=7,.N, by = ie_length_of_stay][order(ie_length_of_stay)]