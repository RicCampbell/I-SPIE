## Script for creation of analysis dataset for statistics

library(data.table)
library(lubridate)


## Read in IE cohort

  ie_cohort <- readRDS("D:/I-SPIEDM/I-SPIEDM/data/datasets/IE_cohort_risk_discharge_causal_180_UoS_2022-02-14.rds")
  
  
## Change ADMIDATE to date type
  
  ie_cohort[, ADMIDATE := as.Date(ADMIDATE, format = "%Y-%m-%d")]


####~~~~ Read in different file dependent on selection of procedures want to look at ~~~~~~####    

  invasive_procedures <- readRDS("D:/I-SPIEDM/I-SPIEDM/data/datasets/first_invasive_procedure_per_procedure_per_date_patient_heart_rule.rds")

# ## Only first procedure per month for each person/procedure

## Reduce to only IE cases of interest, and create admission month field

  ie_cohort <- ie_cohort[index_admission == TRUE]
  ie_cohort[, admission_month := floor_date(ADMIDATE, unit = "month")]


## Give all IE and invasive procedures an ID (first half of id name will be added below)

  ie_cohort[, id := 1:.N]
  invasive_procedures[, id := 1:.N]

  
## Change names so can tell fields apart

  setnames(ie_cohort, setdiff(colnames(ie_cohort), "ENCRYPTED_HESID"),
           paste("ie", setdiff(colnames(ie_cohort), "ENCRYPTED_HESID"), sep = "_"))
  
  setnames(invasive_procedures, setdiff(colnames(invasive_procedures), "ENCRYPTED_HESID"),
           paste("invasive", setdiff(colnames(invasive_procedures), "ENCRYPTED_HESID"), sep = "_"))


## Merge together IE cases with all invasive procedures, keep IE admissions with no invasive procedures

  ie_invasive <- merge(ie_cohort,
                       invasive_procedures,
                       by = "ENCRYPTED_HESID",
                       all.x = TRUE)


## Create field with time between IE admission and invasive procedure  

  ie_invasive[, days_to_admission := as.integer(invasive_ADMIDATE - ie_ADMIDATE)]


## Create month splits for time of invasive procedure in relation to IE admission, one for with and one for without day of admissions

  ie_invasive[, months_to_admission_with_admi_day := ceiling(days_to_admission / 30) - 1L]
  ie_invasive[days_to_admission != 0
              , months_to_admission_without_admi_day := ceiling((days_to_admission + 1L) / 30) - 1L]
  
  
## Set months to admission without admi date to 0 when admission and procedure occurs on the same day (NA reserved for no procedures for IE admission)
  
  ie_invasive[days_to_admission == 0, months_to_admission_without_admi_day := 0]
  

## No need to reduce IE admissions as have 10 years of HES data prior to first IE admission in 2010
  
## Want to reduce to records of just the first of each procedure for each person each month
## Have to be careful, as want this done based on records that will be left after reduction
  
  setorder(ie_invasive, ENCRYPTED_HESID, months_to_admission_without_admi_day, invasive_ADMIDATE, invasive_EPISTART, invasive_EPIORDER, invasive_outcome_numb)
  
  ie_invasive[is.na(invasive_valid_procedure_including_heart_rule) | invasive_valid_procedure_including_heart_rule == TRUE,
              procedure_month_order := 1:.N, by = .(ENCRYPTED_HESID, invasive_procedure, months_to_admission_without_admi_day, ie_index_admission_number, ie_condition)]

## Save cohort for stats
  
  save_time <- gsub(" ", "-", gsub(":", "", Sys.time(), fixed = TRUE), fixed = TRUE)
  
  write.csv2(ie_invasive, file = paste0("data/data_for_stats/ie_analysis_all_ie_cases", save_time, ".csv"))
  
  
  
  
  