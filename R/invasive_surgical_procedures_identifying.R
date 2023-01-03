### Script for finding all Invasive Surgical Procedures

library(data.table)
library(readxl)
library(lubridate)
source("R/database functions.R")

## Read in data

  cohort_A <- readInData("data-raw/src_data/COHORT_A_1999_2015_WITHOUT_PII.txt")


## Change ADMIDATE to an actual date for sorting

  cohort_A[, ADMIDATE := as.Date(ADMIDATE, format = "%Y-%m-%d")]


## Read in invasive procedure codes from spreadsheet 

  invasive_surgical_sheet <- data.table(read_excel("reference_data/ICD OPCS Codes.xlsx",
                                                 sheet = "Invasive Surgical Procedures",
                                                 col_names = TRUE,
                                                 col_types = "text",
                                                 trim_ws = TRUE))

  
  invasive_surgical_sheet[, standard_opcs_code := prepareCodeList(opcs_code)]
  
  
## Read in sheet of heart valve repair/replacement codes

  heart_valve_sheet <- data.table(read_excel("reference_data/ICD OPCS Codes.xlsx",
                                                   sheet = "Heart Valve Repair Replacement",
                                                   col_names = TRUE,
                                                   col_types = "text",
                                                   trim_ws = TRUE))
  
  
  heart_valve_sheet[, standard_opcs_code := prepareCodeList(opcs_code)]
  
  
## Melt the cohort, both for opcs codes and icd-10 codes, and identify those that have the correct diagnosis/procedure
## OPCS for invasive medical procedures,
## OPCS for heart valve repair/replacement,

  melted_cohort_opcs <- meltAndTrim(cohort_A, "OPCS")
  
  
## Remove rows where 'No operation performed' "-", or 'Not known' "&"
  
  melted_cohort_opcs <- melted_cohort_opcs[!(diag_code == "-" | diag_code == "&")]


## Check that all codes are of 4 characters and do not contain 3 character codes
  
  stopifnot(min(nchar(melted_cohort_opcs$diag_code), na.rm = TRUE) == 4)
  stopifnot(max(nchar(melted_cohort_opcs$diag_code), na.rm = TRUE) == 4)
  

## Merge with invasive surgical procedure to add information about procedure. Only want records with procedure that is of interest
  
  melted_cohort_invasive_procedure <- merge(melted_cohort_opcs,
                                            invasive_surgical_sheet,
                                            by.x = "diag_code",
                                            by.y = "standard_opcs_code",
                                            all = FALSE)
  
  
## Merge with heart valve procedures as well, again keeping only records of interest
  
  melted_cohort_heart_procedures <- merge(melted_cohort_opcs,
                                          heart_valve_sheet,
                                          by.x = "diag_code",
                                          by.y = "standard_opcs_code",
                                          all = FALSE)
  
  
## Only interested in which hospital admissions have these heart procedures, not counting certain procedure for these
  
  melted_cohort_heart_procedures <- unique(melted_cohort_heart_procedures[, .(ENCRYPTED_HESID, ADMIDATE)])[, heart_repair_replace := TRUE]
  

## Want to treat groups separately, so select the first procedure (not procedure group) for each person (ENCRYPTED_HESID), for each admissions (ADMIDATE)
  
  setorder(melted_cohort_invasive_procedure, ENCRYPTED_HESID, ADMIDATE, EPISTART, EPIORDER, outcome_numb)
  
  melted_cohort_invasive_procedure[, order := 1:.N, by = .(ENCRYPTED_HESID, ADMIDATE, procedure)]
  
  first_procedure_patient_date <- melted_cohort_invasive_procedure[order == 1][, order := NULL]
  
  
## Merge in if there was a heart repair/replacement for these admissions
  
  first_procedure_patient_date_heart <- merge(first_procedure_patient_date,
                                              melted_cohort_heart_procedures,
                                              by = c("ENCRYPTED_HESID", "ADMIDATE"),
                                              all.x = TRUE)


## Check hasn't duplicated any admissions
  
  stopifnot(first_procedure_patient_date_heart[, .N, by = .(ENCRYPTED_HESID, ADMIDATE, procedure)][N > 1, .N] == 0)
  
  stopifnot(first_procedure_patient_date_heart[, .N] == first_procedure_patient_date[, .N])
  
  
## Create field to indicate whether to use this procedure or not
  
  first_procedure_patient_date_heart[, valid_procedure_including_heart_rule := (heart_repair_replace == TRUE & !(procedure == "coronary_artery_bypass"
                                                                                                                 | procedure == "percutaneous_coronary_procs_stents"))]
  
## Change NAs to TRUE
  
  first_procedure_patient_date_heart[is.na(heart_repair_replace), valid_procedure_including_heart_rule := TRUE]
  
  
## Want to look at if treat each code separate - commented out as not looking at this for now
  
  # melted_cohort_invasive_procedure[, order := NULL]
  # 
  # setorder(melted_cohort_invasive_procedure, ENCRYPTED_HESID, ADMIDATE, EPISTART, EPIORDER, outcome_numb)
  # 
  # melted_cohort_invasive_procedure[, order := 1:.N, by = .(ENCRYPTED_HESID, ADMIDATE, diag_code)]
  # 
  # first_code_patient_date <- melted_cohort_invasive_procedure[order == 1][, order := NULL]

  
## Write this out for now, but will want to change to function later
  
  save_time <- gsub(" ", "-", gsub(":", "", Sys.time(), fixed = TRUE), fixed = TRUE)
    
  saveRDS(first_procedure_patient_date_heart, paste0(file = "data/datasets/first_invasive_procedure_per_procedure_per_date_patient_heart_rule",
                                                     save_time, ".rds"))
    
  # saveRDS(first_code_patient_date, file = "data/datasets/first_invasive_procedure_per_code_per_date_patient.rds")
  
  
  