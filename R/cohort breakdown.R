library(fasttime)
library(data.table)
library(readxl)
library(lubridate)
source("R/database functions.R")

## Read in data

  cohort_A <- readInData("data-raw/src_data/COHORT_A_1999_2015_WITHOUT_PII.txt")
  

## Change ADMIDATE and DISDATE to an actual date for sorting

  cohort_A[, ADMIDATE := as.Date(ADMIDATE, format = "%Y-%m-%d")]
  

## Read in risk codes from spreadsheet, and risk table from exported Rds object 

  diagnosis_code_table <- data.table(read_excel("reference_data/ICD OPCS Codes.xlsx",
                                                sheet = "Diagnosis Cohort Codes",
                                                col_names = TRUE,
                                                col_types = "text",
                                                trim_ws = TRUE))

  
#### Needs changing if have re-run high risk identifying file at any point ######  
    
  complete_risk_table <- readRDS("data/datasets/complete_risk_table_UoS_2021-06-11.rds")
  

## Create IE codes list from first col of spreadsheet, with two different time gaps
  
  ie_codes_narrow <- prepareCodeList(diagnosis_code_table[Diagnosis == "Infective endocarditis (narrow)", ICD_Code])
  
  ie_codes_broad_primary <- prepareCodeList(diagnosis_code_table[Diagnosis == "Infective endocarditis (broad)", ICD_Code])
  ie_codes_broad_secondary <- prepareCodeList(diagnosis_code_table[(Diagnosis == "Infective endocarditis (broad)"
                                                   & Fields_looked_in == "Primary and secondary"), ICD_Code])

    
## Create and save main IE cohort(s) - both narrow (only I33.0 in primary), and wide (all codes except I38 secondary)
# time_gap is used for both time between valid new admissions, and therefore, the amount of time from 2010-04-01 until first admission can be taken

  IE_cohort_narrow <- multipleAdmissionsAndUnique(cohort_A,
                                                  code_list_primary = ie_codes_narrow,
                                                  code_list_secondary = NULL,
                                                  "ICD",
                                                  time_gap = 180,
                                                  have_history = TRUE,
                                                  condition = "Infective Endocarditis narrow")
  
    
  IE_cohort_broad <- multipleAdmissionsAndUnique(cohort_A,
                                                  code_list_primary = ie_codes_broad_primary,
                                                  code_list_secondary = ie_codes_broad_secondary,
                                                  "ICD",
                                                  time_gap = 180,
                                                  have_history = TRUE,
                                                  condition = "Infective Endocarditis broad")
  
  ## Bind cohort

  IE_spells <- rbind(IE_cohort_narrow, IE_cohort_broad)
  
  stopifnot(IE_cohort_narrow[, .N] + IE_cohort_broad[, .N] == IE_spells[, .N])
  

  ## Apply risk values to each admissions date
  
     IE_spells_risk <- assignRiskCategory(complete_risk_table, IE_spells, "ADMIDATE")
  
  
  ## Add discharge information to cohort
  
     IE_spells_risk_discharge <- findDischargeInformation(cohort_A, IE_spells_risk)
  
     
  ## Add if admission was a non-elective admission method or not
  
     IE_spells_risk_discharge_admission <- findAdmissionsMethod(cohort_A, IE_spells_risk_discharge)
  
  
  ## Apply any causal organism diagnosis codes
  
     IE_spells_risk_discharge_admission_causal <- assignCausalOrganism(cohort_A, IE_spells_risk_discharge_admission)

     
  ## Change all dates back to strings before printing so as know they are stored correctly and read in the same
     
     date_cols <- c("ADMIDATE", "EPISTART")
     
     final_IE_admission_cases <- IE_spells_risk_discharge_admission_causal[, (date_cols) := lapply(.SD, as.character), .SDcols = date_cols]
     
  
  ## Save as an R object then write to csv for statistician   
     
    saveRDS(final_IE_admission_cases, paste0("data/datasets/IE_cohort_risk_discharge_causal_180_UoS_", Sys.Date(),".rds"))
  
    fwrite(final_IE_admission_cases, file = paste0("data/data_for_stats/IE_dataset_", Sys.Date(), ".csv"))
  
  
  
## Create and save a second IE cohort with a different time gap, with assigned risk values.

# NB: Misspelling of Endoca-R-ditis    
      
  IE_cohort_395 <- multipleAdmissionsAndUnique(cohort_A,
                                          code_list_primary = ie_codes_narrow,
                                          code_type = "ICD",
                                          time_gap = 395,
                                          have_history = TRUE,
                                          condition = "Infective Endocaditis")
  
  
  IE_risk_assigned_395 <- assignRiskCategory(complete_risk_table, IE_cohort_395, "ADMIDATE")
  
  
  saveRDS(IE_risk_assigned_395, paste0("data/outputs/rds outputs/IE_cohort_395_UoS_", Sys.Date(),".rds"))

  
## Extra code for getting a table/print out
  
  IE_risk_assigned[, .N, by = risk_at_index]
  
  
  IE_risk_assigned[, floored_date := floor_date(ADMIDATE, unit = "month")]
  setorder(IE_risk_assigned, floored_date)
  risk_summary <- IE_risk_assigned[, .N, by = list(risk_category, floored_date)]
  
  
  test <- dcast(risk_summary, floored_date ~ risk_category, value.var = "N")
  risk_summary <- dcast(risk_summary, floored_date ~ risk_category, value.var = "N")
  risk_summary[is.na(risk_summary)] <- 0
  write.csv(risk_summary, file = "risk categories by month.csv")
 
