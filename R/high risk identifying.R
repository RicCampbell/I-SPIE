library(data.table)
library(lubridate)
library(readxl)
source("R/database functions.R")

# Script that returns a table of high risk individuals and the date (earliest) that they became high risk both temporary and permanent ########


## Read in data (remove second argument to use full data) ########

  cohort_A <- readInData("data-raw/src_data/COHORT_A_1999_2015_WITHOUT_PII.txt")
  

## Paste midnight time to date before converting into a time stamp to avoid errors due to daylight savings  
  
  cohort_A[, ADMIDATE := paste0(cohort_A[, ADMIDATE], " 00:00:00")]
  cohort_A[, ADMIDATE := as.Date(fasttime::fastPOSIXct(ADMIDATE))]


## Create list of grouped (by risk type) codes from text file ########
## This could be changes to read in all from one spreadsheet
  
  risk_code_table <- data.table(read_excel("reference_data/ICD OPCS Codes.xlsx",
                          sheet = "Risk Codes",
                          col_names = TRUE,
                          col_types = "text",
                          trim_ws = TRUE))


## Load all lists for categories of risk ########
  
  ie_codes <- prepareCodeList(risk_code_table[risk_category == "icd_10_for_high_risk_infective_endocarditis", codes])
  ie_codes_primary <- prepareCodeList(risk_code_table[risk_category == "icd_10_for_high_risk_infective_endocarditis_primary_only", codes])
  replace_valve_codes <- prepareCodeList(risk_code_table[risk_category == "opcs_4_for_high_risk_prosthetic_replacement_of_a_heart_valve", codes])
  repair_valve_codes <- prepareCodeList(risk_code_table[risk_category == "opcs_4_for_high_risk_valve_repair_using_prosthetic_material", codes])
  prosthetic_heart_codes <- prepareCodeList(risk_code_table[risk_category == "opcs_4_for_high_risk_prosthetic_heart_ventricular_assist_device", codes])
  shunt_conduit_codes <- prepareCodeList(risk_code_table[risk_category == "opcs_4_for_high_risk_congenital_heart_disease_palliative_shunt_or_conduit", codes])
  cyanotic_codes <- prepareCodeList(risk_code_table[risk_category == "icd_10_for_high_risk_unreparied_cyanotic_congenital_heart_condition", codes])
  repaired_congenital_heart_condition <- prepareCodeList(risk_code_table[risk_category == "opcs_4_for_high_completely_repaired_CHC_defect_with_prosthetic_material_or_device", codes])
  rheumatic_fever_codes <- prepareCodeList(risk_code_table[risk_category == "icd_10_for_moderate_risk_previous_rheumatic_fever", codes])
  non_rheumatic_codes <- prepareCodeList(risk_code_table[risk_category == "icd_10_for_moderate_risk_non_rheumatic_valve_disease", codes])
  hypertrophic_codes <- prepareCodeList(risk_code_table[risk_category == "icd_10_for_moderate_risk_hypertrophic_cardiomyopathy", codes])
  congenital_valve_codes <- prepareCodeList(risk_code_table[risk_category == "icd_10_for_moderate_risk_congenital_valve_anomalies", codes])
  

## Group these into high and moderate lists ########
  
  ICD_codes_high <- c(ie_codes, ie_codes_primary)
  OPCS_codes_high <- c(replace_valve_codes, repair_valve_codes, prosthetic_heart_codes, shunt_conduit_codes)
  ICD_codes_moderate <- c(rheumatic_fever_codes, non_rheumatic_codes, hypertrophic_codes, congenital_valve_codes)


## Melt the two tables based on relevant codes ########

  melted_cohort_A_ICD <- meltAndTrim(cohort_A, "ICD")
  melted_cohort_A_OPCS <- meltAndTrim(cohort_A, "OPCS")


## Label all permanent high risk events including ones that apply to only primary diagnosis field

  melted_cohort_A_ICD[, perm_high_risk := (diag_code %in% ie_codes)]
  melted_cohort_A_ICD[outcome_numb == 1, perm_high_risk := (diag_code %in% ie_codes_primary)]
  melted_cohort_A_OPCS[, perm_high_risk := (diag_code %in% OPCS_codes_high)]


## Label all temporary high risk events

  melted_cohort_A_OPCS[, temp_high_risk := (diag_code %in% repaired_congenital_heart_condition)]


## Label all moderate risk events

  melted_cohort_A_ICD[, moderate_risk := (diag_code %in% ICD_codes_moderate)]

    
## Label all repairable high risk events

  melted_cohort_A_ICD[, repairable_high_risk := (diag_code %in% cyanotic_codes)]

## Label diagnosis/procedure type of all conditions

  melted_cohort_A_ICD[diag_code %in% ie_codes, risk_name := "Previous IE"]
  melted_cohort_A_ICD[diag_code %in% ie_codes_primary, risk_name := "Previous IE - I38X"]
  melted_cohort_A_OPCS[diag_code %in% replace_valve_codes, risk_name := "Replacement Heart Valve"]
  melted_cohort_A_OPCS[diag_code %in% repair_valve_codes, risk_name := "Repaired Heart Valve"]
  melted_cohort_A_OPCS[diag_code %in% shunt_conduit_codes, risk_name := "Palliative Shunt or Conduit"]
  melted_cohort_A_ICD[diag_code %in% cyanotic_codes, risk_name := "Cyanotic Heart Disease"]
  melted_cohort_A_OPCS[diag_code %in% prosthetic_heart_codes, risk_name := "Prosthetic Heart/VAD"]
  melted_cohort_A_ICD[diag_code %in% congenital_valve_codes, risk_name := "Congenital Valve Anomalies"]
  melted_cohort_A_ICD[diag_code %in% hypertrophic_codes, risk_name := "Hypertrophic Cardiomyopathy"]
  melted_cohort_A_ICD[diag_code %in% non_rheumatic_codes, risk_name := "Non-Rheumatic Valve"]
  melted_cohort_A_ICD[diag_code %in% rheumatic_fever_codes, risk_name := "Previous Rheumatic Fever"]
  melted_cohort_A_OPCS[diag_code %in% repaired_congenital_heart_condition, risk_name := "Repaired Congenital Heart Disease"]


## Bind the two tables together, keeping only those with some form of risk. Remove previous melted data ########

  melted_cohort_risk <- rbind(melted_cohort_A_ICD[perm_high_risk == TRUE |moderate_risk == TRUE | repairable_high_risk == TRUE,
                                    .(ENCRYPTED_HESID, ADMIDATE, EPIORDER, outcome_numb, perm_high_risk, moderate_risk, repairable_high_risk, risk_name, diag_code)],
                                 melted_cohort_A_OPCS[perm_high_risk == TRUE | temp_high_risk == TRUE, 
                                    .(ENCRYPTED_HESID, ADMIDATE, EPIORDER, outcome_numb, perm_high_risk, temp_high_risk, risk_name, diag_code)]
                                 , fill = TRUE)
  
  rm(melted_cohort_A_ICD, melted_cohort_A_OPCS)


## Don't want to remove IE index cases as these are still high risk codes if someone has two index cases.


## Separate out all the types of risk we want: high, moderate, repairable, temp

  high_risk_events <- melted_cohort_risk[perm_high_risk == TRUE]
  moderate_risk_events <- melted_cohort_risk[moderate_risk == TRUE]
  repairable_risk_events <- melted_cohort_risk[repairable_high_risk == TRUE]
  temp_risk_events <- melted_cohort_risk[temp_high_risk == TRUE]


## Order perm risk events, with earliest first for each id, and create a table with all the earliest events for each id

  setorder(high_risk_events, ENCRYPTED_HESID, ADMIDATE, EPIORDER, outcome_numb)
  high_risk_events[, order := 1:.N, by = .(ENCRYPTED_HESID)]
  earliest_perm_table <- high_risk_events[order == 1]
  
## Remove unwanted cols and rename admission date col
  
  earliest_perm_table[, c("moderate_risk", "repairable_high_risk", "temp_high_risk", "order") := NULL]
  earliest_perm_date <- earliest_perm_table[, .(ENCRYPTED_HESID, perm_high_risk_ADMIDATE = ADMIDATE)]
  
  
## Merge in the earliest perm table by id, into each of the other risk event tables
  
  moderate_risk_events <- merge(moderate_risk_events, earliest_perm_date, all.x = TRUE, by = "ENCRYPTED_HESID")
  repairable_risk_events <- merge(repairable_risk_events, earliest_perm_date, all.x = TRUE, by = "ENCRYPTED_HESID")
  temp_risk_events <- merge(temp_risk_events, earliest_perm_date, all.x = TRUE, by = "ENCRYPTED_HESID")
  
  
## Only select the events that occur before a person is identified as perm risk, or if they never become perm risk
  
  moderate_risk_events <- moderate_risk_events[ADMIDATE < perm_high_risk_ADMIDATE | is.na(perm_high_risk_ADMIDATE)]
  repairable_risk_events <- repairable_risk_events[ADMIDATE < perm_high_risk_ADMIDATE | is.na(perm_high_risk_ADMIDATE)]
  temp_risk_events <- temp_risk_events[ADMIDATE < perm_high_risk_ADMIDATE | is.na(perm_high_risk_ADMIDATE)]
  
  
## Set order of moderate, repairable and temp tables
  
  setorder(moderate_risk_events, ENCRYPTED_HESID, ADMIDATE, EPIORDER, outcome_numb)
  setorder(repairable_risk_events, ENCRYPTED_HESID, ADMIDATE, EPIORDER, outcome_numb)
  setorder(temp_risk_events, ENCRYPTED_HESID, ADMIDATE, EPIORDER, outcome_numb)


## Add order by id so can take earliest occurrence as have been ordered by date

  moderate_risk_events[, order := 1:.N, by = .(ENCRYPTED_HESID)]
  repairable_risk_events[, order := 1:.N, by = .(ENCRYPTED_HESID)]


## Collapse the cases of repairable events to ensure not to double count the same event, and associated events

  repairable_risk_events[, time_diff := ADMIDATE - shift(ADMIDATE), by = ENCRYPTED_HESID]
  repairable_risk_events_collapsed <- repairable_risk_events[order == 1 | time_diff > 30]
  
  
## Only want one event per day of temporary events, order and take first as ordered by Epiorder previously
  
  temp_risk_events[, order := 1:.N, by = .(ENCRYPTED_HESID, ADMIDATE)]
  temp_risk_events_daily_distinct <- temp_risk_events[order == 1]
  
  
## Remove unwanted cols
  
  temp_risk_events_daily_distinct[, c("perm_high_risk", "moderate_risk", "repairable_high_risk", "order", "perm_high_risk_ADMIDATE") := NULL]
  

## Re-order repairable events so can find the first case for each person easier and remove unwanted cols

  repairable_risk_events_collapsed[, order := 1:.N, by = .(ENCRYPTED_HESID)]
  repairable_risk_events_collapsed[, c("perm_high_risk", "moderate_risk", "temp_high_risk", "perm_high_risk_ADMIDATE", "time_diff") := NULL]

  
## Take order 1 for moderate risk as is the only one we are interested in, and remove unwanted cols

  earliest_moderate_table <- moderate_risk_events[order == 1]
  earliest_moderate_table[, c("perm_high_risk", "repairable_high_risk", "temp_high_risk", "order", "perm_high_risk_ADMIDATE") := NULL]
  
  
## Put the first repairable event for each person in a new table, and remove unwanted cols
  
  earliest_repairable_table <- repairable_risk_events_collapsed[order == 1]
  earliest_repairable_table[, order := NULL]
  

## Change names col so easier to reference later

  setnames(earliest_perm_table, 2:ncol(earliest_perm_table), paste0(names(earliest_perm_table)[2:ncol(earliest_perm_table)], "_perm"))
  setnames(earliest_moderate_table, 2:ncol(earliest_moderate_table), paste0(names(earliest_moderate_table)[2:ncol(earliest_moderate_table)], "_mod"))
  setnames(earliest_repairable_table, 2:ncol(earliest_repairable_table), paste0(names(earliest_repairable_table)[2:ncol(earliest_repairable_table)], "_repairable1"))
  setnames(temp_risk_events_daily_distinct, 2:ncol(temp_risk_events_daily_distinct), paste0(names(temp_risk_events_daily_distinct)[2:ncol(temp_risk_events_daily_distinct)], "_temp"))


## Merge earliest perm and earliest moderate, then add in earliest repair data

  perm_mod_table <- merge(earliest_perm_table, earliest_moderate_table, all = TRUE, by = "ENCRYPTED_HESID")
  merged_earliest_risk_table <- merge(perm_mod_table, earliest_repairable_table, all = TRUE, by = "ENCRYPTED_HESID")

  
## Stopifnot that sums everything to check has been merged correctly and nothing lost/added  

  merged_earliest_risk_table_count <- merged_earliest_risk_table[, .N]
  stopifnot(merged_earliest_risk_table_count == length(unique(c(earliest_perm_table[, ENCRYPTED_HESID],
                  earliest_moderate_table[, ENCRYPTED_HESID],
                  earliest_repairable_table[, ENCRYPTED_HESID]))))

  
## Create a copy of the distinct temp events, as this will be altered during the function calls
## Set index to 1, and run function once to find the first repair to match the first repair already found
  
  remaining_temp_events <- copy(temp_risk_events_daily_distinct)
  evoling_risk_table <- copy(merged_earliest_risk_table)
  index <- 1L
  subsequent_repair <- findSubsequentRepair(evoling_risk_table, temp_risk_events_daily_distinct, index)
  
  
## do/while loop to build repairable and repair events until no more to be added
## Each loop adds a repair event, finds next repairable, adds repairable, finds next repair  
  
  while(subsequent_repair[, .N] > 0){
    
      remaining_temp_events_count <- remaining_temp_events[, .N]
      
      
    ## Find the remaining temp events after taking out the ones from the last added ones (repair)
      
      remaining_temp_events <- fsetdiff(remaining_temp_events, subsequent_repair)
      stopifnot(remaining_temp_events[, .N] == (remaining_temp_events_count - subsequent_repair[, .N]))
    
      
    ## Change names for clarity
      
      setnames(subsequent_repair,
               c("ADMIDATE_temp", "EPIORDER_temp", "outcome_numb_temp", "risk_name_temp", "diag_code_temp", "temp_high_risk_temp"),
               paste0(c("ADMIDATE_repair", "EPIORDER_repair", "outcome_numb_repair", "risk_name_repair", "diag_code_repair", "repair_high_risk_repair"), index))  
      
      
    ## Add the next repair events into table containing all events we have found that we are interested in so far
      
      evoling_risk_table <- merge(evoling_risk_table, subsequent_repair, all.x = TRUE, by = "ENCRYPTED_HESID")
      stopifnot(merged_earliest_risk_table_count == evoling_risk_table[, .N])
      
    ## Increase the index count and call function to find table of next repairable events  
      
      index <- index + 1
      next_subsequent_repairable <- findNextSubsequentRepairable(subsequent_repair, repairable_risk_events_collapsed, index)
    
      
    ## Exit loop if there are no more repairable events found
      
      if(next_subsequent_repairable[, .N] == 0){
        break
      }
    
    ## Add next repairable events into table containing all events we have found that we are interested in so far
      
      evoling_risk_table <- merge(evoling_risk_table, next_subsequent_repairable, all.x = TRUE, by = "ENCRYPTED_HESID")
      stopifnot(merged_earliest_risk_table_count == evoling_risk_table[, .N])
      
      
    ## Call function to find table of next repair events
      
      subsequent_repair <- findSubsequentRepair(evoling_risk_table, temp_risk_events_daily_distinct, index)
  }
  
  
##### All temps remaining in the temp event list are needed as they are before a perm event and not related to a repair event
  
  setorder(remaining_temp_events, ADMIDATE_temp)
  remaining_temp_events[, order := 1:.N, by = ENCRYPTED_HESID]
  
  index <- 1L
  count <- remaining_temp_events[, max(order)]
  

  
  while(index <= count){
    
    subsequent_temp <- remaining_temp_events[order == index]
      
    setnames(subsequent_temp,
             c("ADMIDATE_temp", "EPIORDER_temp", "outcome_numb_temp", "risk_name_temp", "diag_code_temp", "temp_high_risk_temp"),
             paste0(c("ADMIDATE_temp", "EPIORDER_temp", "outcome_numb_temp", "risk_name_temp", "diag_code_temp", "temp_high_risk_temp"), index))
    
    subsequent_temp[, order := NULL]
      
    evoling_risk_table <- merge(evoling_risk_table, subsequent_temp, all = TRUE, by = "ENCRYPTED_HESID")
    stopifnot(melted_cohort_risk[, uniqueN(ENCRYPTED_HESID)] == evoling_risk_table[, .N])
     
    
    index <- index + 1
  }
  
  ## Finalise name of table
  
  complete_risk_table <- copy(evoling_risk_table)
  
##### Tidy up Environment keeping functions and risk table and cohort_A, and save risk table to external object
  
  saveRDS(complete_risk_table, paste0("data/datasets/complete_risk_table_UoS_", Sys.Date(), ".rds"))
  
  function_list <- as.vector(lsf.str())  
  rm(list=setdiff(ls(), c("cohort_A", "cohort_B", function_list)))
  



