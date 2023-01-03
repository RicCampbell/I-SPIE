# Database functions ########

## read in data, tidy up ########

  readInData <- function(input_data_location, rows_wanted = Inf){
    cohort_list <- lapply(input_data_location, fread, header = TRUE, sep = ",", na.strings = "",
                          colClasses = "character", nrows = rows_wanted)
    cohort <- rbindlist(cohort_list)
    return(cohort)

}


## Takes a col of diagnosis codes and returns list of codes with blanks and decimal point removed

  prepareCodeList <- function (code_list) {
    code_list <- code_list[!is.na(code_list)]
    code_list <- gsub("[.]","",code_list)
    return(code_list)
  }


## Melts the database on the diag/opertn fields, removes blank elements, adds 20 to outcome number for opertn for ordering purposes ########

  meltAndTrim <- function (cohort, code_type, extra_fields = NULL)  {
    
    if (code_type == "ICD") {
      code_fields <- paste0("DIAG_", formatC(1:20, width = 2, format = "d", flag = "0"))
    }
    
    else if (code_type == "OPCS") {
      code_fields <- paste0("OPERTN_", formatC(1:24, width = 2, format = "d", flag = "0"))
    }
    
    else  {
      print("code_type argument incorrect")
      break
    }
    
    standard_fields <- c("ENCRYPTED_HESID", "ADMIDATE", "EPIORDER", "EPIKEY", "EPISTART", "ADMIMETH")
    wanted_fields <- c(standard_fields, extra_fields)
    
    melted_cohort <- melt(cohort[, .SD, .SDcols = c(wanted_fields, code_fields)],
                         measure.vars = code_fields, variable.name = "outcome_numb", value.name = "diag_code")
    
    melted_cohort <- melted_cohort[!is.na(diag_code)]
    melted_cohort[, diag_code := substr(diag_code, 1, 4)]
    melted_cohort[, outcome_numb := as.character(outcome_numb)]
    melted_cohort[, outcome_numb := substr(outcome_numb, (nchar(outcome_numb)-1), nchar(outcome_numb))]
    melted_cohort[, outcome_numb := as.integer(outcome_numb)]
    
    if (code_type == "OPCS") {
      melted_cohort[, outcome_numb := (outcome_numb + 20)]
    }
    
    return(melted_cohort)
  }


## Function to determine the number of admission of a condition, takes as arguments;
  # cohort - the cohort to find the number of admissions for
  # code_list_primary - a list of diagnosis/procedure codes for identifying relevant admissions from primary field
  # code_list_secondary - a list of diagnosis/procedure codes for identifying relevant admissions from all secondary fields (defaults to NULL)
  # code_type - ICD or OPCS, depending on if admissions are identified by diagnosis or procedure
  # time_gap - the number of days between admissions needed to count as separate events
  # have_history - does the cohort contain data from before date events are being counted from
### Demographic merge at the end is what adds a lot of processing time
  
  
  multipleAdmissionsAndUnique <- function (cohort, code_list_primary, code_list_secondary = NULL,
                                           code_type, time_gap = 0, have_history = FALSE, condition = NULL) {
    
  ## Melt the cohort, and identify those that have the correct diagnosis/procedure

    melted_cohort <- meltAndTrim(cohort, code_type)
    melted_cohort[, condition_flag := ((outcome_numb == 1 & diag_code %in% code_list_primary) | (diag_code %in% code_list_secondary))]
    diagnosed_melted_cohort <- melted_cohort[(condition_flag == TRUE)]
    diagnosed_melted_cohort[, condition := condition]
    
    
  ## Set order based on episode start and episode order, give order numerical value 
    
    setorder(diagnosed_melted_cohort, EPISTART, EPIORDER, EPIKEY)                                              
    diagnosed_melted_cohort[, order := 1:.N, by = .(ENCRYPTED_HESID, ADMIDATE)]
    
    
  ## Take the first event for each day (ADMIDATE) for every person, and remove order col for both tables
    
    condition_spells <- diagnosed_melted_cohort[order == 1][, order := NULL]
    diagnosed_melted_cohort[, order := NULL]
  
    
  ## Re-do order based on ADMIDATE, and give numerical value to each 'spell' for each person
    
    setorder(condition_spells, ADMIDATE)
    condition_spells[, spell_number := 1:.N, by = ENCRYPTED_HESID]
  
    
  ## Create a field that shows how much time has passed between events if a person has multiple events
    
    condition_spells[, time_diff := ADMIDATE - shift(ADMIDATE), by = ENCRYPTED_HESID]
    
  
  ## if clause because for the IE cohort we have history of patients pre-2010 admissions, so do not require a hard 'wash-out' period
    
    if (have_history == FALSE) {
      
      condition_spells[(spell_number == 1 | time_diff > time_gap) & (ADMIDATE >= as.Date("2010-04-01") + time_gap), index_admission := TRUE]
      condition_spells[(time_diff <= time_gap) | (ADMIDATE < as.Date("2010-04-01") + time_gap), index_admission := FALSE]
    }
    
    else if (have_history == TRUE) {
      
      condition_spells[(spell_number == 1 | time_diff > time_gap), index_admission := TRUE]
      condition_spells[time_diff <= time_gap, index_admission := FALSE]
      condition_spells <- condition_spells[ADMIDATE >= "2010-04-01"]
    }
  
  ## Add field for which admission number it is for each person, and another for which index admission it is
  ## Remove spell_number as not useful for analysis
    
    condition_spells[, admission_number := 1:.N, by = ENCRYPTED_HESID]
    
    condition_spells[index_admission == TRUE, index_admission_number := 1:.N, by = ENCRYPTED_HESID]
   
    condition_spells[, spell_number := NULL]
    
    
  ## Add back in required demographics using EPIKEY from original cohort, and return this
  
    admissions_with_demographics <- merge(condition_spells, cohort[, .(EPIKEY, ADMIAGE, SEX)] , all.x = TRUE, by = "EPIKEY")
    
    return(admissions_with_demographics)
  }
  
## Function for finding the discharge date and method for a cohort
  # cohort - the full cohort of events to find the discharge information in
  # diagnosed_cases - the pre-found cohort of people with a condition with admission date for start of spell
  # returns cohort of people with condition, plus discharge date and method for the end of that spell
  
  findDischargeInformation <- function (cohort, diagnosed_cases) {
    
    ## Create copies
    
    copy_cohort <- copy(cohort)
    copy_diagnosed_cases <- copy(diagnosed_cases)

    ## Set order of full cohort to put the latest discharge date, latest episode start date, and highest episode order first
    
    setorder(copy_cohort, -DISDATE, -EPISTART, -EPIORDER, EPIKEY, na.last = TRUE)                                              
    copy_cohort[, order := 1:.N, by = .(ENCRYPTED_HESID, ADMIDATE)]
  
  
    ## Take the event with the latest discharge date for each ADMIDATE-ENCRYPTED HESID spell
    
    end_of_spells <- copy_cohort[order == 1][, order := NULL]
    
    
    ## Remove unwanted cols from start events, then merge discharge date and discharge method into table of spells
    
    condition_spells <- merge(copy_diagnosed_cases, end_of_spells[, .(ENCRYPTED_HESID, ADMIDATE, DISDATE, DISMETH)],
                              all.x = TRUE, by = c("ENCRYPTED_HESID", "ADMIDATE"))
  
    
    stopifnot(condition_spells[, .N] == copy_diagnosed_cases[, .N])
    
   ## Create col for if the patient was discharged alive or not
  
    condition_spells[, discharged_alive := DISMETH %in% c(1, 2 , 3 , 5)]
    condition_spells[DISMETH %in% 8:9 | is.na(DISMETH), discharged_alive := NA]
    
    
   ## Change discharge to a date format so can use it in date maths
    
    condition_spells[, DISDATE := as.Date(DISDATE, format = "%Y-%m-%d")]
  
     ## Create field for time spent for admissions, so can filter out those < 3 days
    
    condition_spells[, length_of_stay := DISDATE - ADMIDATE]
    
    
    ## Rename DISDATE (makes it more obvious that it has been found correctly rather than just added at time of finding case admission)
    
    setnames(condition_spells, "DISDATE", "discharge_date")
    
    
    ## Remove DISMETH as not asked for and have created discharged_alive from this
    
    condition_spells[, DISMETH := NULL]
    
  
    ## Return diagnosis condition with extra discharge information
    
    return(condition_spells)
    
  }
  
## Function for finding if admission was non-elective or not - if any admission method for the persons spell was non-elective, this will count as non-elective
  # cohort - the full cohort of events to find all admission information in
  # diagnosed_cases - the pre-found cohort of people with a condition with admission date for start of spell
  
  findAdmissionsMethod <- function(cohort, diagnosed_cases)  {

    ## Create a copy of diagnosed cohort for safety
    
    copy_cohort <- copy(cohort)
    copy_diagnosed_cases <- copy(diagnosed_cases)
    
    
    ## Create field in diagnosed cases to show if admission method of episode was non-elective or not. Take count for check later.
    
    copy_diagnosed_cases[, non_elective_admission := !(ADMIMETH %in% c(11, 12, 13))]
    elective_admissions_count <- copy_diagnosed_cases[non_elective_admission == FALSE, .N]
    
    
    ## Merge elective admissions with entire cohort, this will give us all episodes for the spell of the condition admission in the cohort. Remove non_elective admissions field
    
    all_episode_for_admission <- merge(copy_diagnosed_cases[non_elective_admission == FALSE], copy_cohort[, .(ENCRYPTED_HESID, ADMIDATE, ADMIMETH)],
                              all.x = TRUE, by = c("ENCRYPTED_HESID", "ADMIDATE"))[, non_elective_admission := NULL]
    
    
    ## Re-create field that indicates if any of the admission methods were non-elective using newly merged in admissions methods
      
    all_episode_for_admission[, non_elective_admission := !(ADMIMETH.y %in% c(11, 12, 13))]
    
    
    ## Sum this logic field, and any that are above 0 have at least one admission method that is non-elective
    
    all_episode_for_admission[, non_elective_admission := any(non_elective_admission), by = .(ENCRYPTED_HESID, ADMIDATE)]
    
    
    ## Use the episode key of the condition admission to change those that have a non-elective admission at any point
    
    non_elective_found <- unique(all_episode_for_admission[non_elective_admission == TRUE, c("EPIKEY", "condition")])
    non_elective_found_count <- length(non_elective_found[, EPIKEY])
    
    copy_diagnosed_cases[EPIKEY %in% non_elective_found[, EPIKEY], non_elective_admission := TRUE]
    
    
    ## Check to see if numbers that have changed elective status is correct
    
    stopifnot((elective_admissions_count - non_elective_found_count) == copy_diagnosed_cases[non_elective_admission == FALSE, .N])
    
    return(copy_diagnosed_cases)
    
  }
 
  
  
## Function that finds the next repair (temp) event for each id, with arguments of;
  # risk_categorisation_table_ref - table that contains earliest perm, mod, and previous repairable event
  # repair_events - table of all possible repair events
  # repairable_event_index - index of repairable event to find, e.g 1 for first repairable event to find a match for

  findSubsequentRepair <- function(risk_categorisation_table_ref, repair_events, repairable_event_index) {
    
    ## Make a copy and change names for clarity
    
    risk_categorisation_table <- copy(risk_categorisation_table_ref)
    setnames(risk_categorisation_table, paste0("ADMIDATE_repairable", repairable_event_index), "..ADMIDATE_repairable_index_event..")
    
    
    ## Merge repairable records only, with all repair events
    
    subsequent_repairs <- merge(risk_categorisation_table[!is.na(..ADMIDATE_repairable_index_event..), .(ENCRYPTED_HESID, ..ADMIDATE_repairable_index_event..)], 
                                repair_events, 
                                all = FALSE, by = "ENCRYPTED_HESID")[ADMIDATE_temp >= ..ADMIDATE_repairable_index_event..]
    
    
    ## Check if there was anything returned to avoid errors, order and take the first repair for each person, and return this table of repair events
    
    if(subsequent_repairs[, .N] != 0){
      setorder(subsequent_repairs, ADMIDATE_temp)
      subsequent_repairs[, order := 1:.N, by = ENCRYPTED_HESID]
      first_subsequent_repair <- subsequent_repairs[order == 1, .(ENCRYPTED_HESID, ADMIDATE_temp, EPIORDER_temp, outcome_numb_temp, risk_name_temp,
                                                                  diag_code_temp, temp_high_risk_temp)]
    }
    else{
      first_subsequent_repair <- subsequent_repairs[, .(ENCRYPTED_HESID, ADMIDATE_temp, EPIORDER_temp, outcome_numb_temp, risk_name_temp,
                                                        diag_code_temp, temp_high_risk_temp)] 
    }
    
    return(first_subsequent_repair)
  }
  
  
  
## Function that finds the next repairable event for each id, takes as arguments;
  # latest_repair_ref - most recent repair event for each id
  # repairable_events - all repairable events
  # repairable_event_index - index of repairable event to find, e.g 1 for first repairable event
  
  findNextSubsequentRepairable <- function(latest_repair_ref, repairable_events, repairable_event_index){
    
    latest_repair_ref <- subsequent_repair
    repairable_events <- repairable_risk_events_collapsed
    repairable_event_index <- index
    
    
    ## Make a copy and change names for clarity
    
    latest_repair <- copy(latest_repair_ref)
    setnames(latest_repair, paste0("ADMIDATE_repair", repairable_event_index - 1), "..ADMIDATE_repair_index_event..")
    
    
    ## Remove all fields that we aren't interested in, and merge the latest repair event with all repairable events
    ## Retain only those that have a repairable event after the last repair event
    
    latest_repair <- latest_repair[, .(ENCRYPTED_HESID, ..ADMIDATE_repair_index_event..)]
    subsequent_repairables_post_repair <- merge(latest_repair, 
                                                repairable_events, 
                                                all = FALSE, by = "ENCRYPTED_HESID")[..ADMIDATE_repair_index_event.. < ADMIDATE]
    
    
    ## Find the next repairable event that has occurred after most recent repair
    
    first_repairable_post_repair <- copy(subsequent_repairables_post_repair[, .SD[order == min(order)]
                                                                            , by = ENCRYPTED_HESID])
    
    ## Check to see that only have one record per id
    
    stopifnot(first_repairable_post_repair[, .N, by = ENCRYPTED_HESID][N > 1, .N] == 0)
    
    
    ## Remove unwanted repair date, change col names, and return table of the repairable events
    
    first_repairable_post_repair[, c("..ADMIDATE_repair_index_event..", "order") := NULL]
    
    setnames(first_repairable_post_repair, c("ADMIDATE", "EPIORDER", "outcome_numb", "repairable_high_risk", "risk_name", "diag_code"),
             paste0(c("ADMIDATE_repairable", "EPIORDER_repairable", "outcome_numb_repairable", "repairable_high_risk_repairable", "risk_name_repairable", "diag_code_repairable"), repairable_event_index))
    
    return(first_repairable_post_repair)
  }
  
  
  
## Function that assigns a risk category to an event, takes as arguments;
  # risk_table - table of individuals with dates at which they change risk category
  # cohort - the cohort to apply the risk categories to
  # index_date_field - the name of the date field which we want to evaluate risk category for
  
  
  assignRiskCategory <- function(risk_table, cohort, index_field_date){

    ## Make a copy of the cohort feed in so can make changes and not affect original object
        
    cohort_copy <- copy(cohort)
    
    
    ## change name of ADMIDATE for index field, and merge with risk table
    
    setnames(cohort_copy, old = index_field_date, new = "..INDEX_DATE..")
    cohort_with_risk <- merge(cohort_copy, risk_table, all.x = TRUE, by = "ENCRYPTED_HESID")
    
    
    stopifnot(risk_table[, .N, by = ENCRYPTED_HESID][N > 1, .N] == 0 )
    
    ## Repairable1 and repair1 checked against Index case date, with check to ensure that dates are in chronological order
    
    cohort_with_risk[..INDEX_DATE.. > ADMIDATE_repairable1, ':=' (risk_at_index = "High", risk_category = risk_name_repairable1, risk_code = diag_code_repairable1)]
    
    
    cohort_with_risk[(..INDEX_DATE.. > ADMIDATE_repair1) & (..INDEX_DATE.. <= ADMIDATE_repair1 + 180), ':=' (risk_at_index = "High", risk_category = risk_name_repair1, risk_code = diag_code_repair1)]
    cohort_with_risk[(..INDEX_DATE.. > ADMIDATE_repair1) & (..INDEX_DATE.. > ADMIDATE_repair1 + 180), ':=' (risk_at_index = "Low", risk_category = NA, risk_code = NA)]
    
    
    ## Repairable2 and repair2 checked against Index case date, with check to ensure that dates are in chronological order
    
    
    cohort_with_risk[..INDEX_DATE.. > ADMIDATE_repairable2, ':=' (risk_at_index = "High", risk_category = risk_name_repairable2, risk_code = diag_code_repairable2)]
    
    
    cohort_with_risk[(..INDEX_DATE.. > ADMIDATE_repair2) & (..INDEX_DATE.. <= ADMIDATE_repair2 + 180), ':=' (risk_at_index = "High", risk_category = risk_name_repair2, risk_code = diag_code_repair2)]
    cohort_with_risk[(..INDEX_DATE.. > ADMIDATE_repair2) & (..INDEX_DATE.. > ADMIDATE_repair2 + 180), ':=' (risk_at_index = "Low", risk_category = NA, risk_code = NA)]
    
    
    ## Repairable3 checked against Index case date with check to ensure that dates are in chronological order
    
    
    cohort_with_risk[..INDEX_DATE.. > ADMIDATE_repairable3, ':=' (risk_at_index = "High", risk_category = risk_name_repairable3, risk_code = diag_code_repairable3)]
    
    
    ## Temp1 and temp2 checked against Index case date
    
    cohort_with_risk[(..INDEX_DATE.. > ADMIDATE_temp1) & (..INDEX_DATE.. <= ADMIDATE_temp1 + 180) & (is.na(risk_at_index) | risk_at_index != "High"),
                            ':=' (risk_at_index = "High", risk_category = risk_name_temp1, risk_code = diag_code_temp1)]
    
    cohort_with_risk[(..INDEX_DATE.. > ADMIDATE_temp2) & (..INDEX_DATE.. <= ADMIDATE_temp2 + 180) & (is.na(risk_at_index) | risk_at_index != "High"),
                            ':=' (risk_at_index = "High", risk_category = risk_name_temp2, risk_code = diag_code_temp1)]
    
    
    ## Moderate checked against Index case, only applied if person is not already high - there is no way to return to low risk once you are moderate risk.
    
    cohort_with_risk[(..INDEX_DATE.. > ADMIDATE_mod) & (is.na(risk_at_index) | risk_at_index != "High"),
                            ':=' (risk_at_index = "Moderate", risk_category = risk_name_mod, risk_code = diag_code_mod)]
    
    
    ## Perm checked against Index case, only applied if person is not already high, check that perm is always the latest date for individuals
    
    cohort_with_risk[(..INDEX_DATE.. > ADMIDATE_perm) & (is.na(risk_at_index) | risk_at_index != "High"),
                            ':=' (risk_at_index = "High", risk_category = risk_name_perm, risk_code = diag_code_perm)]
    
    
    ## Label all others people as Low/Unknown risk
    
    cohort_with_risk[is.na(risk_at_index), risk_at_index := "Low"]
    
    setnames(cohort_with_risk, old = "..INDEX_DATE..", new = index_field_date)
    
    cols_to_return <- c(colnames(cohort), "risk_at_index", "risk_category", "risk_code") 
    
    return(cohort_with_risk[, ..cols_to_return])
    
  }



## Function for assigning which causal organisms are present in each admission
  # cohort - the full cohort of events to find the discharge information in
  # diagnosed_cases - the pre-found cohort of people with a condition with admission date for start of spell
  
  assignCausalOrganism <- function(cohort, diagnosed_cases)  {
    
    ## Create copy of diagnosed cohort so can make changes

    copy_cohort <- copy(cohort)    
    diagnosed_cases_copy <- copy(diagnosed_cases)

    ## Create list of diagnosis cols names
    
    code_fields <- paste0("DIAG_", formatC(1:20, width = 2, format = "d", flag = "0"))
    cols_wanted <- c("ENCRYPTED_HESID", "ADMIDATE", code_fields)
    
    
    ## Get all episodes and diagnosis codes for each admission in diagnosed cohort
    
    condition_spells <- merge(diagnosed_cases_copy, copy_cohort[, ..cols_wanted],
                              all.x = TRUE, by = c("ENCRYPTED_HESID", "ADMIDATE"))
    
  
    ##  Read in causal organism sheet from spreadsheet of all codes
    
    organism_code_table <- read_excel("reference_data/ICD OPCS Codes.xlsx",
                                      sheet = "Causal Organism Codes",
                                      col_names = TRUE,
                                      col_types = "text",
                                      trim_ws = TRUE)
    
    ## Change into a data table to prepare for merge later and remove unwanted col
    
    organism_code_table <- setDT(organism_code_table)
    organism_code_table[, ':=' (Description = NULL, 
                                Causal_Organism = tolower(Causal_Organism))]
    
    ## Remove decimal point from code list
    
    organism_code_table <- organism_code_table[, ICD10_Hospital_Codes := gsub("[.]", "", ICD10_Hospital_Codes)]
    
    
    ## Melt cohort A so all diagnostic codes are in one col
    
    melted_condition_spells <- meltAndTrim(condition_spells, "ICD")
    
    
    ## Remove fields we are not interested in #####
    
    cols_to_remove <- colnames(melted_condition_spells)[!(colnames(melted_condition_spells) %in% c("ENCRYPTED_HESID", "ADMIDATE", "diag_code"))]
    
    melted_condition_spells[, (cols_to_remove) := NULL]
    
    ## Merge two table based on diag_code, with addition of causal organism, intersect join as only interested if there is a match of code.
    
    melted_spells_with_organism <- merge(melted_condition_spells, organism_code_table, by.x = "diag_code", by.y = "ICD10_Hospital_Codes", all = FALSE)
    
    melted_spells_with_organism[, ':=' (diag_code = NULL,
                                        present = 1L)]
    
    
    ## Remove full duplicates as created due to a melt
    
    melted_spells_with_organism <- unique(melted_spells_with_organism)
    
    
    ## Cast back to get all causal organisms on one line ready to merge back into case admissions (can change length() function to get logical result?)
    
    spells_with_organism <- dcast(melted_spells_with_organism, ENCRYPTED_HESID + ADMIDATE ~ Causal_Organism, value.var = "present",
                                  fill = 0L)
    
    
    ## Merge back into the case admission dataset
    
    IE_admissions_causal_organism <- merge(diagnosed_cases_copy, spells_with_organism, all.x = TRUE, by = c("ENCRYPTED_HESID", "ADMIDATE"))
    
    
    ## List of all causal orgs
    
    causal_organism_cols <- organism_code_table[, unique(Causal_Organism)]

    
    ## Replace all NAs with FALSE and take a count
    
    IE_admissions_causal_organism[, (causal_organism_cols) := lapply(.SD, function(x) replace(x, is.na(x), 0L)), .SDcols = causal_organism_cols]
    
    
    ## Create field for if any causal organism is present
    
    IE_admissions_causal_organism[, any_causal_organism := (Reduce(`+`, .SD) > 0), .SDcol = causal_organism_cols]
    
    
    ## Convert causal org cols back to logic
    
    IE_admissions_causal_organism[, (causal_organism_cols) := lapply(.SD, as.logical), .SDcols = causal_organism_cols]
    
    
    ## Check have not affected counts in replacing NAs and that all records have been retained
    
    stopifnot(diagnosed_cases[, .N] == IE_admissions_causal_organism[, .N])
    
    return(IE_admissions_causal_organism)
 
  }
  