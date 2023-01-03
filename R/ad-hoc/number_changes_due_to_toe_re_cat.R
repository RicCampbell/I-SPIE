### Checking the number of 'new' invasive' procedures included with re-categorisation of U202

library(data.table)
library(readxl)
library(lubridate)
source("R/database functions.R")

## Read in data

cohort_A <- readInData("data-raw/src_data/COHORT_A_1999_2015_WITHOUT_PII.txt")


## Change ADMIDATE and DISDATE to an actual date for sorting

cohort_A[, ADMIDATE := as.Date(ADMIDATE, format = "%Y-%m-%d")]


## Read in risk codes from spreadsheet, and risk table from exported Rds object 

invasive_surgical_sheet <- data.table(read_excel("reference_data/ICD OPCS Codes.xlsx",
                                                 sheet = "Invasive Surgical Procedures",
                                                 col_names = TRUE,
                                                 col_types = "text",
                                                 trim_ws = TRUE))


invasive_surgical_sheet[, standard_opcs_code := prepareCodeList(opcs_code)]


## Melt the cohort, and identify those that have the correct diagnosis/procedure

melted_cohort <- meltAndTrim(cohort_A, "OPCS")


## Remove rows where 'No operation performed' "-", or 'Not known' "&"

melted_cohort <- melted_cohort[!(diag_code == "-" | diag_code == "&")]


## Check that all codes are of 4 characters and do not contain 3 character codes

stopifnot(min(nchar(melted_cohort$diag_code), na.rm = TRUE) == 4)
stopifnot(max(nchar(melted_cohort$diag_code), na.rm = TRUE) == 4)


## Merge with invasive surgical procedure to add information about procedure. Only want records with procedure that is of interest

melted_cohort_invasive_procedure <- merge(melted_cohort,
                                          invasive_surgical_sheet,
                                          by.x = "diag_code",
                                          by.y = "standard_opcs_code",
                                          all = FALSE)


## Want to treat groups separately, so select the first procedure (not procedure group) for each person (ENCRYPTED_HESID), for each admissions (ADMIDATE)

  setorder(melted_cohort_invasive_procedure, ENCRYPTED_HESID, ADMIDATE, EPISTART, EPIORDER, outcome_numb)

  melted_cohort_invasive_procedure[, order := 1:.N, by = .(ENCRYPTED_HESID, ADMIDATE, procedure)]


## Get all hes-id - admission date pairs that have U202 in position one - these will be in the invasive procedure data table
  
  melted_cohort_invasive_procedure[, flimsy_link_id := paste0(ENCRYPTED_HESID, "_", ADMIDATE)]

  included_toe_procedures <- melted_cohort_invasive_procedure[diag_code == "U202" & order == 1, flimsy_link_id]
  

## Look for procedures that are in the same EPIKEY as a TOE procedure, not in position one (can't be as TOE is there), and in the same procedure group
## These procedures would be included as extra procedures if TOE was re-categorised to a different procedure group
  
  #extra_procedures <- melted_cohort_invasive_procedure[order != 1 & procedure == "endoscopic_oesophageal" & EPIKEY %chin% included_toe_procedures]
  
  extra_procedures <- melted_cohort_invasive_procedure[outcome_numb != 21 & procedure == "other_endoscopic_oesophageal" & flimsy_link_id %chin% included_toe_procedures]
  
  extra_procedures[, .N, by = diag_code]
  
  