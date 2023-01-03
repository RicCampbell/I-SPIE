## Script for downloading reference data

library(data.table)

## Download OPCS data files from TRUD - v4.9, 2019-11-04

  opcs_trud_address <- "https://isd.digital.nhs.uk/trud3/api/v1/keys/4149673aafc1a67390597b8f681b4d6a3660968b/files/OPCS4/9.0.0/NHS_OPCS4/nhs_opcs4df_9.0.0_20191104000001.zip"
  
  temp <- tempfile(fileext = ".zip")
  download.file(opcs_trud_address, temp, mode = "wb")
  files <- unzip(temp, exdir = "D:/I-SPIEDM/I-SPIEDM/reference_data/temp/opcs")
  
  opcs_codes <- fread("D:/I-SPIEDM/I-SPIEDM/reference_data/temp/opcs/OPCS49 CodesAndTitles Nov 2019 V1.0.txt",
                      colClasses = "character",
                      header = FALSE)
  
  setnames(opcs_codes, c("codes", "titles"))
  
  write.csv(opcs_codes, "D:/I-SPIEDM/I-SPIEDM/reference_data/opcs_codes_titles_nov_2019_v1.0.csv")
  
  
  unlink(c(temp, file))
  
  
## Download ICD-10 data files from TRUD - 4th edition v1.0.0, 2011-08-10
  
  icd_10_trud_address <- "https://isd.digital.nhs.uk/trud3/api/v1/keys/4149673aafc1a67390597b8f681b4d6a3660968b/files/ICD/1.0.0/NHS_ICD10/nhs_icd10_1.0.0_20120401000001.zip"
  
  temp <- tempfile(fileext = ".zip")
  download.file(icd_10_trud_address, temp, mode = "wb")
  files <- unzip(temp, exdir = "D:/I-SPIEDM/I-SPIEDM/reference_data/temp/icd_10")
  
  icd_10_codes <- fread("D:/I-SPIEDM/I-SPIEDM/reference_data/temp/icd_10/ICD10_Edition4_20120401/Content/ICD10_Edition4_CodesAndTitlesAndMetadata_GB_20120401.txt",
                      colClasses = "character",
                      header = TRUE)
  
  write.csv(icd_10_codes, "D:/I-SPIEDM/I-SPIEDM/reference_data/ICD10_Edition4_CodesAndTitlesAndMetadata_GB_20120401.csv")
  
  
  unlink(c(temp, file))
  