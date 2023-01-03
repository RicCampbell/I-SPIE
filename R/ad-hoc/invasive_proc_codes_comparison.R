## Check invasive procedure codes

library(data.table)
library(readxl)
source("R/database functions.R")

## Read in the one I've created from the appendix table

  appendix_codes <- data.table(read_excel("reference_data/ICD OPCS Codes.xlsx",
                                          sheet = "Invasive Surgical Procedures",
                                          col_names = TRUE,
                                          col_types = "text",
                                          trim_ws = TRUE))
  
  
## Read in all the sheets from the large file Martin sent
  
  big_gi_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                        sheet = "1. GI",
                                        col_names = c("codes", "desc"),
                                        col_types = "text",
                                        trim_ws = TRUE))
  
  big_gu_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                        sheet = "2. GU ",
                                        col_names = c("codes", "desc"),
                                        col_types = "text",
                                        trim_ws = TRUE))
  
  big_obs_gynea_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                               sheet = "3. Obs & Gynae",
                                               col_names = c("codes", "desc"),
                                               col_types = "text",
                                               trim_ws = TRUE))
  
  big_resp_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                          sheet = "4. Resp",
                                          col_names = c("codes", "desc"),
                                          col_types = "text",
                                          trim_ws = TRUE))
  
  big_cardiac_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                             sheet = "5. Cardiac",
                                             col_names = c("codes", "desc"),
                                             col_types = "text",
                                             trim_ws = TRUE))
  
  big_ent_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                         sheet = "6. ENT",
                                         col_names = c("codes", "desc"),
                                         col_types = "text",
                                         trim_ws = TRUE))
  
  big_derm_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                          sheet = "7. Derm",
                                          col_names = c("codes", "desc"),
                                          col_types = "text",
                                          trim_ws = TRUE))
  
  big_haem_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                          sheet = "8. Haem",
                                          col_names = c("codes", "desc"),
                                          col_types = "text",
                                          trim_ws = TRUE))
  
  big_renal_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                           sheet = "9. Renal",
                                           col_names = c("codes", "desc"),
                                           col_types = "text",
                                           trim_ws = TRUE))
  
  big_dental_codes <- data.table(read_excel("reference_data/OPCS codes 2021-07-02.xlsx",
                                            sheet = "10. Dental",
                                            col_names = c("codes", "desc"),
                                            col_types = "text",
                                            trim_ws = TRUE))
  
  
## Remove any rows with empty desc - full empties and headings
  
  big_gi_codes <- big_gi_codes[!is.na(desc)]
  big_gu_codes <- big_gu_codes[!is.na(desc)]
  big_obs_gynea_codes <- big_obs_gynea_codes[!is.na(desc)]
  big_resp_codes <- big_resp_codes[!is.na(desc)]
  big_cardiac_codes <- big_cardiac_codes[!is.na(desc)]
  big_ent_codes <- big_ent_codes[!is.na(desc)]
  big_derm_codes <- big_derm_codes[!is.na(desc)]
  big_haem_codes <- big_haem_codes[!is.na(desc)]
  big_renal_codes <- big_renal_codes[!is.na(desc)]
  big_dental_codes <- big_dental_codes[!is.na(desc)]
  
## Make sure none have decimal point in
  
  big_gi_codes <- big_gi_codes[, prepped_code := prepareCodeList(codes)]
  big_gu_codes <- big_gu_codes[, prepped_code := prepareCodeList(codes)]
  big_obs_gynea_codes <- big_obs_gynea_codes[, prepped_code := prepareCodeList(codes)]
  big_resp_codes <- big_resp_codes[, prepped_code := prepareCodeList(codes)]
  big_cardiac_codes <- big_cardiac_codes[, prepped_code := prepareCodeList(codes)]
  big_ent_codes <- big_ent_codes[, prepped_code := prepareCodeList(codes)]
  big_derm_codes <- big_derm_codes[, prepped_code := prepareCodeList(codes)]
  big_haem_codes <- big_haem_codes[, prepped_code := prepareCodeList(codes)]
  big_renal_codes <- big_renal_codes[, prepped_code := prepareCodeList(codes)]
  big_dental_codes <- big_dental_codes[, prepped_code := prepareCodeList(codes)]
  
  appendix_codes[, standard_opcs_code := prepareCodeList(opcs_code)]
  
  
  big_file_codes <- rbind(big_gi_codes, big_gu_codes, big_obs_gynea_codes, big_resp_codes, big_cardiac_codes,
                          big_ent_codes, big_derm_codes, big_haem_codes, big_renal_codes, big_dental_codes)
  
  
## Appendix only
  
  appendix_only_codes <- setdiff(appendix_codes$standard_opcs_code, big_file_codes$prepped_code)
  
  appendix_only_codes <- appendix_codes[standard_opcs_code %chin% appendix_only_codes][nchar(standard_opcs_code) > 3]
  
  
## Big list only
  
  big_list_only <- setdiff(big_file_codes$prepped_code, appendix_codes$standard_opcs_code)
  
  big_list_only <- big_file_codes[prepped_code %chin% big_list_only]
  

## Write out output to copy into an email
  
  write.csv2(appendix_only_codes, file = "D:/I-SPIEDM/I-SPIEDM/data/datasets/ad-hoc/appendix_only_codes.csv")
  write.csv2(big_list_only, file = "D:/I-SPIEDM/I-SPIEDM/data/datasets/ad-hoc/big_list_only.csv")
  
  