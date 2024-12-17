# process data from Origins Laboratory for Chevron-Colorado Project

if(!require(pacman)){ install.packages("pacman") } else { library(pacman) }

# Import ####
p_load(
  here, 
  dplyr, 
  readxl,
  tibble,
  purrr,
  stringr,
  tidyr,
  lubridate,
  openxlsx,
  janitor,
  data.table
)
options(scipen = 999)
source("source/function.R")

# user input ####
dataset_desc <- "2024-Origin_WG_Batch4" #this will be used to name EDD file
facility_name <- "CHEVRON-COLORADO"

# set up directories ####
fol_main <- here::here()
path_edd <- file.path(fol_main, "reference", "ESBasic_ERM_Desc.xlsx")
fol_data <- file.path(fol_main, "data")

# read in data ####
files_list <- list.files(fol_data, recursive = FALSE, full.names = TRUE, pattern = ".xls")

df_anl_full <- lapply(files_list, function(x){
  df_in <- read_excel(x, sheet = "SAMPDATA") %>%
    mutate(
      sample_delivery_group = str_extract(sub(".*\\/", "", x), "[^ ]+")
    )
}) %>%
  bind_rows()

df_qc_full <- lapply(files_list, function(x){
  df_in <- read_excel(x, sheet = "QCDATA") %>%
    mutate(
      sample_delivery_group = str_extract(sub(".*\\/", "", x), "[^ ]+")
    )
}) %>%
  bind_rows()

# prep regular samples ####
df_anl_prep <- df_anl_full %>%
  rename_origin_headers() %>%
  rename(
    Sys_Sample_Code = SAMPLENAME #QC data doesn't have this header so do outside of function
  ) %>%
  mutate(
    Sys_Loc_Code = case_when(
      str_detect(Sys_Sample_Code, "MW") ~ substr(Sys_Sample_Code, 1, nchar(Sys_Sample_Code)-9),
      TRUE ~ NA_character_
    ),
    Sample_Type_Code = case_when(
      str_detect(Sys_Sample_Code, "(?i)TB") ~ "TB",
      TRUE ~ "N"
    ),
    Sample_Matrix_Code = case_when(
      Sample_Type_Code == "N" | Sample_Type_Code == "FD" & MATRIX == "Water" ~ "WG",
      Sample_Type_Code == "TB" & MATRIX == "Water" ~ "WQ"
    ),
    Lab_Name_Code = case_when(
      LabName == "Origins Laboratory" ~ "ORIGINS"
    ),
    remediation_number = case_when(
      str_detect(Sys_Sample_Code, "MW") ~ str_extract(Sys_Sample_Code, "[^-]+"),
      TRUE ~ NA_character_
    ),
    Sample_Date = str_replace(SAMPDATE, " .*", ""),
    Sample_Time = str_replace(SAMPDATE, ".* ", ""),
    Analysis_Date = str_replace(ANADATE, " .*", ""),
    Analysis_Time = str_replace(ANADATE, ".* ", ""),
    Prep_Date = case_when(
      PREPNAME != "NO PREP" ~ str_replace(PREPDATE, " .*", "")
    ),
    Prep_Time = case_when(
      PREPNAME != "NO PREP" ~ str_replace(PREPDATE, ".* ", "")
    ),
    Lab_Prep_Method_Name = case_when(
      PREPNAME == "NO PREP" ~ NA_character_,
      TRUE ~ PREPNAME),
    Result_Value = case_when(
      Result == "ND" ~ NA_character_,
      str_detect(Result, "<") ~ NA_character_,
      TRUE ~ Result
    ),
    Detect_Flag = case_when(
      is.na(Result_Value) ~ "N",
      TRUE ~ "Y"
    ),
    Lab_Qualifiers = case_when(
      Detect_Flag == "N" ~ "U",
      TRUE ~ NA_character_
    ),
    Fraction = "N", #hardcoding this for now since no metals
    Test_Type = "INITIAL",
    Sample_Source = "FIELD",
    result_type_code = case_when(
      SURROGATE ~ "SUR", #these are boolean fields in EDDs
      TIC ~ "TIC",
      TRUE ~ "TRG"
    ),
    reportable_result = "Y",
    validated_yn = "N",
    task_code = case_when(
      str_detect(Sys_Sample_Code, "MW") ~ paste0(remediation_number, "-", sub(".*\\-", "", Sys_Sample_Code)),
      TRUE ~ NA_character_
    ),
    lab_matrix_code = case_when(
      Sample_Type_Code == "N" | Sample_Type_Code == "FD" & RPTMATRIX == "Water" ~ "WG",
      Sample_Type_Code == "TB" & RPTMATRIX == "Water" ~ "WQ"
    ),
    quantitation_limit = Reporting_Detection_Limit,
  )
####


# prep QC samples ####
df_qc_prep <- df_qc_full %>%
  rename_origin_headers() %>%
  mutate(
    Sys_Sample_Code = Lab_Sample_ID,
    Sample_Type_Code = case_when(
      QCTYPE == "Blank" ~ "LB",
      QCTYPE == "LCS" ~ "LCS",
      QCTYPE == "LCS Dup" ~ "BD1",
      QCTYPE == "Duplicate" ~ "LD",
      QCTYPE == "Matrix Spike" ~ "MS",
      QCTYPE == "Matrix Spike Dup" ~ "MSD",
      QCTYPE == "LCSD" ~ "LCSD",
      QCTYPE == "LRB" ~ "LR",
      QCTYPE == "MS" ~ "MS",
      QCTYPE == "MSD" ~ "MSD"
    ),
    Sample_Matrix_Code = case_when(
      MATRIX == "Water" | MATRIX == "AQU" ~ "WQ"
    ),
    Lab_Name_Code = case_when(
      LabName == "Origins Laboratory" ~ "ORIGINS"
    ),
    Analysis_Date = str_replace(ANADATE, " .*", ""),
    Analysis_Time = str_replace(ANADATE, ".* ", ""),
    Prep_Date = case_when(
      PREPNAME != "NO PREP" ~ str_replace(PREPDATE, " .*", "")
    ),
    Prep_Time = case_when(
      PREPNAME != "NO PREP" ~ str_replace(PREPDATE, ".* ", "")
    ),
    Lab_Prep_Method_Name = case_when(
      PREPNAME == "NO PREP" ~ NA_character_,
      TRUE ~ PREPNAME),
    Result_Value = case_when(
      RESULT == "ND" ~ NA_character_,
      str_detect(RESULT, "<") ~ NA_character_,
      TRUE ~ RESULT
    ),
    Detect_Flag = case_when(
      is.na(Result_Value) ~ "N",
      TRUE ~ "Y"
    ),
    Lab_Qualifiers = case_when(
      Detect_Flag == "N" ~ "U",
      TRUE ~ NA_character_
    ),
    Fraction = "N", #hardcoding this for now since no metals
    Test_Type = "INITIAL",
    Sample_Source = "LAB",
    result_type_code = case_when(
      SURROGATE ~ "SUR", #these are boolean fields in EDDs
      TIC ~ "TIC",
      TRUE ~ "TRG"
    ),
    reportable_result = "Y",
    validated_yn = "N",
    lab_matrix_code = case_when(
      MATRIX == "Water" | MATRIX == "AQU" ~ "WQ"
    ),
    quantitation_limit = Reporting_Detection_Limit,
    Parent_Sample_Code = NA_character_
  )
####

# get list of ids to assign QC sample parents ####
parent_ids <- df_anl_prep %>%
  select(Sys_Sample_Code, Lab_Sample_ID) %>%
  distinct()

parent_id_list <- list(parent_ids$Sys_Sample_Code) %>%
  unlist() %>%
  setNames(parent_ids$Lab_Sample_ID)

df_qc_prep$Parent_Sample_Code <- parent_id_list[df_qc_prep$SOURCEID]

#handle QC samples that don't have parents
df_qc_prep_stu <- df_qc_prep %>%
  mutate(
    Sample_Type_Code = case_when(
      QCTYPE == "Duplicate" & is.na(Parent_Sample_Code) ~ "LD",
      QCTYPE == "LCS Dup" | QCTYPE == "LCSD" & is.na(Parent_Sample_Code) ~ "BD1",
      QCTYPE == "LRB" & is.na(Parent_Sample_Code) ~ "LR1",
      QCTYPE == "Matrix Spike" | QCTYPE == "MS" & is.na(Parent_Sample_Code) ~ "MS1",
      QCTYPE == "Matrix Spike Dup" | QCTYPE == "MSD" & is.na(Parent_Sample_Code) ~ "SD1",
      TRUE ~ Sample_Type_Code
    )
  )

# combine regular and lab QC data, send to EDD ####
df_edd <- bind_rows(df_anl_prep, df_qc_prep_stu) %>%
  add_edd_columns(path_edd, "ESBasic_ERM") %>%
  rename(`#Sys_Sample_Code` = Sys_Sample_Code)

# handle some remapping ####
df_cas_remaps <- read_excel(file.path(fol_main, "reference", "origins_remap_guide.xlsx"),
                            sheet = "analyte")
df_ameth_remaps <- read_excel(file.path(fol_main, "reference", "origins_remap_guide.xlsx"),
                              sheet = "anl_method")
df_pmeth_remaps <- read_excel(file.path(fol_main, "reference", "origins_remap_guide.xlsx"),
                              sheet = "prep_method")

cas_maps <- list(df_cas_remaps$equis_cas_rn) %>%
  unlist() %>%
  setNames(df_cas_remaps$Chemical_Name)

anl_meth_maps <- list(df_ameth_remaps$equis_anl_method) %>%
  unlist() %>%
  setNames(df_ameth_remaps$Lab_Anl_Method_Name)

prep_meth_maps <- list(df_pmeth_remaps$equis_prep_method) %>%
  unlist() %>%
  setNames(df_pmeth_remaps$Lab_Prep_Method_Name)

df_edd$Cas_Rn <- cas_maps[df_edd$Chemical_Name]
df_edd$Lab_Anl_Method_Name <- anl_meth_maps[df_edd$Lab_Anl_Method_Name]
df_edd$Lab_Prep_Method_Name <- prep_meth_maps[df_edd$Lab_Prep_Method_Name]

# grab some additional data for remapping/QC ####
if (TRUE) {
  analyte <- df_edd %>%
    select(
      Chemical_Name,
      Cas_Rn
    ) %>%
    distinct()
  
  anl_method <- df_edd %>%
    select(
      Lab_Anl_Method_Name
    ) %>%
    distinct()
  
  prep_method <- df_edd %>%
    select(
      Lab_Prep_Method_Name
    ) %>%
    distinct() %>%
    filter(
      !is.na(Lab_Prep_Method_Name)
    )
  
  unit <- df_edd %>%
    select(
      Result_Unit
    ) %>%
    distinct()
  
  sample_name <- df_edd %>%
    select(
      `#Sys_Sample_Code`,
      Sys_Loc_Code,
      task_code
    ) %>%
    distinct()
  
  qc_list <- list("analyte" = analyte,
                  "anl_method" = anl_method,
                  "prep_method" = prep_method,
                  "unit" = unit,
                  "sample_name" = sample_name,
                  "parent_ids" = parent_ids)
  write.xlsx(qc_list, file.path(fol_main, "output", paste0("QC_reference_items_", dataset_desc, ".xlsx")))
}

out <- list("ESBasic_ERM" = df_edd)
write.xlsx(out, file.path(fol_main, "output", paste0(dataset_desc, ".", facility_name, ".ESBasic_ERM.xlsx")))
















