# process data from Element Laboratory for Chevron Projects

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
source("function.R")

fol_main <- here::here()
path_edd <- file.path(fol_main, "ESBasic_ERM_Desc.xlsx")
fol_data <- readr::read_file("element_data_path.txt")

files_list <- list.files(fol_data, full.names = TRUE)

df_full <- lapply(files_list, function(x){
  df_in <- read_excel(x)
}) %>%
  bind_rows()

df_edd_prep <- df_full %>%
  rename(
    Chemical_Name = Analyte,
    Lab_Anl_Method_Name = TestNo,
    Lab_Sample_ID = SampID,
    method_detection_limit = R_MDL,
    quantitation_limit = R_PQL,
    Lab_Qualifiers = R_Qual
  ) %>%
  mutate(
    Sys_Loc_Code = str_replace(ClientSampID, " .*", ""),
    Sample_Date = format(as.Date(DateCollected, "%Y-%m-%d %H:%M:%S"), "%m/%d/%Y"),
    Sample_Time = str_replace(DateCollected, ".* ", ""),
    Analysis_Date = Sample_Date, #since we don't have an analysis date given
    Analysis_Time = "00:00:00",
    fmt_date = format(as.Date(DateCollected, "%Y-%m-%d %H:%M:%S"), "%Y%m%d"),
    Reporting_Detection_Limit = quantitation_limit,
    Result_Value = case_when(
      str_detect(R_Rslt, "<") ~ NA_character_,
      str_detect(R_Rslt, ",") ~ R_Rslt,
      TRUE ~ as.character(as.numeric(R_Rslt))
    ),
    depth_range = str_replace_all(str_extract(ClientSampID, " .*"), "\\(|\\)|'| ", ""),
    start_depth = str_replace(depth_range, "-.*", ""),
    end_depth = str_replace(depth_range, ".*-", ""),
    Sample_Matrix_Code = case_when(
      R_Matrix == "Sediment" ~ "SE", 
      R_Matrix == "Soil" ~ "SO"
    ),
    Sys_Sample_Code = case_when(
      !is.na(depth_range) ~ paste0(Sys_Loc_Code, "-", depth_range, "-", Sample_Matrix_Code, "-", fmt_date),
      TRUE ~ paste0(Sys_Loc_Code, "-", Sample_Matrix_Code, "-", fmt_date)
    ),
    Cas_Rn = case_when(
      Chemical_Name == "Arsenic" ~ "7440-38-2",
      Chemical_Name == "Percent Moisture" ~ "Moisture",
      Chemical_Name == "pH Measurement" ~ "12408-02-5",
      Chemical_Name == "True Total Barium" ~ "7440-39-3",
      TRUE ~ CAS
    ),
    Result_Unit = case_when(
      R_Units == "% dry wt" ~ "% dw",
      R_Units == "mg/Kg" ~ "mg/kg",
      R_Units == "mg/Kg-dry" ~ "mg/kg dw",
      R_Units == "S.U." ~ "SU",
      R_Units == "wt%" ~ "% wet wt",
      TRUE ~ R_Units
    ),
    Fraction = case_when(
      Chemical_Name == "True Total Barium" ~ "T-DRY",
      Lab_Anl_Method_Name == "LDNR 29-B" ~ "N-DRY",
      TRUE ~ "N-WET"
    ),
    Sample_Type_Code = "N",
    Lab_Name_Code = "ELELA",
    Test_Type = "INITIAL",
    Detect_Flag = case_when(
      is.na(Result_Value) ~ "N",
      TRUE ~ "Y"
    ),
    column_number = "NA",
    result_type_code = "TRG",
    reportable_result = "Y",
    validated_yn = "N",
    depth_unit = case_when(
      !is.na(start_depth) ~ "ft"
    ),
    sample_delivery_group = str_replace(Lab_Sample_ID, "-.*", ""),
    task_code = "HIST-ELEMENT",
    detection_limit_unit = Result_Unit
  )

percent_moisture <- df_edd_prep %>%
  filter(
    Chemical_Name == "Percent Moisture"
  ) %>%
  select(
    Sys_Sample_Code,
    Result_Value
  ) %>%
  rename(
    dilution_factor = Result_Value
  )

df_edd <- df_edd_prep %>%
  left_join(percent_moisture, by = c("Sys_Sample_Code")) %>%
  add_edd_columns(path_edd, "ESBasic_ERM") %>%
  rename(`#Sys_Sample_Code` = Sys_Sample_Code)
  
out <- list("ESBasic_ERM" = df_edd)
write.xlsx(out, "Element2018_data.CHEVRON-GENTILLY.ESBasic_ERM.xlsx")
