# process data from Eberline Laboratory for Chevron Projects

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
fol_data <- readr::read_file("eberline_data_path.txt")

files_list <- list.files(fol_data, recursive = TRUE, full.names = TRUE, pattern = "(?i)*.xls*")

df_full <- lapply(files_list, function(x){
  df_in <- read_excel(x, col_types = "text")
  
  names(df_in) <- df_in[5,]
  names(df_in) <- str_replace_all(names(df_in), "\r\n|\n| ", "_")
  sample_delivery_group <- df_in[[1,10]]
  sample_matrix <- df_in[[4,10]]
  
  df_out <- df_in %>%
    mutate(
      sample_delivery_group = sample_delivery_group,
      Sample_Matrix_Code = sample_matrix
    ) %>%
    tail(-5)
}) %>%
  bind_rows()

if(FALSE){
  write.xlsx(df_full, "Eberline_intermediary_setps.xlsx")
  df_cleaned <- read_excel(file.path(fol_main, "Eberline_intermediary_setps.xlsx"))
}

df_edd_prep <- df_cleaned %>%
  filter(
    !is.na(Lab_ID)
  ) %>%
  rename(
    Lab_Sample_ID = Lab_ID,
    Chemical_Name = Analyte,
    Lab_Anl_Method_Name = Method,
    sampleDateTime = Sample_Date,
    analysisDateTIme = Analysis_Date,
    count_uncertainty = CU,
    total_uncertainty = CSU,
    minimum_detectable_conc = MDA,
    Result_Unit = Report_Units,
    Result_Value = Result,
    Test_Batch_ID = Batch_ID
  ) %>%
  mutate(
    Sample_Time = format(janitor::excel_numeric_to_date(as.numeric(sampleDateTime), include_time = TRUE), "%H:%M:%S"),
    Analysis_Time = format(janitor::excel_numeric_to_date(as.numeric(analysisDateTIme), include_time = TRUE), "%H:%M:%S"),
    Sample_Date = format(janitor::excel_numeric_to_date(as.numeric(sampleDateTime), include_time = TRUE), "%m/%d/%Y"),
    Analysis_Date = format(janitor::excel_numeric_to_date(as.numeric(analysisDateTIme), include_time = TRUE), "%m/%d/%Y"),
    fmt_date = format(janitor::excel_numeric_to_date(as.numeric(sampleDateTime), include_time = TRUE), "%m%d%Y"),
    depth_range = str_replace_all(Client_ID, ".* ", ""),
    depth_unit = case_when(
      !is.na(start_depth) ~ "ft"
    ),
    Sample_Type_Code = case_when(
      Sample_Type == "TRG" ~ "N",
      Sample_Type == "DUP" ~ "FD",
      Sample_Type == "MBL" ~ "LB",
      Sample_Type == "DO" ~ "N",
      TRUE ~ Sample_Type
    ),
    Reporting_Detection_Limit = minimum_detectable_conc,
    method_detection_limit = minimum_detectable_conc,
    quantitation_limit = minimum_detectable_conc,
    detection_limit_unit = Result_Unit,
    Sample_Matrix_Code = case_when(
      str_detect(Sample_Type_Code, "LCS|MB") ~ "SQ",
      TRUE ~ Sample_Matrix_Code
    ),
    Lab_Name_Code = "Eberline",
    Test_Type = "Initial",
    Basis = "Wet",
    Detect_Flag = "Y",
    Fraction = "N",
    Sample_Source = case_when(
      str_detect(Sample_Matrix_Code, "LCS|MB|LB") ~ "LAB",
      TRUE ~ "FIELD"
    ),
    column_number = "NA",
    result_type_code = case_when(
      Sys_Loc_Code == "SPIKE" ~ "SC",
      TRUE ~ "TRG"
    ),
    reportable_result = "Y",
    validated_yn = "N",
    lab_matrix_code = Sample_Matrix_Code,
    Sys_Sample_Code = case_when(
      !is.na(start_depth) & Sample_Type_Code == "FD" ~ str_replace_all(paste(Sys_Loc_Code, "-", Sample_Matrix_Code, "-", start_depth, "-", end_depth, "-", fmt_date, "-ERM-FD"), " ", ""),
      !is.na(start_depth) & Sample_Type_Code != "FD" ~ str_replace_all(paste(Sys_Loc_Code, "-", Sample_Matrix_Code, "-", start_depth, "-", end_depth, "-", fmt_date, "-ERM"), " ", ""),
      is.na(start_depth) & Sample_Type_Code == "FD" ~ str_replace_all(paste(Sys_Loc_Code, "-", Sample_Matrix_Code, "-",fmt_date, "-ERM-FD"), " ", ""),
      is.na(start_depth) & Sample_Type_Code != "FD" ~ str_replace_all(paste(Sys_Loc_Code, "-", Sample_Matrix_Code, "-",fmt_date, "-ERM"), " ", "")
    ),
    Sys_Loc_Code = case_when(
      str_detect(Sample_Matrix_Code, "LCS|MB|LB") ~ NA_character_,
      TRUE ~ Sys_Loc_Code
    )
  )

df_edd <- df_edd_prep %>%
  add_edd_columns(path_edd, "ESBasic_ERM") %>%
  rename(`#Sys_Sample_Code` = Sys_Sample_Code)

out <- list("ESBasic_ERM" = df_edd)
write.xlsx(out, "Eberline2018_data.CHEVRON-GENTILLY.ESBasic_ERM.xlsx")


