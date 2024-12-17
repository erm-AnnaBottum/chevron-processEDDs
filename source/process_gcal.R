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
fol_data <- readr::read_file("gcal_data_path.txt")

files_list <- list.files(fol_data, recursive = TRUE, full.names = TRUE, pattern = "*[[:digit:]].xls*")

df_full <- lapply(files_list, function(x){
  df_in <- read_excel(x, sheet = "GCAL STD")
}) %>%
  bind_rows()

df_edd_prep <- df_full %>%
  rename(
    Lab_Sample_ID = Lab_ID,
    Lab_Anl_Method_Name = Analytical_Method,
    Chemical_Name = Parameter,
    Lab_Qualifiers = Laboratory_Qualifiers,
    Result_Value = Result,
    Result_Unit = Unit,
    Reporting_Detection_Limit = Detection_Limit,
    dilution_factor = Dilution_Factor,
    Sample_Date = Collection_Date,
    Sample_Time = Extraction_Time,
    Lab_Name_Code = Lab_Name,
    Cas_Rn = CAS
  ) %>%
  mutate(
    Sample_Matrix_Code = case_when(
      Matrix == "SOLID" ~ "SO"
    ),
    Sys_Loc_Code = str_replace(Sample_ID, " .*", ""),
    fmt_date = format(Sample_Date, "%m%d%Y"),
    depth_range = str_replace_all(Sample_ID, ".* |\\(|\\)", ""),
    start_depth = str_replace(depth_range, "-.*", ""),
    end_depth = str_replace(depth_range, ".*-", ""),
    depth_unit = case_when(
      !is.na(depth_range) ~ "ft"
    ),
    Sys_Sample_Code = paste0(Sys_Loc_Code, "-", Sample_Matrix_Code, "-", depth_range, "-", fmt_date, "-ERM"),
    quantitation_limit = Reporting_Detection_Limit,
    detection_limit_unit = Result_Unit,
    Test_Type = "Initial",
    Fraction = "N",
    Sample_Source = "FIELD",
    column_number = "NA",
    reportable_result = "Y",
    validated_yn = "N",
    lab_matrix_code = Sample_Matrix_Code
  )

df_edd <- df_edd_prep %>%
  add_edd_columns(path_edd, "ESBasic_ERM") %>%
  rename(`#Sys_Sample_Code` = Sys_Sample_Code)

out <- list("ESBasic_ERM" = df_edd)
write.xlsx(out, "GCAL2018_data.CHEVRON-GENTILLY.ESBasic_ERM.xlsx")
  