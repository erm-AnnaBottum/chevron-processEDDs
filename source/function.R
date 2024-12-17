get_edd_column_names <- function(edd_format_path, edd_ws) {
  vec_fields <- read_excel(edd_format_path, edd_ws) %>%
    pull(`Field Name`)
  
  n_fields <- min(which(is.na(vec_fields)) - 1, length(vec_fields))
  
  vec_fields[1:n_fields]
}

add_edd_columns <- function(df, edd_format_path, edd_ws) {
  edd_column_names <- get_edd_column_names(edd_format_path, edd_ws)
  
  current_column_names <- names(df)
  
  missing_column_names <- base::setdiff(
    edd_column_names, current_column_names
  ) %>%
    setNames(nm = .)
  
  missing_column_names[] <- NA
  
  tibble::add_column(df, !!!missing_column_names) %>%
    select(all_of(edd_column_names))
}

rename_origin_headers <- function(df) {
  df_out <- df %>%
    rename(
      Lab_Sample_ID = LABSAMPID,
      Basis = BASIS,
      Test_Batch_ID = BATCH,
      Lab_Anl_Method_Name = METHODNAME,
      Chemical_Name = ANALYTE,
      Cas_Rn = CASNUMBER,
      Reporting_Detection_Limit = RL,
      Result_Unit = UNITS,
      method_detection_limit = DL,
      dilution_factor = DILUTION
    )
}
