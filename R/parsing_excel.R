#' Format Excel date serial to period ID
#'
#' @param excel_date Numeric Excel date serial
#' @return Character period ID in format "YYYYMM"
#' @keywords internal
format_period_from_date <- function(excel_date) {
  # Convert Excel serial to R Date (Excel's epoch is 1899-12-30)
  r_date <- as.Date(excel_date, origin = "1899-12-30")

  # Format as YYYYMM
  year <- format(r_date, "%Y")
  month <- format(r_date, "%m")

  paste0(year, "M", month)
}

#' Extract data from single BPM6 Excel sheet
#'
#' Reads one sheet from a BPM6 Excel file, extracting only the 12 monthly
#' data columns and parsing the GROUP_CODE into numeric code and description.
#'
#' @param file_path Character string. Path to Excel file.
#' @param sheet_name Character string. Name of sheet to read.
#'
#' @return Data frame with columns: code, description, period_id, value
#'
#' @keywords internal
extract_bpm6_sheet <- function(file_path, sheet_name) {
  # Read Excel, skip first 3 rows
  raw_data <- readxl::read_excel(
    file_path,
    sheet = sheet_name,
    skip = 3,
    col_names = TRUE
  )

  # Find the "SUM" column
  sum_col_idx <- which(names(raw_data) == "SUM")

  if (length(sum_col_idx) == 0) {
    stop("Could not find 'SUM' column in sheet ", sheet_name)
  }

  # Take only columns up to (but not including) SUM
  # This gives us the 2 metadata columns + however many months exist
  raw_data <- raw_data[, 1:(sum_col_idx[1] - 1)]

  # Get the date headers (everything except first 2 columns)
  date_headers <- names(raw_data)[3:ncol(raw_data)]

  # Convert Excel date serials to period IDs
  period_ids <- purrr::map_chr(date_headers, ~{
    date_val <- suppressWarnings(as.numeric(.x))
    if (is.na(date_val)) {
      stop("Expected numeric Excel date in column header, got: ", .x)
    }
    format_period_from_date(date_val)
  })

  # Rename columns
  names(raw_data) <- c("group_code_raw", "stat_type", period_ids)

  # Drop stat_type column
  raw_data <- raw_data[, -2]

  # Pivot to long format
  long_data <- raw_data |>
    tidyr::pivot_longer(
      cols = -group_code_raw,
      names_to = "period_id",
      values_to = "value"
    )

  # Parse group_code_raw into code and description
  long_data <- long_data |>
    dplyr::mutate(
      code = stringr::str_extract(group_code_raw, "^[^ ]+"),
      description = stringr::str_trim(stringr::str_replace(group_code_raw, "^[^ ]+ ", ""))
    ) |>
    dplyr::select(code, description, period_id, value)

  return(long_data)
}


#' Extract data from all sheets in a BPM6 Excel file
#'
#' Reads all sheets from a BPM6 Excel file and combines them into a single
#' dataframe. Auto-detects sheet names and processes each one.
#'
#'
#' @param file_path Character string. Path to Excel file.
#' @param sheet_names Character vector. Optional. Sheet names to process.
#'   If NULL (default), auto-detects all sheets in the file.
#'
#' @return Data frame with columns: code, description, period_id, value
#' @export
extract_all_bpm6_sheets <- function(file_path, sheet_names = NULL) {

  # Auto-detect sheet names if not provided
  if (is.null(sheet_names)) {
    sheet_names <- readxl::excel_sheets(file_path)
    message("Auto-detected sheets: ", paste(sheet_names, collapse = ", "))
  }

  # Extract data from each sheet
  all_data <- purrr::map_dfr(sheet_names, ~{
    message("Processing sheet: ", .x)
    extract_bpm6_sheet(file_path, .x)
  })

  # Check for duplicate period_ids (which would indicate overlapping data)
  duplicates <- all_data |>
    dplyr::count(code, period_id) |>
    dplyr::filter(n > 1)

  if (nrow(duplicates) > 0) {
    stop(
      "Found duplicate period_ids for ", nrow(duplicates), " code-period combinations. ",
      "Sheets appear to have overlapping data. ",
      "First few duplicates: ",
      paste(utils::head(duplicates$code, 3), collapse = ", ")
    )
  }

  return(all_data)
}
