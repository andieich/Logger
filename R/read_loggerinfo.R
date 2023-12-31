#' Read logger metadata
#'
#' This function is a wrapper for `readxl::read_excel` and imports the Excel table containing the logger metadata.
#'
#' @param path The path to the Excel file (`.xlsx`) containing the metadata for the logger deployments
#' @param sheet The tab in the Excel file containing the metadata for the logger deployments. If not specified, the first sheet is used.
#'
#' @return a `data.frame` with the logger metadata
#' @export
#'
#' @examples
#' \dontrun{
#' loggerinfo <- read_loggerinfo(here::here("data/logger_info.xlsx"))
#' }
#'
read_loggerinfo <- function(path, sheet = NA){

  if (!is.na(sheet)){
    loggerinfo <- readxl::read_excel(path,
                                     sheet = sheet)
  } else {
    loggerinfo <- readxl::read_excel(path)
  }


  # some checks:

  # 1) do basic columns exist?

  col_names <- names(loggerinfo)

  needed_cols <- c("filename", "deployed", "retrieved")

  if (any(!needed_cols %in% col_names)) {

    missing_cols <-  needed_cols[which(!needed_cols %in% col_names)]

    rlang::abort(c(
      "One or more nedded columns missing in loggerinfo.",
      "x" = paste("The column", missing_cols, "is missing in the loggerinfo file."),
      "i" = paste("Make sure that", missing_cols, "exists in loggerinfo.")
    ))

  }

  # 2) Any NAs in basic columns?

  Logger::check_NA_cols(loggerinfo, "retrieved")
  Logger::check_NA_cols(loggerinfo, "deployed")
  Logger::check_NA_cols(loggerinfo, "filename")

  # 3) Datetimes formatted correctly?

  Logger::check_datetime_cols(loggerinfo, "retrieved")
  Logger::check_datetime_cols(loggerinfo, "deployed")

  # 4) Filenames formatted correctly?

  # 4.1) csv as extension?

  extensions <- substr(loggerinfo$filename,
                       nchar(loggerinfo$filename) - 3,
                       nchar(loggerinfo$filename))

  if(any(extensions != ".csv")) {
    rows_fn_no_csv <- which(extensions != ".csv")

    rlang::abort(c(
      "There is a problem in the filename column",
      "x" = paste("The filname in line", rows_fn_no_csv, "does not end with `.csv`"),
      "i" = paste("Please use complete filenames including the file extension `.csv`.")
    ))

  }

  # 4.2) any "/" in filename?

  if(any(grepl("/", loggerinfo$filename))) {
    rows_fn_slash <- which(grepl("/", loggerinfo$filename))

    rlang::abort(c(
      "There is a problem in the filename column",
      "x" = paste("The filname in line", rows_fn_slash, "contains a `/`, which is not allowed"),
      "i" = paste("Please remove all `/` from the filename column")
    ))

  }

  return(loggerinfo)

}


#
#' Checks for datetime columns
#'
#' Helper function to check if a `column` of a `data`frame is formatted as POSIXct
#'
#' @param data The dataframe in which the datetime columns will be checked
#' @param column The name of the datetime column
#'
#' @return Returns a `rlang::abort()` message in case the datetime values are not correctly formatted
#' @export
#'
#' @examples
#' \dontrun{
#' check_datetime_cols(loggerinfo, "retrieved")
#' }
#'

check_datetime_cols <- function(data, column){

  values <- data[[column]]

  if (any(!lubridate::is.POSIXct(values))){

    row_is_no_datetime <- which(!lubridate::is.POSIXct(values))

    rlang::abort(c(
      "date_time values not formatted correctly",
      "x" = paste("In row", row_is_no_datetime, "of column", column, "is not in datetime format"),
      "i" = paste("Check if that", column, "is formatted correctly in Excel before import.")
    ))
  }
}



#' Check NA in columns
#'
#' Helper function to check if a `column` of a `data`frame contains NA values
#'
#' @param data The dataframe in which for specified columns will be checked if NA values occur
#' @param column The column that should be checked
#'
#' @return Returns a `rlang::abort()` message in case NA values occur in the selected column
#' @export
#'
#' @examples
#' \dontrun{
#' check_NA_cols(loggerinfo, "retrieved")
#' }
#'

check_NA_cols <- function(data, column){

  values <- data[[column]]

  if (any(is.na(values))){

    row_is_na <- which(is.na(values))

    rlang::abort(c(
      "NA values occur in loggerinfo",
      "x" = paste("In row", row_is_na, "of column", column, "an NA value was found"),
      "i" = paste("Make sure that the loggerinfo table is filled out completly for", column)
    ))
  }
}
