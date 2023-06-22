#' Read and combine logger csv files
#'
#' Uses the info provided in `loggerinfo` to read and merge all logger csv files.
#'
#' @param loggerinfo The name of the `dataframe`containing the logger metadata
#' @param folder The path to the folder containing the logger `.csv` files. The default is `data/raw_csv`
#' @param check_SN logical: Should the serial number (SN) provided in the metadata dataframe be checked against the SN stored in the logger `.csv` files? The default is `TRUE`.
#'
#' @return A `dataframe` in "long" format containing all the logger data. Additional information, as SN, filename, logger type, etc, is added
#' @export
#'
#' @examples
#' \dontrun{
#' dat_logger <- read_loggerfiles(loggerinfo)
#' }
#'
read_loggerfiles <- function(loggerinfo,
                             folder = "data/raw_csv",
                             check_SN = TRUE){

  merged_data <- data.frame("date_time" = lubridate::as_datetime(NA),
                            "temperature" = NA,
                            "light_intensity" = NA,
                            "SN" = NA,
                            "type" = "NA",
                            "site" = "NA",
                            "habitat" = "NA",
                            "replicate" = NA)[0,]

  for (row_i in seq_along(rownames(loggerinfo))){
    data <- utils::read.csv(here::here(folder, loggerinfo$filename[row_i]))
    SN <- Logger::get_SN(data)
    names <- Logger::clean_colnames(data, row_i)

    names(data) <- names

    #check if SN in loggerinfo is the same as in csv

    if (check_SN & SN != loggerinfo$SN[row_i]){
      stop(paste0("Error in line ", row_i," of loggerinfo: The SN provided in loggerinfo (",
                  loggerinfo$SN[row_i], ") is different than the SN in the csv data (", SN ,")" ))
    }



    # filter data with infos provided in loggerinfo
    data <- data %>%
      mutate(date_time = lubridate::as_datetime(date_time)) %>%
      filter(date_time >= lubridate::as_datetime(loggerinfo$deployed[row_i])) %>%
      filter(date_time <= lubridate::as_datetime(loggerinfo$retrieved[row_i]))

    data$SN <- SN
    data$type <- loggerinfo$type[row_i]
    data$site <- loggerinfo$site[row_i]
    data$habitat <- loggerinfo$habitat[row_i]
    data$replicate <- loggerinfo$replicate[row_i]


    merged_data <- add_row(merged_data, data)

    print(paste("Successfully read line", row_i ,"of loggerinfo"))

  }
  return(merged_data)
}


#' Get serial number from logger csv files
#'
#' The serial number is stored in the column names if the data was exported correctly as csv.
#' This function extracts this serial number so it can be compared to the one provided in the logger metadata.
#'
#' @param data The current logger csv
#' @param sep The separating character between the parameter name and the serial number. Defaults to `.`
#'
#' @return The serial number.
#' @export
#' @export
#'
#' @examples
#' \dontrun{
#' SN <- get_SN(data)
#' }
#'
get_SN <- function(data, sep = "."){
  last_colname <- names(data)[ncol(data)]
  SN <- gsub(".*?([0-9]+).*", "\\1", last_colname) %>%
    as.numeric()

  return(SN)
}


#' Clean logger column names
#'
#' The function extracts the column names from the logger csv files and cleans them.
#' These names contain special characters, units, and the serial number and will be standardized to date_time, temperature, and light_intensity.
#'
#' @param data The current logger csv
#' @param rownumber The row number in the `loggerinfo` dataframe corresponding to `data`.
#'
#' @return a character vector of clean column names.
#' @export
#'
#' @examples
#' \dontrun{
#' names <- clean_colnames(data, row_i)
#' }
#'
clean_colnames <- function(data, rownumber){
  col_names <- names(data)

  cleaned_colnames <- c("NA")[0]

  for (col_name_i in col_names) {

    parameter <- gsub("([A-Za-z]+).*", "\\1", col_name_i) #get first "word"

    #actual cleaning

    if (parameter == "Date"){
      parameter <- "date_time"
    } else if (parameter == "Temp"){
      parameter <- "temperature"
    } else if (parameter == "Intensity"){
      parameter <- "light_intensity"
    } else {
      rlang::abort(c(
        paste("Error in line", rownumber,"of loggerinfo"),
        "x" = paste("The parameter", parameter, "is not a standard parameter (Date, Temp, or Intensity)"),
        "i" = "Check if the column names in the logger csv are actually exported in English"
      ))
    }

    cleaned_colnames <- c(cleaned_colnames, parameter)

  }

  return(cleaned_colnames)

}
