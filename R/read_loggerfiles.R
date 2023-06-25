#' Read and combine logger csv files
#'
#' Uses the info provided in `loggerinfo` to read and merge all logger csv files.
#'
#' @param loggerinfo The name of the `dataframe`containing the logger metadata
#' @param add_info a character vector of column names in `loggerinfo`. The content of these columns will be added to the corresponding logger data. As default, SN, type, site, and depth is chosen.
#' @param folder The path to the folder containing the logger `.csv` files. The default is `data/raw_csv`
#' @param check_SN logical: Should the serial number (SN) provided in the metadata dataframe be checked against the SN stored in the logger `.csv` files? The default is `TRUE`.
#'
#' @return A `dataframe` in "long" format containing all the logger data. Additional information, as SN, filename, logger type, etc, is added
#' @export
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' dat_logger <- read_loggerfiles(loggerinfo)
#' }
#'
read_loggerfiles <- function(loggerinfo,
                             add_info = c("SN",
                                          "type",
                                          "site",
                                          "depth"),
                             folder = "data/raw_csv",
                             check_SN = TRUE){

  # check if folder exists:

  if (!file.exists(folder)){
    rlang::abort(c(
      "The specified folder does not exist",
      "x" = paste("The folder ", folder, "was not found"),
      "i" = paste("Make sure that the specified folder exists or that you specify the folder if it is not the default (`data/raw_csv)")
    ))
  }

  # check if all files exist

  path_to_file <- paste(folder, loggerinfo$filename, sep = "/")

  if (any(!file.exists(path_to_file))){

    missing_files <- loggerinfo$filename[which(!file.exists(path_to_file))]
    row_missing_files <- which(!file.exists(path_to_file))

    rlang::abort(c(
      "A file from the filename column in loggerinfo does not exist",
      "x" = paste("The file ", missing_files, "does not exist"),
      "i" = paste("Make sure that all files from the filename column of loggerinfo exists in the folder with the csv files")
    ))
  }


  # if check_SN is TRUE, check if it's numeric and not NA

  if (check_SN) {

    #does the column exist?

    if (!"SN" %in% names(loggerinfo)){
      rlang::abort(c(
        "The column SN does not exist in loggerinfo",
        "i" = "Either add the SN column or set check_SN = FALSE"
      ))
    }

    # are there NA values?

    if (any(is.na(loggerinfo$SN))){

      row_with_NA <- which(is.na(loggerinfo$SN))

      rlang::abort(c(
        "NA values in SN column of loggerinfo",
        "x" = paste("In row", row_with_NA, "a NA value was detected"),
        "i" = "Make sure that all cells in the SN column are filled out"
      ))
    }

    # all SN integer?

    if (any(!is.numeric(loggerinfo$SN))){

      row_SN_no_integer <- which(!is.numeric(loggerinfo$SN))

      rlang::abort(c(
        "SN values contain not only numbers",
        "i" = "Make sure that all cells in the SN column are filled out correctly"
      ))
    }

  }


  # check if all add_info columns exist in loggerinfo

  if (any(!add_info %in% names(loggerinfo))) {

    missing_cols <-  add_info[which(!add_info %in% names(loggerinfo))]

    rlang::abort(c(
      "One or more nedded columns missing in loggerinfo.",
      "x" = paste(missing_cols, " was selected in `add_info` but is missing in the loggerinfo file."),
      "i" = paste("Make sure that", missing_cols, "exists in loggerinfo or remove it from `add_info`.")
    ))

  }

  #add `filename` as unique ID for a logger deployment

  add_info <- c(add_info, "filename")

  for (row_i in seq_along(rownames(loggerinfo))){

    filename <- loggerinfo$filename[row_i]

    data <- utils::read.csv(here::here(folder, filename))
    SN <- Logger::get_SN(data)
    names <- Logger::clean_colnames(data, row_i)

    names(data) <- names

    #check if SN in loggerinfo is the same as in csv

    if (check_SN & SN != loggerinfo$SN[row_i]){
      stop(paste0("Error in line ", row_i," of loggerinfo: The SN provided in loggerinfo (",
                  loggerinfo$SN[row_i], ") is different than the SN in the csv data (", SN ,")" ))
    }


    #check if deployed and retrieved in data

    if (min(lubridate::as_datetime(data$date_time), na.rm = T) > lubridate::as_datetime(loggerinfo$deployed[row_i])){
      rlang::inform(c(
        paste("The time of deployment does not exists in the logger data"),
        "i" = "Double check that the dates are filled out correctly"
      ))
    }

    if (max(lubridate::as_datetime(data$date_time), na.rm = T) < lubridate::as_datetime(loggerinfo$retrieved[row_i])){
      rlang::inform(c(
        paste("The time of retrival does not exists in the logger data"),
        "i" = "Double check that the dates are filled out correctly"
      ))
    }






    nrow_before <- nrow(data)


    # filter data with infos provided in loggerinfo
    data <- data %>%
      dplyr::mutate(date_time = lubridate::as_datetime(.data$date_time)) %>%
      dplyr::filter(.data$date_time >= lubridate::as_datetime(loggerinfo$deployed[row_i])) %>%
      dplyr::filter(.data$date_time <= lubridate::as_datetime(loggerinfo$retrieved[row_i]))

    nrow_after <- nrow(data)

    nrow_removed <- nrow_before - nrow_after

    perc_removed <- round(100 * nrow_removed / nrow_before,1)



    # add infos from loggerinfo to data based on selection in add_info

    add_info_filled <- loggerinfo[row_i,] %>%
      dplyr::select(dplyr::all_of(add_info))

    data <- data %>%
      dplyr::bind_cols(add_info_filled)


    if (!exists("merged_data")){

      merged_data <- data

    } else {

      merged_data <- dplyr::bind_rows(merged_data, data)

    }

    # remove "light_intensity" column if only NA values occur
    if (all(is.na(merged_data$light_intensity))){

      merged_data <- merged_data %>%
        dplyr::select(!.data$light_intensity)

    }



    rlang::inform(c("i" = filename,
                    "*" = paste0("removed ", nrow_removed, " of ", nrow_before, " rows (", perc_removed, "%)"),
                    "v" = paste("Read", row_i, "of", nrow(loggerinfo),"files."),
                    ""))

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
