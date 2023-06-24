
#' Summarize daily mean, min, and max values
#'
#' This function summarizes each logger run for the daily mean, min, and max values. This is supported fro `temperature` and `light_intensity`.
#'
#' @param loggerdata A dataframe containing the `loggerdada`
#' @param by A character string of columns in `loggerdata` by which the data should be grouped for the summary, additional to `filename`.
#' @param parameter The parameter that should be summarised. Can be `temperature` or `light_intensity`.
#'
#' @return A dataframe containing the columns specified in `by` and the mean, min, and max values of `parameter`.
#' @export
#'
#' @examples
#' \dontrun{
#' loggerdataS <- summarise_daily(loggerdata)
#' }
#'
#'
#'
summarise_daily <- function(loggerdata,
                            by = "filename",
                            parameter = "temperature"){


  # only one parameter

  if (length(parameter) != 1){
    rlang::abort(c(
      "`parameter` has to be a character vector of length 1.",
      "x" = paste("The provided `parameter` is of length", length(parameter))
    ))
  }


  #check if parameter is temp or int

  if (!parameter %in% c("temperature", "light_intensity")){

    rlang::abort(c(
      paste("The chosen `parameter`", parameter, "was not recognized"),
      "x" = paste("`parameter` has to be either `temperature` or `light_intensity`")))
  }


  # check if parameter exists in loggerdata

  if (any(!by %in% names(loggerdata))) {

    missing_cols <-  by[which(!by %in% names(loggerdata))]

    rlang::abort(c(
      "One or more nedded columns missing in loggerinfo.",
      "x" = paste(missing_cols, " was selected in `add_info` but is missing in the loggerinfo file."),
      "i" = paste("Make sure that", missing_cols, "exists in loggerinfo or remove it from `add_info`.")
    ))

  }


  # check if by is character vector

  if (any(!is.character(by))){
    rlang::abort(c(
      "`by` is not a character vector",
      "i" = "The grouping variables (`by`) have to be provided as a character vector"
    ))
  }


  # if filename is not present in by, add it

  if (!"filename" %in% by){
    by <-  c(by, "filename")
  }

  # check if all columns exists in loggerdata

  if (any(!by %in% names(loggerdata))) {

    missing_cols <-  by[which(!by %in% names(loggerdata))]

    rlang::abort(c(
      "`parameter` column not found in loggerdata",
      "x" = paste(missing_cols, " was chosen as `parameter` but is missing in the loggerdata file.")
    ))

  }

  by <- c("date", by)


  enq_parameter <- rlang::sym(parameter)

  enq_by <- rlang::syms(by)

  loggerdataS <- loggerdata %>%
    dplyr::filter(!is.na(!!enq_parameter)) %>%
    dplyr::mutate(date = lubridate::date(.data$date_time)) %>%
    dplyr::group_by(!!!enq_by) %>%
    dplyr::summarize("mean_{parameter}" := mean(!!enq_parameter),
                     "min_{parameter}" := min(!!enq_parameter),
                     "max_{parameter}" := max(!!enq_parameter))

  return(loggerdataS)
}


