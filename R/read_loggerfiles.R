#' Read and combine logger csv files
#'
#' @param loggerinfo
#' @param folder
#' @param check_SN
#'
#' @return
#' @export
#'
#' @examples
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
    data <- read.csv(here::here(folder, loggerinfo$filename[row_i]))
    SN <- get_SN(data)
    names <- clean_colnames(data, row_i)

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
