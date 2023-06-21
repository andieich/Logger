#' Read logger metadata
#'
#' @param path The path to the Excel file (`.xlsx`) containing the metadata for the logger deployments
#' @param tab The tab in the Excel file containing the metadata for the logger deployments
#'
#' @return a `data.frame` with the logger metadata
#' @export
#'
#' @examples
#' \dontrun{
#' loggerinfo <- read_loggerinfo(here::here("data/logger_info.xlsx"))
#' }
#'
read_loggerinfo <- function(path, tab = "retrieved"){
  loggerinfo <- readxl::read_excel("data/logger_info.xlsx",
                                   sheet = "retrieved")
}
