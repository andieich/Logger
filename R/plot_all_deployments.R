#' Plot logger deployemnts
#'
#' This function plots all logger runs in `loggerdata` separately so you can check if there is any problem in the data.
#'
#' @param loggerdata The `loggerdata` dataframe
#' @param parameter The parameter that should be plotted. The default is `temperature`, currenlty supported is also `light_intensity`.
#' @param y_lab The y_lab that should be used in the plots. The default is `auto` wich sets the righ label for temperature and light_intensity. A character string can be specified if you want to use your own label.
#'
#' @return prints a plot for each logger deployment.
#' @export
#'
#' @examples
#' #' \dontrun{
#' plot_all_deployments <- plot_all_deployments(loggerdata)
#' }
#'
plot_all_deployments <- function(loggerdata,
                                 parameter = "temperature",
                                 y_lab = "auto"){


  if (!parameter %in% c("temperature", "light_intensity")){

    rlang::abort(c(
      paste("The chosen `parameter`", parameter, "was not recognized"),
      "x" = paste("`parameter` has to be either `temperature` or `light_intensity`")))
  }

  # select filenames with non-NA values for selected parameter

  enq_parameter <- rlang::sym(parameter)

  filenames <- loggerdata %>%
    dplyr::filter(!is.na(!!enq_parameter)) %>%
    dplyr::pull(filename) %>%
    unique()

  if (y_lab == "auto"){

    if (parameter == "temperature"){

      y_lab <- "Temperature (°C)"

    } else if (parameter == "light_intensity"){

      y_lab <- "Light intensity (lux)"

    }

  } else if (!is.character(y_lab)){

    rlang::abort(c(
      "The specified `y_lab` is not a character",
      "i" = paste("Make sure that `y_lab` is formatted as character")
    ))

  }

  for (filename_i in filenames){



    plot <- loggerdata %>%
      dplyr::filter(filename == filename_i) %>%
      ggplot2::ggplot(ggplot2::aes(x = date_time, y = !!enq_parameter))+
      ggplot2::geom_line()+
      ggplot2::scale_x_datetime(date_labels = "%d.%m.%y")+
      ggplot2::labs(x = NULL, y = y_lab, title = filename_i)+
      ggplot2::theme_minimal()

    print(plot)
  }
}
