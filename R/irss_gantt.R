#' @title Generate Gantt Chart
#'
#' @param gantt_df A dataframe useable by ggplot; see \code{\link[iRss]{gantt_example}} for format
#' @param palette a color palette function to be used to colour the plot
#' @param transparent makes the background transparent, need to save it to properly visualize
#' @param ... ggplot theme options
#'
#' @examples
#' # change palette
#' irss_gantt(gantt_df = gantt_example, palette = scico::scale_colour_scico_d(palette = "batlow"))
#'
#' # make background transparent
#' irss_gantt(gantt_df = gantt_example, transparent = TRUE)
#'
#'
#' @importFrom magrittr %>%
#' @export

irss_gantt <- function(gantt_df, palette = ggplot2::scale_colour_discrete(), transparent = FALSE, ...) {

  if(is.null(colnames(gantt_df))) {
    stop("Please use a dataframe with the columns: label, name, start.date, end.date as the input for gantt_df")
  }

  if(sum(colnames(gantt_example) %in% c("label", "name", "start.date", "end.date")) != 4) {
    stop("Missing the correct column names. label, name, start.date, end.date are required column names")
  }

  if(!("ScaleDiscrete" %in% class(palette))) {
    stop("Palette is not a discrete colour palette")
  }

  df <- gantt_df %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("date"), lubridate::mdy)) %>%
    dplyr::arrange(start.date, as.Date(start.date)) %>%
    dplyr::mutate(date.order = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c(start.date, end.date), names_to = "variable") %>%
    dplyr::mutate(name = forcats::fct_rev(forcats::fct_reorder(name, date.order, min)),
                  label = forcats::fct_reorder(label, date.order, min))

  labels = df %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(value = mean(value))

  if(transparent) {
    gantt_plot <- df %>% ggplot2::ggplot(ggplot2::aes(value, name)) +
      ggplot2::geom_line(ggplot2::aes(colour = label), size = 4) +
      ggplot2::geom_text(data = labels, label = labels$name, vjust = -0.6, colour = "white") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     legend.position = c(0.9, 0.80),
                     legend.text = ggplot2::element_text(colour = "white"),
                     legend.background = ggplot2::element_rect(colour = "white"),
                     legend.key = ggplot2::element_rect(fill = "transparent"),
                     #axis.text.y = element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 0, vjust = -2, size = 14, colour = "white"),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_line(colour = "white"),
                     axis.line.x.top = ggplot2::element_line(colour = "white"),
                     axis.line.y.left = ggplot2::element_line(colour = "white"),
                     rect = ggplot2::element_rect(fill = "transparent"),
                     panel.background = ggplot2::element_rect(colour = "transparent"),
                     # bg of the panel
                     plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                     panel.grid.major = ggplot2::element_blank(), # get rid of major grid
                     panel.grid.minor = ggplot2::element_blank(), # get rid of minor grid
                     # get rid of legend bg
                     legend.box.background = ggplot2::element_rect(fill = "transparent"),
                     ...) +
      ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
      palette +
      ggplot2::labs(x = NULL,
                    y = NULL)

  } else {

    gantt_plot <- df %>%
      ggplot2::ggplot(ggplot2::aes(value, name)) +
      ggplot2::geom_line(ggplot2::aes(colour = label), size = 4) +
      ggplot2::geom_text(data = labels, label = labels$name, vjust = -0.6) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     legend.position = c(.8, .78),
                     axis.text.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     panel.grid.major.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     ...) +
      ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
      palette +
      ggplot2::labs(x = NULL,
                    y = NULL)
  }

  print(gantt_plot)
  gantt_plot
}
