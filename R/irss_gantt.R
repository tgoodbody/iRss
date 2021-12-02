#' @title Generate Gantt Chart
#'
#' @param gantt_df A dataframe useable by ggplot; see \code{\link[iRss]{gantt_example}} for format
#' @param palette a color palette function to be used to colour the plot
#' @param ... ggplot theme options and geom line options, used to make transparent background or other minor edits
#'
#'
#' @importFrom magrittr %>%
#' @export

irss_gantt <- function(gantt_df, palette = ggplot2::scale_colour_discrete(), ...) {

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

  gantt_plot <- df %>% ggplot2::ggplot(ggplot2::aes(value, name)) +
    ggplot2::geom_line(ggplot2::aes(colour = label), size = 4) +
    ggplot2::geom_text(data = labels, label = labels$name, vjust = -0.6, ...) +
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

  print(gantt_plot)
  gantt_plot
}
