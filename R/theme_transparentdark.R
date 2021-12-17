#' @title Make a transparent plot in ggplot
#'
#' @examples
#' # change palette
#' irss_gantt(gantt_df = gantt_example, palette = scico::scale_colour_scico_d(palette = "batlow")) +
#' theme_transparentdark()
#'
#' NOTE: if you would like the labels to be white you must specify this in the geom_label of the plot argument
#' e.g. : geom_label(aes(), color = "white")
#'
#' # make background transparent
#' irss_gantt(gantt_df = gantt_example) + theme_transparentdark()
#'
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 %+%
#' @export


theme_transparentdark <- function(){ggplot2::theme_bw(base_size = 15, base_family = "sans") %+replace%
    ggplot2::theme(
      axis.text = ggplot2::element_text(colour = "white"),
      axis.title = ggplot2::element_text(size = 18, color = "white"),
      legend.text = ggplot2::element_text(size = 14, color = "white"),
      legend.title = ggplot2::element_text(size = 14, color = "white"),

      strip.background = ggplot2::element_rect(
        color = NA, fill = NA, size = 1.5),
      strip.text.x = ggplot2::element_text(colour = "white", size = 15,
                                  margin =
                                    margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),
      strip.text.y = ggplot2::element_text(colour = "white", size = 15, angle = 270,
                                  margin =
                                    margin(t = 0, r = 0, b = 0, l = 3, unit = "pt")),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
      axis.line = ggplot2::element_line(colour = "white",
                               size = 1.5, linetype = "solid"),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank())
}
