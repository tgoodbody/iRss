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


theme_transparentlight <- function(){theme_bw(base_size = 14, base_family = "sans") %+replace%
    theme(
      axis.text = element_text(colour = "black"),
      axis.title = element_text(size = 16, color = "black"),
      legend.text = element_text(size = 14, color = "black"),
      legend.title = element_text(size = 14, color = "black"),
      strip.background = element_rect(
        color = NA, fill = NA, size = 1.5),
      strip.text.x = element_text(colour = "black", size = 10,
                                  margin =
                                    margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),
      strip.text.y = element_text(colour = "black", size = 10, angle = 270,
                                  margin =
                                    margin(t = 0, r = 0, b = 0, l = 3, unit = "pt")),
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      #panel.border = element_rect(colour = '#393b38', size = 1.5),
      plot.background = element_rect(fill = "transparent",colour = NA),
      axis.line = element_line(colour = "#393b38",
                               size = 1.5, linetype = "solid"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank())
}
