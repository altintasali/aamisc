#'Generate ggplot2 default colors
#'
#'@param n Number of colors
#'
#'@return Vector with color hexcodes
#'
#'@importFrom grDevices hcl
#'@examples
#'gg_color_hue(2)
#'# [1] "#F8766D" "#00BFC4"
#'gg_color_hue(5)
#'# [1] "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"
#'@export

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
