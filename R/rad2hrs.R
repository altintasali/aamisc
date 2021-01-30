#' Radian to hours
#'
#' @param rad radian
#'
#' @return hours
#' @export
#'
#' @examples
#' degree <- c(0, 90, 180, 270, 360)
#' x <- degree*2*pi/360
#' rad2hrs(x)
rad2hrs <- function(rad){
  hrs <- NISTradianTOdeg(rad) / 360 * 24
  return(hrs)
}
