#' Abbreviate character string using a maximum character limit
#'
#' @param x A vector of character string.
#' @param n Maximum number (positive integer) of characters to be displayed before suffixed with triple dots ("...").
#'
#' @return An abbreviated character string
#'
#' @export
#'
#' @examples
#' a <- paste(letters, collapse = "")
#' abbrev(a, n = 1)
#' #[1] "a..."
#'
#' abbrev(a, n = 4)
#' #[1] "abcd..."
#'
abbrev <- function(x, n = 50){
  n <- as.integer(n)
  if(n <= 0){
    stop("'n' should be a positive integer")
  }else{
    x[nchar(x) >= n] <- paste(strtrim(x[nchar(x) >= n+3], n), "...", sep="")
  }
  return(x)
}
