#' Abbreviate character string using a maximum character limit
#'
#' @param x A vector of character string.
#' @param n Maximum Number of characters to be displayed before suffixed with triple dots ("...")
#'
#' @return An abbreviated character string
#' @export
#'
#' @examples
#'
#' a <- paste(letters, collapse = "")
#' abbrev(a, n = 0)
#' #[1] "a..."
#'
#' abbrev(a, n = 4)
#' #[1] "abcd..."
#'
abbrev <- function(x, n = 50){ #x: vector of character, n: maximum character allowed
  n <- as.integer(n)
  if(n < 0){
    stop("n cannot be negative")
  }else if(n == 0){
    warning("n = 0 results with non-informative abbreviation as '...'")
  }else{
    x[nchar(x) >= n] <- paste(strtrim(x[nchar(x) >= n+3], n), "...", sep="")
  }
  return(x)
}
