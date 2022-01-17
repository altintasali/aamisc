#' Find exact patterns using grep/grepl
#'
#' @param pattern Character vector of patterns. See [grep] for further details
#' @param x Character vector where patterns are sought. See [grep] for further details
#' @param boolean If TRUE (default) [grepl] will be used. If FALSE, [grep] will be used
#' @param ... Further parameters to be passed on [grep] or [grepl]
#'
#' @return A vector of pattern matches. If boolean = TRUE, a boolean vector of matches will be returned. If FALSE, a numeric vector of indices with correct matches will be returned. Can be modified by using other parameters to be passed on [grep] or [grepl]. See examples below.
#' @export
#'
#' @examples
#' x <- c("a", "aa", "b", "bb", 1, 11, 111, 2, 22, 222)
#' y <- c("a")
#'
#' # grep/grepl finds multiple matches
#' grepl(y, x)
#'
#' # grepExact finds exact matches
#' grepExact(y, x, boolean = FALSE)
#' grepExact(y, x, boolean = TRUE)
#'
#' # grep/grepl works with one string at a time
#' y <- c("a", 1)
#' grepl(y, x)
#' # although it can be modified with a lapply wrapper
#' unlist(lapply(y, function(a){grep(a,x)}))
#' # However, grepExact works as intended
#' grepExact(y, x, boolean = FALSE)
#' grepExact(y, x, boolean = TRUE)
grepExact <- function(pattern, x, boolean = TRUE, ...){
  if(boolean){
    grepl(paste(paste0("^", pattern, "$"), collapse = "|"), x, ...)
  }else{
    grep(paste(paste0("^", pattern, "$"), collapse = "|"), x, ...)
  }
}
