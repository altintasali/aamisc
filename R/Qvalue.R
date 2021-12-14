#' Calculate q-value by \code{\link{qvalue}}. If it fails, calculated adjusted p-value by \code{\link{p.adjust}}
#'
#' @param x p-values
#' @param alternative.method If \code{\link{qvalue}} fails to calculate, function defaults to \code{\link{p.adjust}}. \code{alternative.method}
#' will be used as \code{method} in \code{\link{p.adjust}}. Default is "BH".
#' @return q-values. If q-value cannot be calculated by \code{\link{qvalue}}, it throws an error. The \code{\link{Qvalue}} catches
#' this error without stopping and changes the p-value adjustment to \code{\link{p.adjust}}.
#' @import qvalue
#' @examples
#'# Q-values can be calculated
#'p <- c(runif(n = 100, min = 0, max = 0.05),
#'       runif(n = 900, min = 0, max = 1))
#'q <- Qvalue(p)
#'par(mfrow = c(1,2))
#'hist(p); hist(q)
#'
#'# Q-values cannot be calculated
#'p <- c(runif(n = 1, min = 0, max = 0.05),
#'       runif(n = 9, min = 0, max = 1))
#'q <- Qvalue(p)
#'par(mfrow = c(1,2))
#'hist(p); hist(q)
#' @export

Qvalue <- function(x, alternative.method = "BH"){
  tryCatch(
    expr = {
      y <- qvalue::qvalue(x)$qvalues
      message("Successfully calculated Q-value")
      return(y)
    },
    error = function(e){
      message(paste0('q-value cannot be calculated. Using alternative method: ', alternative.method))
      #qvalue(x, pi0 = 1)$qvalues
      y <- p.adjust(x, method = alternative.method)
      return(y)
    },
    warning = function(w){
      message('Caught a warning for qvalue()!')
      print(w)
    },
    finally = {
      message('Done p-value adjustments')
    }
  )
}

