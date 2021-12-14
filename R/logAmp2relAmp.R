#' Convert log2 absolute amplitudes to relative amplitudes
#'
#' @param x Absolute amplitude of log2 scale data. Input as numeric vector.
#'
#' @return Relative amplitude (linear scale). Output as numeric vector
#' @details
#'Below are some details about the different amplitude definitions in cosinor regression:
#'
#'* Absolute amplitude \code{A_abs} is the amplitude calculated in a cosinor regression
#'* Log amplitude \code{A_log} is the amplitude of the log2 transformed cosinor curve
#'* Relative amplitude \code{A_rel} is calculated by dividing the \code{A_abs} by the mean value
#'* Fold change amplitude \code{A_fc} is fold change of relative amplitude
#'
#'\deqn{ 2A_{log} = log_{2}(A_{fc}) }
#'\deqn{ 2A_{fc} = (1+A_{rel}) / (1-A_{rel}) = 2^{2(A_{log})} }, therefore
#'\deqn{ A_{rel} = (2^{2(A_{log})}-1)/(2^{2(A_{log})}-1) }
#'
#' @export
#'
#' @examples
#' logAmp2relAmp(1:4)
logAmp2relAmp <- function(x){
  if(!is.numeric(x)){
    stop("x must be a numeric vector")
  }else{
    y <- (2^(2*x)-1) / (2^(2*x)+1)
  }
  return(y)
}
