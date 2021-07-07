#' Create an 1-row NA table from data.frame while keeping the column names. Works for empty data.frames as well.
#'
#' @param df data.frame
#' @param onlyEmptyTable logical. If TRUE (default), only empty tables will be converted to 1-row NA table. Else, full table will be converted to 1-row NA table.
#'
#' @return 1-row NA table.
#' @export
#'
#' @examples
#'
#'x <- matrix(seq(1:12), nrow = 3)
#'colnames(x) <- letters[1:4]
#'x <- data.frame(x)
#'print(x)
#'
#'## Throws an error as the data.frame is not empty.
#'createNAdf(df = x, onlyEmptyTable = TRUE)
#'
#'## Setting the onlyEmptyTable to FALSE will ignore it
#'createNAdf(df = x, onlyEmptyTable = FALSE)
#'#   a  b  c  d
#'#1 NA NA NA NA
#'
#'## Originally intended to use for empty data.frames
#'x2 <- x[0,]
#'createNAdf(df = x2, onlyEmptyTable = TRUE)
#'#   a  b  c  d
#'#1 NA NA NA NA

createNAdf <- function(df, onlyEmptyTable = TRUE){
 if(onlyEmptyTable){
   if(dim(df)[1] > 0){
     stop("Input data.frame in not empty (0-row). If you still would like to create a 1-row NA data.frame from it, set onlyEmptyTable = FALSE")
   }else{
     df <- df
   }
 }else{
   df <- df[0,]
 }
 colNames <- colnames(df)
 out <- df[0, ]
 nCol <- ncol(df)
 out[1, 1:nCol] <- NA

 return(out)
}

