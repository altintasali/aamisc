#' Get file root
#'
#' @param x Path of a file
#'
#' @return Removed file extension. Still includes the path information
#' @export
#'
#' @examples
#' file <- "/home/projects/read.r"
#' getFileRoot(file)
#' #[1] "/home/projects/read"
#'
#' file <- "/aaa/bbb/ccc/ddd/eee/fff/ggg.PDF"
#' getFileRoot(file)
#' #[1] "/aaa/bbb/ccc/ddd/eee/fff/ggg"
#'
#' basenameLevel(getFileRoot(file))
#' #[1] "ggg"
#'
#' basenameLevel(getFileRoot(file), dirLevel = 1)
#' #[1] "fff/ggg"

getFileRoot <- function(x){
  subStrs <- strsplit(x, "[.]")[[1]]
  paste(subStrs[1:(length(subStrs)-1)], collapse=".")
}

