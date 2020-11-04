#' Get directory root
#'
#' @param x Path of a file or directory. Acts similar to \code{\link{basename}}
#' @param dirLevel Pointer to base directory. 0 means the base directory. 1 or bigger integer is upper directory in that level. See examples.
#'
#' @return Directory with the requested level information
#' @export
#'
#' @examples
#' file <- "/home/projects/read.r"
#' basenameLevel(file)
#' # [1] "read.r"
#'
#' basenameLevel(file, dirLevel = 1)
#' # [1] "projects/read.r"
#'
#' #' basenameLevel(file, dirLevel = 2)
#' # [1] "home/projects/read.r"

basenameLevel <- function(x, dirLevel = 0){
  subStrs <- strsplit(x, "[/]")[[1]]
  paste(subStrs[(length(subStrs) - dirLevel):length(subStrs) ], collapse="/")
}
