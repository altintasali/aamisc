#' Get current script's path
#' @import rstudioapi
#' @param keep_name Keep name of the script. Default is TRUE.
#'
#' @return Path of the current script
#' @export
#'
#' @references
#' This script was inspired by \href{https://stackoverflow.com/a/49197577/1488825}{StackOverflow post} by user \href{https://stackoverflow.com/users/9131220/tinker}{Tinker}.
#' Only added a parameter called "keep_name"

get_script_path <- function(keep_name = TRUE) {
  args <- commandArgs(trailingOnly = FALSE)
  file <- "--file="
  rstudio <- "RStudio"

  if(keep_name){
    pathFunction <- I
  }else{
    pathFunction <- dirname
  }

  match <- grep(rstudio, args)
  if (length(match) > 0) {
    return(pathFunction(rstudioapi::getSourceEditorContext()$path))
  } else {
    match <- grep(file, args)
    if (length(match) > 0) {
      return(pathFunction(normalizePath(sub(file, "", args[match]))))
    } else {
      return(pathFunction(normalizePath(sys.frames()[[1]]$ofile)))
    }
  }
}
