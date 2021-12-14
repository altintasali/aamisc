#' Read sample datasets in [aamisc]
#'
#' @param file Path of the sample file
#'
#' @return \code{data.table}
#' @export
#' @importFrom data.table fread
#'
#' @examples
#' dt <- system.file("extdata", "sample_gsea_kegg.tsv", package = "aamisc")
#' sample_read(dt)
#'
sample_read <- function(file) {
  data.table::fread(file)
}
