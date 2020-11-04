#'Generate unique names of a vector with repeated elements
#'
#'@param x Input vector.
#'@param sep A character string to separate the terms. Default is "_".
#'@return Vector of unique names created by
#'@examples
#'x <- letters[c(1,2,2,3,3,3,4,6)]
#'res <- uniqueNames(x, sep = "_")
#'print(res)
#'res2 <- uniqueNames(x, sep = ".")
#'print(res2)
#'@export

uniqueNames <- function(x, sep="_"){
  x <- as.character(x)
  tab <- table(x)
  tab <- tab[tab > 1]
  lentab <- length(tab)
  if (lentab > 0) {
    u <- names(tab)
    for (i in 1:lentab) {
      n <- tab[i]
      x[x == u[i]] <- paste(x[x == u[i]],
                            formatC(1:n, width = 1 + floor(log10(n)), flag = "0"),
                            sep = sep)
    }
  }
  x
}
