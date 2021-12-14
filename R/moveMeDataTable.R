#'Move columns in a \code{data.table}
#'
#'This function helps moving the columns in big \code{\link{data.table}}.
#'
#'@param data A \code{data.table} object.
#'@param tomove The column to move.
#'@param where Where to move the column(s)?
#'\itemize{
#'     \item \code{last} Moves the column to the last (default)
#'     \item \code{first} Moves the column to the first
#'     \item \code{before} Moves the column before \code{ba}
#'     \item \code{after} Moves the column after \code{ba}
#'     }
#'@param ba The column that points "before" or "after" locations used in \code{where}
#'
#'@return Re-ordered \code{data.table}
#'
#'@importFrom data.table data.table setcolorder
#'
#'@examples
#'DT <- data.table::data.table(A=sample(3, 10, TRUE),
#'                             B=sample(letters[1:3], 10, TRUE), C=sample(10))
#'DT <- moveMeDataTable(DT, "C", "after", "A")
#'
#'@author
#'User \href{https://stackoverflow.com/users/5973989/moun}{Moun} and \href{https://stackoverflow.com/users/1270695/a5c1d2h2i1m1n2o1r2t1}{A5C1D2H2I1M1N2O1R2T1} from StackOverflow.
#'@references
#'\href{https://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe}{StackOverflow Thread}
#'
#'@export

moveMeDataTable <-function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = setcolorder(data,c(tomove, temp)),
    last = setcolorder(data,c(temp, tomove)),
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)-1))
      setcolorder(data,order)

    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      order = append(temp, values = tomove, after = (match(ba, temp)))
      setcolorder(data,order)
    })
  x
}
