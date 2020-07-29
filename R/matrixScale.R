#' Scale a matrix using group information across the columns
#'
#' @param x Input matrix
#' @param id A vector of group IDs. The length of this vector should be equal to the column number of input matrix.
#' @param center Should the data be centered? See \code{\link{scale}} for details
#' @param scale Should the data be scaled? See \code{\link{scale}} for details
#' @return A groupwise column scaled matrix
#' @examples
#'# Create a matrix with column-wise group information
#'set.seed(666)
#'x <- matrix(rnorm(75), 15, 5)
#'print(x)
#'groups <- c(rep("A",2), rep("B",3))
#'colnames(x) <- uniqueNames(groups)
#'y <- matrixScale(x = x, id = groups, center = TRUE, scale = TRUE)
#'print(y)
#'
#'# Group IDs do not need to be ordered
#'set.seed(666)
#'groups2 <- sample(groups)
#'colnames(x) <- uniqueNames(groups2)
#'y2 <- matrixScale(x = x, id = groups2, center = TRUE, scale = TRUE)
#'print(y2)
#' @export

matrixScale <- function(x = x,
                        id = rep("A", dim(x)[2]),
                        center = TRUE,
                        scale = TRUE)
{
  ##------------------------------------------------------------------------
  ## Data and erro handling
  ##------------------------------------------------------------------------
  x <- as.matrix(x)
  if(!all(apply(x, 2, is.numeric))){
    stop("All columns should be numeric. Please check your input.")
  }
  if(length(id) != dim(x)[2]){
    stop("The length of 'id' vector should be equal to the number of 'x' matrix columns")
  }

  ##------------------------------------------------------------------------
  ## The actual job
  ##------------------------------------------------------------------------
  uniq.id <- sort(unique(id))
  id.inds <- lapply(uniq.id, function(x){which(id %in% x)})
  names(id.inds) <- uniq.id # TODO: not really necessary to have, but good for debugging

  scaled <- lapply(id.inds, function(a, ...){
    scaled <- scale(as.vector(x[,a]), center = center, scale = scale)
    scaled <- matrix(scaled, ncol = length(a), byrow = F)
  })
  scaled <- do.call(cbind, scaled)[,unlist(id.inds)]
  colnames(scaled) <- colnames(x)
  return(scaled)
}
