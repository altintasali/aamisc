#'Convert \code{data.frame} to \code{matrix} while moving the non-numeric values into \code{rownames}
#'
#'@param df A \code{data.frame}
#'@param col2rowname Vector for the columns to be moved to rownames. Can be numeric (column ids) or character (column names)
#'@param sep Seperator for rownames if more than 1 element supplied in \code{col2rowname}
#'@return A matrix object.
#'@examples
#'# Create sample data.frame
#'x <- data.frame(Group1 = letters[1:5], ids = 1:5, id.squared = (1:5)^2)
#'x
#'X <- df2matrix(x)
#'X
#'
#'# Create sample data.frame with 2 character columns
#'x2 <- data.frame(x, Group2 = LETTERS[6:10])
#'x2
#'
#'X2 <- df2matrix(x, col2rowname = c(1,4), sep = "_")
#'X2
#'X2 <- df2matrix(x, col2rowname = c("Group1","Group2), sep = "_")
#'X2
#'
#'# Rownames can be reverse ordered
#'X3 <- df2matrix(x, col2rowname = c(1,4), sep = "_")
#'X3
#'X3 <- df2matrix(x, col2rowname = c("Group1","Group2), sep = "_")
#'X3
#'
#'@export

df2matrix <- function(df, col2rowname = c("new_groups","groups"), sep = "_"){
  if(!is.character(col2rowname)){
    col.names <- colnames(df)[col2rowname]
    col.ids <- col2rowname
  }else{
    col.names <- col2rowname
    col.ids <- lapply(col.names, function(a){grep(pattern = paste0("^",a,"$"), x = colnames(df))})
    col.ids <- unlist(col.ids)
  }

  if(length(col2rowname) < 2){
    rnames <- df[,col.ids]
  }else{
    rnames <- apply(df[,col.ids], 1, paste, collapse="_")
  }

  M <- as.matrix(df[,-col.ids])
  rownames(M) <- rnames
  return(M)
}
