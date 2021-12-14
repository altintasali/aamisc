#' ggplot2 wrapper for plotMDS
#'
#' @param mds Output of [plotMDS]
#' @param dim Dimensions (x and y) to plot. Integer vector of length 2
#' @param meta Meta data of the experimental design. Column names: experimental factors to color. Row names: identical to \code{mds$cmdscale}.
#' @param color.by Column name of \code{meta} for colors
#' @param shape.by Column name of \code{meta} for shapes
#' @param text.by Column name of \code{meta} for texts
#' @param dot.size Dot size to pass into \code{geom_point(aes(size))}
#' @param text.size Test size to pass into \code{geom_text(aes(size))}
#' @param legend.position Legend position to pass into \code{theme(legend.position)}
#'
#' @return A ggplot2 object with MDS plots
#'
#' @export
#'
#' @importFrom limma plotMDS
#' @importFrom reshape2 melt dcast
#' @import ggplot2
#'
#' @examples
#' ## Meta data
#' meta <- data.frame(library = paste0("Lib", as.factor(1:12)),
#'                    patient = as.factor(rep(1:6, each = 2)),
#'                    disease = rep(c("healthy", "sick"), each = 6),
#'                    tissue = rep(c("liver","muscle"), times = 6))
#' meta$group <- paste(meta$disease, meta$tissue, sep = ".")
#' rownames(meta) <- meta$library
#'
#' ## Count matrix
#' counts <- matrix(data = round(runif(n = 1200, min = 0, max = 100)), ncol = nrow(meta))
#' colnames(counts) <- rownames(meta)
#' rownames(counts) <- paste("Gene", 1:nrow(counts), sep = "_")
#' head(counts)
#'
#' ## MDS object
#' mds <- limma::plotMDS(x = counts, ndim = 3, gene.selection = "pairwise", top = 500, plot = FALSE)
#'
#' ## Generate MDS plots with ggMDS
#' # Plot only points
#' ggMDS(mds = mds, meta = meta, dim = c(1,2))
#'
#' # Add colors
#' ggMDS(mds = mds,
#'       meta = meta,
#'       dim = c(1,2),
#'       color.by = "disease")
#'
#' # Add shapes
#' ggMDS(mds = mds,
#'       meta = meta,
#'       dim = c(1,2),
#'       color.by = "disease",
#'       shape.by = "tissue")
#'
#' # Add labels
#' ggMDS(mds = mds,
#'       meta = meta,
#'       dim = c(1,2),
#'       color.by = "disease",
#'       shape.by = "tissue",
#'       text.by = "library")
#'
#' # Change dimensions
#' ggMDS(mds = mds,
#'       meta = meta,
#'       dim = c(2,3),
#'       color.by = "disease",
#'       shape.by = "tissue",
#'       text.by = "library")
#'
#' # Change legend location
#' ggMDS(mds = mds,
#'       meta = meta,
#'       dim = c(1,2),
#'       color.by = "disease",
#'       shape.by = "tissue",
#'       text.by = "library",
#'       legend.position = "right")
#'
#' # Edit other ggplot2 options by creating a ggplot2 object
#' p <- ggMDS(mds = mds,
#'            meta = meta,
#'            dim = c(1,2),
#'            color.by = "disease",
#'            text.by = "library")
#' p
#' p + ggplot2::theme_minimal()
#'
#' \dontrun{
#' # Make a panel of all dimensions
#' p1 <- ggMDS(mds = mds,
#'             meta = meta,
#'             dim = c(1,2),
#'             color.by = "disease",
#'             text.by = "library",
#'             legend.position = "right")
#' p2 <- ggMDS(mds = mds,
#'             meta = meta,
#'             dim = c(1,3),
#'             color.by = "disease",
#'             text.by = "library",
#'             legend.position = "right")
#' p3 <- ggMDS(mds = mds,
#'             meta = meta,
#'             dim = c(2,3),
#'             color.by = "disease",
#'             text.by = "library",
#'             legend.position = "right")
#' patchwork::wrap_plots(list(p1, p2, p3)) + patchwork::plot_layout(guides = 'collect')
#' }
ggMDS <- function(mds,
                  meta,
                  dim = c(1,2),
                  color.by = NULL,
                  shape.by = NULL,
                  text.by = NULL,
                  dot.size = 2,
                  text.size = 2.5,
                  legend.position = "bottom"){

  if(!all(rownames(mds$cmdscale.out) %in% rownames(meta))){
    stop("rownames(meta) should be identical to colnames(x)")
  }
  axisLab <- mds$axislabel
  mds <- reshape2::melt(mds$cmdscale.out)
  colnames(mds)[1:2] <- c("sample", "dimension")
  mds$dimension <- paste0("dim", mds$dimension)
  mds <- reshape2::dcast(data = mds, formula = sample ~ dimension, value.var = "value")
  meta[,".sample"] <- rownames(meta)
  mds <- merge(mds, meta, by.x = "sample", by.y= ".sample")

  p <- ggplot(data = mds,
              mapping = aes_string(x = paste0("dim",dim[1]),
                                   y = paste0("dim",dim[2]))) +
    switch(EXPR = as.character(is.null(shape.by)),
           "FALSE" = geom_point(aes_string(color = color.by, shape = shape.by), size = dot.size),
           "TRUE" = geom_point(aes_string(color = color.by), size = dot.size)) +
    switch(EXPR = as.character(is.null(text.by)),
           "FALSE" = geom_text(aes_string(label = text.by), size = text.size, vjust = 1.5)) +
    xlab(paste(axisLab, dim[1])) + ylab(paste(axisLab, dim[2])) +
    theme_bw(base_size = 12) +
    theme(legend.position = legend.position)

  return(p)

}

