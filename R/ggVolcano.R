#' ggplot2 wrapper for volcano plots
#'
#' @param x Table ([data.frame] or [data.table]) of differential expression results. See \code{limma::topTable} output as an example.
#' @param fdr.column Name of the column storing FDR values
#' @param pvalue.column Name of the column storing (raw) p-values. Default = NULL. Required if \code{yinfo = "pvalue"}
#' @param fdr FDR cutoff. Default = 0.05
#' @param logFC.column Name of the column storing logFC (log2 fold change) values
#' @param logFC logFC cutoff. Default = 0
#' @param col Color of dots
#' @param col.sig Color of significant dots. If 2 colors are picked, they will be used for 'down-regulated' and 'up-regulated' genes, respectively
#' @param alpha Transparency of dots. Range from 0 (fully tranparent) to 1 (opaque). Default = 1
#' @param text.column Name of the column storing text (e.g. genes) for labeling
#' @param text2plot Show text for dot. Character vector of \code{text} column in \code{x}. Default = NULL
#' @param dot.size Dot size to pass into \code{geom_point(aes(size))}. Default = 1
#' @param text.size Text size to pass into \code{geom_text(aes(size))} Default = 3
#' @param yaxis Plot 'p-value' or 'FDR' on y-axis. Default 'FDR'
#' @param linetype Linetype of the signficance cutoffs. Default 'dashed'.
#' @param xlab X-axis label. Default= NULL
#' @param ylab Y-axis label. Default= NULL
#' @param dge.info Boolean for showing differentially expressed gene (protein etc.) numbers. Default = TRUE
#' @param dge.info.loc The location of the \code{dge.info}. 0 is bottom and 1 is top on y-axis. Default = 0.1
#'
#' @return A ggplot2 object
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales breaks_pretty
#' @importFrom data.table data.table dcast `:=`
#' @import ggplot2
#'
#' @examples
#' ## Generate some data
#' n <- 5000
#' set.seed(666)
#' dge <- data.frame(Tags = paste0("Gene_", 1:n),
#'                   logFC = rnorm(n, mean = 0, sd = 1),
#'                                     pval = runif(n))
#' dge[1:100, "pval"] <- runif(100, min = 0, max = 0.001)
#' dge <- dge[order(dge$pval),]
#' dge$FDR <- p.adjust(dge$pval, method = "BH")
#'
#' ## Create volcano plot
#' ggVolcano(x = dge, fdr.column = "FDR", logFC.column = "logFC", fdr = 0.05)
#'
#' ## Change FDR cutoff
#' ggVolcano(x = dge, fdr.column = "FDR", logFC.column = "logFC", fdr = 0.10)
#'
#' ## Change logFC cutoff
#' ggVolcano(x = dge, fdr.column = "FDR", logFC.column = "logFC", fdr = 0.05, logFC = 1)
#'
#' ## Plot raw p-values on y-axis (keeps the FDR cutoff)
#' ggVolcano(x = dge, fdr.column = "FDR", logFC.column = "logFC", fdr = 0.05, logFC = 1,
#'           pvalue.column = "pval", yaxis = "pvalue")
#'
#' ## Add text for genes
#' text2plot <- dge[1:10, "Tags"]
#' ggVolcano(x = dge, fdr.column = "FDR", logFC.column = "logFC", fdr = 0.05,
#'           text.column = "Tags", text2plot = text2plot)
#' ggVolcano(x = dge, fdr.column = "FDR", logFC.column = "logFC", fdr = 0.05, logFC = 1,
#'           text.column = "Tags", text2plot = text2plot)
ggVolcano <- function(x,
                      fdr = 0.05,
                      fdr.column = "FDR",
                      pvalue.column = NULL,
                      logFC = 0,
                      logFC.column = "logFC",
                      col = "gray",
                      col.sig = c("blue", "red"),
                      alpha = 1,
                      text.column,
                      text2plot = NULL,
                      text.size = 3,
                      dot.size = 1,
                      linetype = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[3],
                      xlab = NULL,
                      ylab = NULL,
                      yaxis = c("fdr", "pvalue")[1],
                      dge.info = TRUE,
                      dge.info.loc = 0.1){

  ##------------------------------------------------------------------------
  ## Assign parameters to NULL to avoid check() NOTES
  ##------------------------------------------------------------------------
  y <- color <- value <- text <- `:=` <- .dummy <-  NULL

  ##------------------------------------------------------------------------
  ## Modify input
  ##------------------------------------------------------------------------
  x <- data.table::as.data.table(x)
  x[, .col := col]
  .logFC <- logFC
  .fdr <- fdr
  if(!is.null(pvalue.column)){
    .pvalue <- max(x[get(fdr.column) <= .fdr][, get(pvalue.column)])
  }



  if(length(col.sig) == 1){
    x[get(fdr.column) <= .fdr & (get(logFC.column) <= -abs(.logFC) | get(logFC.column) >= abs(.logFC)),
      .col := col.sig]
  }else if(length(col.sig) == 2){
    x[get(fdr.column) <= .fdr & get(logFC.column) <= -abs(.logFC), .col := col.sig[1]]
    x[get(fdr.column) <= .fdr & get(logFC.column) >= abs(.logFC), .col := col.sig[2]]
  }else{
    stop("col.sig should a color vector of length 1 or 2. 2 colors will be used for up/down-regulated genes, respectively.")
  }

  ##------------------------------------------------------------------------
  ## Parameter management
  ##------------------------------------------------------------------------
  ## Add more testing later
  if(!(is.data.frame(x) | data.table::is.data.table(x))){
    stop("x should be either data.frame or data.table")
  }

  if(!(is.numeric(fdr) & (min(fdr)) >= 0 | max(fdr) <= 1)){
    stop("fdr cutoff should a numeric with range [0, 1]")
  }

  if(!is.numeric(logFC)){
    stop("logFC should be numeric")
  }

  ##------------------------------------------------------------------------
  ## DGE info
  ##------------------------------------------------------------------------
  if(dge.info){
    updown <- x[, updown := ifelse(get(logFC.column) <= 0, "down", "up")]
    updown <- data.table::as.data.table(updown)
    #updown <- x[get(fdr.column) <= fdr & abs(get(logFC.column)) >= .logFC,]
    updown$updown <- factor(updown$updown, levels = c("down", "up"))
    updown[, .dummy := "count"]

    if(nrow(updown[get(fdr.column) <= fdr & abs(get(logFC.column)) >= .logFC, ]) == 0){
      updown <- data.table::dcast(updown,
                                updown ~ .dummy,
                                value.var = "updown",
                                fun.aggregate = length,
                                drop = FALSE)
      updown[,2] <- 0
    }else{
      updown <- data.table::dcast(updown[get(fdr.column) <= fdr & abs(get(logFC.column)) >= .logFC, ],
                                updown ~ .dummy,
                                value.var = "updown",
                                fun.aggregate = length,
                                drop = FALSE)

    }
    colnames(updown)[1:2] <- c("direction", "value")
    updown[, x:= c(-max(abs(range(x[,get(logFC.column)]))*0.9), max(abs(range(x[,get(logFC.column)]))*0.9))]
    updown$y <- c(max(-log10(x[,fdr.column, with = FALSE])*dge.info.loc))
    #updown[, text:= paste0(c("↓ ", "↑ "), value)]
    updown[, text:= paste0(c(intToUtf8(9660), intToUtf8(9650)), " ", value)]
    updown[, color:= col.sig]
  }

  ##------------------------------------------------------------------------
  ## Plot
  ##------------------------------------------------------------------------
  p <- suppressMessages(
    ggplot(x, aes(x = get(logFC.column),
                  y = switch(yaxis,
                             "fdr" = -log10(get(fdr.column)),
                             "pvalue" = -log10(get(pvalue.column)),
                  ),
    )
    ) +
      geom_point(col=x$.col, alpha = alpha, size = dot.size) +
      geom_vline(xintercept = c(-abs(logFC),abs(logFC)), linetype = linetype) +
      geom_hline(yintercept = switch(yaxis,
                                     "fdr" = -log10(.fdr),
                                     "pvalue" = -log10(.pvalue),
      ),
      linetype = linetype
      ) +
      switch(EXPR = as.character(dge.info),
             "TRUE" = geom_label(data = updown,
                                 aes(x,y, label = text),
                                 color = updown$color,
                                 parse = F,
                                 size = text.size,
                                 fill = NA
             )
      ) +
      switch(EXPR = as.character(is.null(xlab)),
             "TRUE" = xlab(expression(log[2]~Fold~Change)),
             "FALSE" = xlab(xlab)
      ) +
      switch(EXPR = as.character(is.null(ylab)),
             "TRUE" = switch(yaxis,
                             "fdr" = ylab(expression(-log[10](FDR))),
                             "pvalue" = ylab(expression(-log[10](p)))
             ),
             "FALSE" = xlab(xlab)
      ) +
      switch(EXPR = as.character(!is.null(text2plot)),
             "TRUE" = ggrepel::geom_text_repel(
               data = x[grepExact(text2plot, get(text.column))],
               aes(x = get(logFC.column),
                   y = -log10(get(fdr.column)),
                   label = get(text.column)
               ),
               color = x[grepExact(text2plot, get(text.column)), .col],
               size = text.size,
               box.padding = 0.5,
               max.overlaps = Inf
             )
      ) +
      theme_bw(base_size = 16) +
      xlim(-max(abs(range(x[,get(logFC.column)]))), max(abs(range(x[,get(logFC.column)])))) +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
      coord_cartesian(xlim = c(-max(abs(range(x[,get(logFC.column)]))), max(abs(range(x[,get(logFC.column)])))))
  )

  return(p)
}

