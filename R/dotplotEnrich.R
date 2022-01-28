#'Dotplot representation for enrichment results
#'
#'@param dt data.table or data.frame with the enrichment results. See details for necessary information.
#'@param topn Top \code{n} enriched terms to be plotted
#'@param topn.pref The ranking method to identify \code{topn} terms. Can be either \code{qval} or \code{dot}. \code{qval} sorts the dots by ascending order (lowest q-value first) and \code{dot} sorts the dots by descending order (e.g. biggest NES/GeneRatio) by absolute values.
#'@param qcut The adjusted p-value cutoff for enriched terms.
#'@param nchar Maximum characters to be shown on on y-axis. If the term contains more characters than \code{nchar}, it will be abbreviated with "(...)"
#'@param direction The column name in \code{dt} defining the direction  (e.g. up, down, all) of the enrichment
#'@param group The column name in \code{dt} defining the groups for each enrichment. The contents will be written on x-axis
#'@param dot The column name in \code{dt} defining the dot sizes. It must be numeric (e.g. 0.07) or character of fractions (e.g. "7/100")
#'@param dot.cex Positive numeric dot size scaling parameter.
#'@param qval The column name in \code{dt} defining the adjusted p-values for the enrichment.
#'@param term.reverse Logical. Controls the reverse order (y-axis) plotting.
#'@param plot.by Panels (facet_grids) to divide while plotting. The options are "direction" (default) and "group"
#'@param onepanel Logical (default FALSE). If TRUE, removes the multiple panels (no facet_grid)
#'@param arrows Logical (default FALSE). Should "direction" arrows be plotted inside dots?
#'@param term.id ID of the ontology term
#'@param term.name Name of the ontology term
#'@param arrow.col Color of the \code{arrows}. Default is "beige".
#'
#'@return ggplot2 object for dotplot
#'
#'@import ggplot2
#'@import magrittr
#'@importFrom data.table data.table setDT is.data.table
#'@importFrom stats hclust dist p.adjust
#'@importFrom utils head
#'@importFrom stringr str_trunc
#'
#'
#'@examples
#'## Read sample data
#'dt <- system.file("extdata", "sample_gsea_kegg.tsv", package = "aamisc")
#'dat <- sample_read(dt)
#'colnames(dat)
#'
#'## Dotplot with top 5 significant ontology (KEGG) terms
#'p <- dotplotEnrich(
#'         dt = dat,
#'         topn = 5,
#'         topn.pref = "dot",
#'         qcut = 0.05,
#'         nchar = 65,
#'         direction = "Direction",
#'         group = "Contrast",
#'         dot = "NES",
#'         dot.cex = 1,
#'         qval = "qvalue",
#'         term.id = "ID",
#'         term.name = "Description",
#'         term.reverse = TRUE,
#'         plot.by = "direction")
#'print(p)
#'
#'## Saving as PDF file: I recommend using cairo_pdf
#'\dontrun{
#'cairo_pdf("pretty_enrich_plot.pdf", height = 7, width = 7)
#'print(p)
#'dev.off()
#'}
#'
#'@export
dotplotEnrich <- function(
  dt,
  topn      = 10,
  topn.pref = c("qval", "dot"), # order q-values or dot sizes first?
  qcut      = 0.05,
  nchar     = 60,
  direction = "DEstatus",
  group     = "Condition",
  dot       = "GeneRatio",
  dot.cex   = 1,
  qval      = "p.adjust",
  term.id   = "ID",
  term.name = "Description",
  term.reverse = FALSE,
  plot.by   = c("direction", "group")[1], # Panels (facet_grids) to divide while plotting
  onepanel  = FALSE,
  arrows    = FALSE,
  arrow.col = "beige"
){
  ##------------------------------------------------------------------------
  ## Fix global variables
  ##------------------------------------------------------------------------
  .label <- . <- `:=` <- `.SD` <- NULL

  #-------------------------------------------------------------------------
  # Check if the data structure is as required
  #-------------------------------------------------------------------------
  if(!(is.data.frame(dt) | is.data.table(dt))){
    stop("'dt' can be either data.frame or data.table")
  }

  dt <- data.table::setDT(dt)
  dtcols <- colnames(dt)

  if(!is.numeric(topn)){
    stop("'topn' must be an integer")
  }

  if(!topn.pref %in% c(c("qval", "dot"))){
    stop("'topn.pref' can be either 'qval' or 'dot'")
  }

  if(!is.numeric(qcut) & (qcut >= 0 | qcut <=1)){
    stop("'qcut' must be a numeric between 0 and 1")
  }

  if(!is.numeric(nchar) & (nchar > 4)){
    stop("'qcut' must be a numeric bigger than 4")
  }

  if(!direction %in% dtcols){
    stop(
      paste0("Column with 'direction' information cannot be found in 'dt'\n",
             "You have provided: ", direction, "\n",
             "Column names in 'dt': ", paste(dtcols, collapse = ", ") )
    )
  }

  if(!group %in% dtcols){
    stop(
      paste0("Column with 'group' information cannot be found in 'dt'\n",
             "You have provided: ", group, "\n",
             "Column names in 'dt': ", paste(dtcols, collapse = ", ") )
    )
  }

  if(!dot %in% dtcols){
    stop(
      paste0("Column with 'dot' information cannot be found in 'dt'\n",
             "You have provided: ", dot, "\n",
             "Column names in 'dt': ", paste(dtcols, collapse = ", ") )
    )
  }

  if(!is.numeric(dot.cex) & (dot.cex > 0)){
    stop("'dot.cex' must be a numeric bigger 0")
  }

  if(!qval %in% dtcols){
    stop(
      paste0("Column with 'qval' information cannot be found in 'dt'\n",
             "You have provided: ", qval, "\n",
             "Column names in 'dt': ", paste(dtcols, collapse = ", ") )
    )
  }

  if(!term.id %in% dtcols){
    stop(
      paste0("Column with 'term.id' information cannot be found in 'dt'\n",
             "You have provided: ", term.id, "\n",
             "Column names in 'dt': ", paste(dtcols, collapse = ", ") )
    )
  }

  if(!term.name %in% dtcols){
    stop(
      paste0("Column with 'term.id' information cannot be found in 'dt'\n",
             "You have provided: ", term.name, "\n",
             "Column names in 'dt': ", paste(dtcols, collapse = ", ") )
    )
  }

  if(!is.logical(term.reverse)){
    stop("'term.reverse' must be logical")
  }

  if(!plot.by %in% c("direction", "group")){
    stop("'plot.by' can be either 'direction' or 'group'")
  }

  if(!is.logical(arrows)){
    stop("'arrows' must be logical")
  }

  #-------------------------------------------------------------------------
  # Input data (create data.table)
  #-------------------------------------------------------------------------
  dt[,eval(group)] <- dt[,get(group)] %>% as.factor
  dt[,eval(direction)] <- dt[,get(direction)] %>% as.factor
  dt[,eval(dot) := parseGeneRatio(get(dot))]

  dt[, .label := get(direction)]
  dt[tolower(.label) == "down", .label := "\u25BC"]
  dt[tolower(.label) == "up", .label := "\u25B2"]
  dt[tolower(.label) == "all", .label := "\u21C5"]
  dt[!tolower(get(direction)) %in% c("all", "down", "up"), .label := "\u00D7"]

  #-------------------------------------------------------------------------
  # Debug variable below
  #-------------------------------------------------------------------------
  # direction = "DEstatus"
  # group     = "Condition"
  # dot       = "GeneRatio"
  # q         = "p.adjust"
  # qcut      = 0.05
  # term.id   = "ID"
  # term.name = "Description"
  # plot.by   = "direction"
  # topn = 10

  #-------------------------------------------------------------------------
  # Subset the top n terms to be plotted
  #-------------------------------------------------------------------------
  if(topn.pref == "qval"){
    ids <- dt[order(get(direction),
                    get(qval),
                    -abs(get(dot))),
              head(.SD, topn),
              by = .(get(group),
                     get(direction))]
  }else if(topn.pref == "dot"){
    ids <- dt[order(get(direction),
                    -abs(get(dot)),
                    get(qval),
    ),
    head(.SD, topn),
    by = .(get(group),
           get(direction))]
  }else {
    stop("The order preference (priority) for topn gene can be either 'q' values or absolute 'dot' sizes")
  }

  ids <- ids[get(qval) < qcut, get(term.id)]
  datP <- dt[(get(term.id) %in% ids) & (get(qval) < qcut),]
  datP <- datP[, eval(dot) := parseGeneRatio(get(dot))]

  #-------------------------------------------------------------------------
  # Hierarchical cluster for dot organization
  #-------------------------------------------------------------------------
  cMat <- datP[,.(get(term.id), get(group), get(direction))]
  hcMat <- table(cMat)
  panels.hcMat <- attributes(hcMat)$dimnames$V3
  hcMat <- lapply(panels.hcMat, function(x){rbind(hcMat[,,x])})
  names(hcMat) <- panels.hcMat
  hcMat.final <- do.call(cbind, hcMat)
  colnames.hcMat.final <- paste(lapply(hcMat, colnames) %>% unlist,
                                rep(panels.hcMat, times = lapply(hcMat, function(x){dim(x)[2]}) %>% unlist),
                                sep = "_")
  colnames(hcMat.final) <- colnames.hcMat.final

  hcDist <- dist(hcMat.final, method = "canberra")
  if(attributes(hcDist)$Size > 1){
    hc <- stats::hclust(hcDist, method = "ward.D2")
    hcOrder <- hc$order %>% rev
    idOrder <- hcMat.final[hcOrder, ] %>% rownames
    idOrder <- lapply(idOrder, function(x){grep(x, datP[,get(term.id)] %>% unlist)}) %>% do.call(c, .)
    datP <- datP[idOrder, ]
  }else{
    datP <- datP[order(get(direction), get(dot)),]
  }

  datP[, eval(term.name) := factor(datP[,get(term.name)], levels = datP[,get(term.name)] %>% unique)]
  if(term.reverse){
    datP[, eval(term.name) := factor(datP[,get(term.name)], levels = datP[,get(term.name)] %>% unique %>% rev)]
  }
  datP[, eval(group) := factor(datP[,get(group)], levels = levels(dt[,get(group)]) %>% unique)]
  datP[, eval(direction) := factor(datP[,get(direction)], levels = levels(dt[,get(direction)]) %>% unique)]

  #-------------------------------------------------------------------------
  # Generate plots
  #-------------------------------------------------------------------------
  if( nrow(datP) == 0 ){
    message("No terms enriched. Skipping the dotplot...")
  }else{
    p <- ggplot(datP, aes(x = switch(plot.by,
                                     "direction" = switch(as.character(onepanel),
                                                          "FALSE" = get(group),
                                                          "TRUE"  = get(direction)
                                                          ),
                                     "group"     = switch(as.character(onepanel),
                                                          "FALSE" = get(direction),
                                                          "TRUE"  = get(group)
                                                          )
                                     ),
                          y = get(term.name))) +
      geom_point(aes(size = get(dot), color = get(qval))) +
      switch(as.character(arrows),
             "TRUE" = geom_text(aes(label = .label, size = get(dot)),
                                family = "Arial Unicode MS",
                                color = arrow.col,
                                show.legend = FALSE)

             ) +
      theme_bw(base_size = 12) +
      scale_colour_gradient(limits=c(0, qcut), low="red", high="blue") +
      ylab(NULL) + xlab(NULL) +
      scale_x_discrete(drop=FALSE) +
      scale_y_discrete(label=function(x) stringr::str_trunc(string = x, width = nchar)) +
      scale_size_area() +
      labs(size=dot, colour=qval) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      switch(as.character(onepanel),
             "FALSE" = switch(plot.by,
                              "direction" = facet_grid(~get(direction), drop = FALSE),
                              "group"     = facet_grid(~get(group), drop = FALSE)
                              )
             )
    return(p)
  }

}

## Helper functions
parseGeneRatio <- function(x){lapply(x, function(a){eval(parse(text = a))}) %>% unlist}

parseMath <- function(x){eval(parse(text = x))} # Text to math operations

