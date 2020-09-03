#'Dotplot representation for enrichment results
#'
#'@param dt data.table or data.frame with the enrichment results. See details for necessary information.
#'@param topn Top \code{n} enriched terms to be plotted
#'@param topn.pref The ranking method to identify \code{topn} terms. Can be either \code{q} or \code{dot}.
#'@param qcut The adjusted p-value cutoff for enriched terms.
#'@param nchar Maximum characters to be shown on on y-axis. If the term contains more characters than \code{nchar}, it will be abbreviated with "(...)"
#'@param direction The column name in \code{dt} defining the direction  (e.g. up, down, all) of the enrichment
#'@param group The column name in \code{dt} defining the groups for each enrichment. The contents will be written on x-axis
#'@param dot The column name in \code{dt} defining the dot sizes. It must be numeric (e.g. 0.07) or character of fractions (e.g. "7/100")
#'@param dot.cex Positive numeric dot size scaling parameter.
#'@param q The column name (numeric) in \code{dt} defining the adjusted p-values for the enrichment.
#'@param ID The column name (numeric) in \code{dt} defining the ID of the enriched term.
#'@param Description The column name (numeric) in \code{dt} defining the description of the enriched term.
#'@param term.reverse Logical. Controls the reverse order (y-axis) plotting.
#'@param plot.by Panels (facet_grids) to divide while plotting. The options are "direction" (default) and "group"
#'@param pdf.name Generates a pdf file if not \code{NULL}
#'@param return.plotObj Boolean controlling whether a \code{ggplot2} object should be returned. Default is FALSE.
#'@return dotplot
#'@examples
#'## To be added later
#'@export


# - TO DO: add support for different criteria to plot
# - TO DO: remove parseGeneratio or add a flag for it

dotplotEnrich <- function(
  dt,
  topn      = 10,
  topn.pref = c("q", "dot"), # order q-values or dot sizes first?
  qcut      = 0.05,
  nchar     = 60,
  direction = "DEstatus",
  group     = "Condition",
  dot       = "GeneRatio",
  dot.cex   = 1,
  q         = "p.adjust",
  term.id   = "ID",
  term.name = "Description",
  term.reverse = FALSE,
  plot.by   = c("direction", "group")[1], # Panels (facet_grids) to divide while plotting
  pdf.name  = NULL,
  return.plotObj = FALSE
){
  #-------------------------------------------------------------------------
  # Input data (create data.table)
  #-------------------------------------------------------------------------
  dt <- setDT(dt)
  dt[,eval(group)] <- dt[,get(group)] %>% as.factor
  dt[,eval(dot) := parseGeneRatio(get(dot))]
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
  if(topn.pref == "q"){
    ids <- dt[order(get(direction),
                    get(q),
                    -abs(get(dot))),
              head(.SD, topn),
              by = .(get(group),
                     get(direction))]
  }else if(topn.pref == "dot"){
    ids <- dt[order(get(direction),
                    -abs(get(dot)),
                    get(q),
    ),
    head(.SD, topn),
    by = .(get(group),
           get(direction))]
  }else {
    stop("The order preference (priority) for topn gene can be either 'q' values or absoulte 'dot' sizes")
  }

  ids <- ids[get(q) < qcut, get(term.id)]
  datP <- dt[(get(term.id) %in% ids) & (get(q) < qcut),]
  #print(datP)
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
  colnames(hcMat.final) <- colnames.hcMat.final #paste(colnames(hcMat.final), rep(panels.hcMat, each = length(panels.hcMat)), sep = "_")

  hcDist <- dist(hcMat.final, method = "canberra")
  #if(!is.null(dim(hcDist))){
  if(attributes(hcDist)$Size > 1){
    hc <- hclust(hcDist, method = "ward.D2")
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
  #print(datP)

  #-------------------------------------------------------------------------
  # Generate plots
  #-------------------------------------------------------------------------
  if( dim(datP)[1] == 0 ){
    message("No terms enriched. Skipping the dotplot...")
  }else{
    if(plot.by == "direction"){
      p <- ggplot(datP, aes(x = get(group), y = get(term.name))) +
        geom_point(aes(size = get(dot), color = get(q))) +
        theme_bw(base_size = 12) +
        scale_colour_gradient(limits=c(0, qcut), low="red", high="blue") +
        ylab(NULL) + xlab(NULL) +
        scale_x_discrete(drop=FALSE) +
        scale_y_discrete(label=function(x) abbrev(x,nchar)) +
        scale_size_area() +
        labs(size=dot, colour=q) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        facet_grid(~get(direction))
    }else if(plot.by == "group"){
      p <- ggplot(datP, aes(x = get(direction), y = get(term.name))) +
        geom_point(aes(size = get(dot), color = get(q))) +
        theme_bw(base_size = 12) +
        scale_colour_gradient(limits=c(0, qcut), low="red", high="blue") +
        ylab(NULL) + xlab(NULL) +
        scale_x_discrete(drop=FALSE) +
        scale_y_discrete(label=function(x) abbrev(x,nchar)) +
        scale_size_area() +
        labs(size=dot, colour=q) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        facet_grid(~get(group))
    }

    print(p)
    if(!is.null(pdf.name)){
      pdf(pdf.name)
      print(p)
      dev.off()
    }

    if(return.plotObj){
      return(p)
    }
  }
}

## Helper functions
parseGeneRatio <- function(x){lapply(x, function(a){eval(parse(text = a))}) %>% unlist}
abbrev <- function(x, n=50){ #x: vector of character, n: maximum character allowed
  x[nchar(x) >= n] <- paste(strtrim(x[nchar(x) >= n], n-3), "...", sep="")
  return(x)
}

