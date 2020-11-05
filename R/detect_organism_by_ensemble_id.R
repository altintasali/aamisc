#' Get organism information from ENSEMBL ID
#'
#' @param ensembl_gene_id Ensemble gene, transcript or protein ID as character (e.g. ENSMUSG00000024411). Currently supports only human, mouse and rat
#'
#' @return List object (named \code{org}) contanining \n
#'\begin{itemize}
#'    \item \code{$org} organism name (e.g. mouse)
#'    \item \code{$id} \href{http://www.genome.jp/kegg/catalog/org_list.html}{KEGG ID}  of the organism (e.g. mmu for mouse)
#'    \item \code{$db} org.db library in R (e.g. org.Mm.eg.db for mouse)
#'    \item \code{$scientific} Scientific name of the organism (e.g. 'Mus musculus' for mouse)
#'\end{itemize}
#' @export
#'
#' @examples
#' detect_organism_by_ensemble_id(ensembl_gene_id = "ENSMUSG00000024411")
#'
#' #$org
#' #[1] "mouse"
#' #
#' #$id
#' #[1] "mmu"
#' #
#' #$db
#' #[1] "org.Mm.eg.db"
#' #
#' #$scientific
#' #[1] "Mus musculus"
#' #
#' #$type
#' #[1] "gene"
#'
detect_organism_by_ensemble_id <- function(ensembl_gene_id){
  if(grepl("ENS\\D\\d+", ensembl_gene_id)){
    org <- list(org = "human", id = "hsa", db = "org.Hs.eg.db", scientific = "Homo sapiens")
    if(grepl("ENSG\\d+", ensembl_gene_id)){
      org$type <- "gene"
    }else if(grepl("ENST\\d+", ensembl_gene_id)){
      org$type <- "transcript"
    }else if(grepl("ENSP\\d+", ensembl_gene_id)){
      org$type <- "protein"
    }else{
      warning("The ID might belong to 'human', but does not seem to be valid.")
    }

  }else if(grepl("ENSMUS\\D\\d+", ensembl_gene_id)){
    org <- list(org = "mouse", id = "mmu", db = "org.Mm.eg.db", scientific = "Mus musculus")
    if(grepl("ENSMUSG\\d+", ensembl_gene_id)){
      org$type <- "gene"
    }else if(grepl("ENSMUST\\d+", ensembl_gene_id)){
      org$type <- "transcript"
    }else if(grepl("ENSMUSP\\d+", ensembl_gene_id)){
      org$type <- "protein"
    }else{
      warning("The ID might belong to 'mouse', but does not seem to be valid.")
    }

  }else if(grepl("ENSRNO\\D\\d+", ensembl_gene_id)){
    org <- list(org = "rat", id = "rna", db = "org.Rn.eg.db", scientific = "Rattus norvegicus")
    if(grepl("ENSRNOG\\d+", ensembl_gene_id)){
      org$type <- "gene"
    }else if(grepl("ENSRNOT\\d+", ensembl_gene_id)){
      org$type <- "transcript"
    }else if(grepl("ENSRNOP\\d+", ensembl_gene_id)){
      org$type <- "protein"
    }else{
      warning("The ID might belong to 'mouse', but does not seem to be valid.")
    }

  }else{
    stop("Unknown ENSEMBL ID!")
  }

  return(org)
}

