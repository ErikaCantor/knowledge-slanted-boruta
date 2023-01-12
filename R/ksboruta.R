#' boruta_method
#'
#' Runs a feature selection function
#'
#' @description Boruta is an all relevant feature selection wrapper algorithm,
#' _boruta_method_ is a wrapper for knowledge-slanted random forest implementation.
#'  It tries to pick all the important dataset features
#'
#' @param data Training data of class \code{data.frame} or \code{matrix}
#' @param ntree `integer(1)` number of tree.
#' @param mtry `integer(1)` number of Variables randomly chosen at each split
#' @param selected variables selected
#' @param \ldots further arguments passed to [`ranger()`].
#'
#' @return `ks` object, see [`knowledge_slanted()`] for details.
#'
#' @seealso
#' [`knowledge_slanted()`]
#'
#' @references
#' Biological knowledge-slanted random forest approach
#' for the classification of calcified aortic valve stenosis
#' 10.1186/s13040-021-00269-4
#'
#' @import ranger
#' @import future
#' @import future.apply
#' @import tidyverse
#' @import purrr
#' @import ROCR
#' @export
boruta_method <- function(data, ntree, mtry, selected, ...){

  # number of genes or nodes
  p <- NULL
  if(hasArg(p)) p <- list(...)$p
  else p <- ncol(data)-1
  if( p > ncol(data)) stop(paste("p must be a number between 1 and", ncol(data)))
  shadows <- sapply(data[,-1], sample)
  ndata <- cbind(data, shadows)

  weights <- (rep(weights, 2)/ (1/p))/(2*p)
  colnames(ndata) <- c('y', paste('X', seq(1, p), sep=''), paste('S', seq(1, p), sep=''))

  #random forest
  rf <- ranger((y) ~. ,
               data = ndata,
               importance = 'impurity_corrected',
               split.select.weights=weights,
               mtry=mtry,
               num.trees=ntree,
               classification = TRUE,
               seed=23,
               ...)

  # janitza method for p values
  pvalues=data.frame(importance_pvalues(rf, method = "janitza"))
  # cuttoff=MZSA (Maximo p valor de las shadows), uso minimo dado que trabajo con valores p y son más significativos entre más cercanos a 0
  cuttoff=min(pvalues$pvalue[c(p+1,2*p)])
  pvalues$hit.data<-rep(0,2*p)
  pvalues$hit.data[pvalues$pvalue<cuttoff]<-1
  results=data.frame(hits=pvalues$hit.data[1:p])

  # Hace las nrep por cada datasets
  # hits <- future.apply::future_lapply(
  #   seq_len(1:nrep),
  #   ,
  #   future.seed = TRUE
  # )
  return(results)
}
