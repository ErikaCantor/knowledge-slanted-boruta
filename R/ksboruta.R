#' ksboruta
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
#' @param weights numeric vector with weights between 0 and 1.
#' @param trace verbose. 0 means no tracing, 1 means decision about each attribute, 2 means the same as 1, plus reporting step by step,
#' @param \ldots further arguments passed to [`ranger()`].
#'
#' @return `ks` object, see [`knowledge_slanted()`] for details.
#'
#' @seealso [`knowledge_slanted()`]
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
#' @importFrom methods hasArg
#' @export
ksboruta <- function(data, ntree, mtry, weights, trace=FALSE, ...){

  # number of vars
  p <- NULL
  #Convert x into a data.frame
  if(!is.data.frame(data))
    data<-data.frame(data)

  if(hasArg(p)) p <- list(...)$p
  else p <- ncol(data)-1
  if( p > ncol(data)) stop(paste("p must be a number between 1 and", ncol(data)))
  shadows <- future.apply::future_sapply(data[,-1], FUN=sample, future.seed = NULL)
  newdata <- cbind(data, shadows)
  if (sum(weights) != 1) weights <- weights / sum(weights)

  # to include shadow and non-shadow weights
  if(length(newdata) > length(weights) + 1) weights <- (rep(weights, 2)/ (1/p))/(2*p)

  colnames(newdata) <- c('y', paste('X', seq(1, p), sep=''), paste('S', seq(1, p), sep=''))
  #random forest
  options(warn = -1);
  rf <- ranger((y) ~. ,
               data = newdata,
               importance = 'impurity_corrected',
               split.select.weights=weights,
               mtry=mtry,
               num.trees=ntree,
               classification = TRUE,
               verbose =FALSE,
               ...)
  # calculates p-values using janitza method
  pvalues <-  data.frame(importance_pvalues(rf, method = "janitza"))
  options(warn = 0);
  # NOTE: cuttoff = MZSA (Maximo p valor para las shadows),
  # NOTE: uso minimo dado que trabajo con valores p
  # NOTE: y son más significativos entre más cercanos a 0
  cuttoff=min(pvalues$pvalue[c(p+1, 2*p)])
  pvalues$hit.data<-rep(0, 2*p)
  pvalues$hit.data[pvalues$pvalue < cuttoff]<-1
  results=pvalues$hit.data[1:p]
  if(trace){
    message(
      sprintf(
        '%s are significantly better than shadows',
        length(results[results!=1]) ))
  }
  return(results)
}
