#' knowledge_slanted
#'
#' The knowledge-slanted RF approach to identify genes potentially implicated
#' with CAVS in patients
#'
#' @description The knowledge-slanted RF approach  is a combination of two stages as an attempt to 
#' implement a knowledge-guided supervised learning approach based on the conventional RF. 
#'
#' @param data Training data of class \code{data.frame}, \code{matrix}
#' @param ntree Number of trees.
#' @param mtry Number of variables to possibly split at in each node.
#' @param weights Numeric vector with weights between 0 and 1
#' @param \ldots further arguments to be passed to or from other methods especially
#'  - \code{test = data.frame}, to predict the values based on the previous data
#'  - \code{do.metrics = TRUE}, to list different metrics that were added to your model (error S E auc F1)
#'
#' @seealso [`ranger()`]
#' @seealso [`kspredict()`]
#' @seealso [`ksmetrics()`]
#' @references
#' Biological knowledge-slanted random forest approach
#' for the classification of calcified aortic valve stenosis
#' 10.1186/s13040-021-00269-4
#'
#'
#' @importFrom methods hasArg
#' @import ranger
#' @export
knowledge_slanted <- function(
    data=NULL,
    ntree,
    mtry,
    weights,
    ...
    ){

  # outcome variable check
  if(!("y" %in% names(data))){
    stop("Missing outcome as variable. Set outcome as variable as y")
  }
  # normalization
  if (sum(weights) != 1) weights <- weights / sum(weights)

  rf <- ranger::ranger(
      y ~ .,
      num.trees=ntree,
      mtry=mtry,
      dependent.variable.name=y,
      data = data,
      split.select.weights=weights,
      classification=TRUE,
      probability=TRUE,
      verbose =FALSE,
      ...
  )
  predictions <-  NULL
  performance <- NULL
  error_train <- rf$prediction.error
  slanted=list(
    "forest"=rf,
    "predictions"=predictions,
    "train_loss" = error_train,
    "test_loss"=performance
  )
  class(slanted) <- "kslanted"
  if(hasArg(test)){
    test <- list(...)$test
    slanted <- kspredict(slanted, data=test, ...)
  }
  return(slanted)
}
