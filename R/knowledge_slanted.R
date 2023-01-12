#' knowledge_slanted
#'
#' @name knowledge_slanted
#' @rdname knowledge_slanted
#' @aliases knowledge_slanted slanted ks
#'
#' @param train Training data of class \code{data.frame}, \code{matrix}
#' @param ntree Number of trees.
#' @param mtry Number of variables to possibly split at in each node.
#' @param weights Numeric vector with weights between 0 and 1
#' @param \ldots further arguments passed to [`ranger()`].
#'
#' @seealso [`ranger()`]
#'
#' @import ranger
#' @export
knowledge_slanted <- function(train, ntree, mtry, weights, ...){

  rf <- ranger::ranger(y ~ .,
                       num.trees=ntree,
                       mtry=mtry,
                       dependent.variable.name=y,
                       data = train,
                       split.select.weights=weights,
                       seed=23,
                       classification=TRUE,
                       probability=TRUE,
                       ...
  )
  errors <-  FALSE
  auc <-  FALSE
  performance <- NULL
  if(hasArg(errors)) errors <- TRUE
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
    slanted <- kspredict(slanted, data=test, do.metrics=TRUE, ...)
  }
  return(slanted)
}
