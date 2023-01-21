#' kspredict
#'
#' @name kspredict
#' @rdname kspredict
#' @aliases kspredict predict ksp
#'
#' @description Prediction with test data and a knowledge_slanted result.
#' @param ksobject A knowledge_slanted object.
#' @param data test data
#' @param do.metrics `bool` perform [`ksmetrics()`] if TRUE
#' @param \ldots further arguments passed to [`predict()`].
#'
#' @seealso [`knowledge_slanted()`]
#' @seealso [`ksmetrics()`]
#'
#' @import ranger
#' @importFrom methods is
#' @importFrom stats predict
#' @export
kspredict <- function(ksobject, data = NULL, do.metrics = FALSE, ...){
  if( is(ksobject, "kslanted") || is(ksobject, "kslanted_predict")){
    errors <-  FALSE
    auc <-  FALSE
    performance <- NULL
    ksobject$predictions = predict(ksobject$forest, data, type="response", ...)$predictions
    if(do.metrics) ksobject <- ksmetrics(ksobject, data, ksobject$predictions, ...)
    if(do.metrics){
      class(ksobject) <- "kslanted_full"
    }else{
      class(ksobject) <- "kslanted_predict"
    }
    return(ksobject)
  }else{
    stop("Please choose a knowledge_slanted result.\n")
  }
}
