#' kspredict
#'
#' @name kspredict
#' @rdname kspredict
#' @aliases kspredict predict ksp
#'
#' @description Prediction with test data and a knowledge_slanted result.
#' @param ksobject A knowledge_slanted object.
#' @param data test data
#' @param \ldots further arguments passed to [`predict()`].
#'
#' @seealso [`knowledge_slanted()`]
#' @seealso [`ksmetrics()`]
#'
#' @import ranger
#' @export
kspredict <- function(ksobject, data = NULL, do.metrics = FALSE, ...){

  if( !is(ksobject, "kslanted") ){
    cat("Please choose a knowledge_slanted result.\n")
  }else{
    errors <-  FALSE
    auc <-  FALSE
    performance <- NULL
    predictions=predict(ksobject$forest, data, type="response", ...)$predictions
    if(do.metrics) performance <- ksmetrics(ksobject, data, predictions, ...)
    slanted=list(
      "forest"=ksobject$forest,
      "predictions"=predictions,
      "train_loss"=ksobject$train_loss,
      "test_loss"=performance
    )
    if(do.metrics){
      class(slanted) <- "kslanted_full"
    }else{
      class(slanted) <- "kslanted_predict"
    }
    return(slanted)
  }
}
