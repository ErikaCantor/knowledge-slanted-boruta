#' ksmetrics
#'
#' @name ksmetrics
#' @rdname ksmetrics
#' @aliases ksmetrics metrics
#' @description Compute Area Under the Curve (AUC).
#' ROC is a probability curve and AUC represents the degree or measure of separability
#'
#' @param ksobject A knowledge_slanted object.
#' @param data test data
#' @param predictions aggregated predictions for all trees
#' @param \ldots further arguments passed to [`pROC::roc()`].
#'
#' @seealso [`knowledge_slanted()`]
#' @seealso [`kspredict()`]
#'
#' @import dplyr
#' @import pROC
#' @export
ksmetrics <- function(ksobject, data=NULL, predictions, ...){
  if( class(ksobject) == "kslanted" || class(ksobject) == "kslanted_predict" ){
    if(length(unique(data$y))==1){
      auc=NaN
    } else{
      auc=pROC::roc(data$y, predictions[,2], direction="<", ...)$auc
    }
    error.test= data.frame(
      observados=data$y,
      slanted=colnames(predictions)[apply(predictions[,1:2], 1, which.max)]
    ) %>%
      summarise(
        error=1-sum(observados==slanted)/nrow(.), S=(sum(slanted[observados==1]==1)/sum(observados==1)),
        E=(sum(slanted[observados==0]==0)/sum(observados==0)),
        auc=auc,
        F1=sum(slanted[observados==1]==1)/(sum(slanted[observados==1]==1)+0.5*(sum(slanted[observados==0]==1)+sum(slanted[observados==1]==0)))
      )
    slanted=list(
      "forest"=ksobject$forest,
      "predictions"=predictions,
      "train_loss"=ksobject$train_loss,
      "test_loss"=error.test
    )
    if( is(ksobject,"kslanted_predict")) class(slanted) <- "kslanted_full"
    return(slanted)
  }else{
    cat("Please choose a knowledge_slanted result.\n")
  }
}

