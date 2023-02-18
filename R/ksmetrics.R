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
#' @importFrom methods is
#' @noRd
ksmetrics <- function(ksobject, data=NULL, predictions, ...){
  if( is(ksobject, "kslanted")  || is(ksobject, "kslanted_predict")){
    if(length(unique(data$y))==1){
      auc=NaN
    } else{
      auc=pROC::roc(data$y, predictions[,2], direction="<", quiet = TRUE, ...)$auc
    }
    error.test= data.frame(
      values=data$y,
      slanted=colnames(predictions)[apply(predictions[,1:2], 1, which.max)]
    ) %>%
      summarise(
        error=1-sum(values==slanted)/nrow(.), S=(sum(slanted[values==1]==1)/sum(values==1)),
        E=(sum(slanted[values==0]==0)/sum(values==0)),
        auc=auc,
        F1=sum(slanted[values==1]==1)/(sum(slanted[values==1]==1)+0.5*(sum(slanted[values==0]==1)+sum(slanted[values==1]==0)))
      )
    ksobject$test_loss <- error.test
    if( is(ksobject,"kslanted_predict")) class(ksobject) <- "kslanted_full"
    return(ksobject)
  }else{
    cat("Please choose a knowledge_slanted result.\n")
  }
}

