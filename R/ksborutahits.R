#' stringify
#'
#' Join all items in a vector into a string, using a collate character as separator:
#' @param arr an iterable vector
#' @param limit quantity of elements into string before inlcude 'more'
#' @param collapse string separator
#' @noRd
stringify<-function(arr, limit=8, collapse=", "){
  arr<-sort(arr)
  if(length(arr) < limit + 1)
    return(sprintf("%s",paste(arr, collapse=collapse)))
  sprintf("%s and %s more",paste(utils::head(arr, limit), collapse=collapse), length(arr) - limit)
}
#' feature_extraction
#'
#' Comprehensive toolkit for generating various numerical features of protein sequences
#' @param features a vector of numbers with equal size than data cols
#' @param iter the number of trials.
#' @param data `data.frame` a dataset, columns are features
#' @param method adjustment method for [stats::p.adjust]
#' @param p_value confidence level. Default values could be used.
#' @param prob.accept: the probability of success of each trial for accept
#' @param prob.reject: the probability of success of each trial for reject
#' @importFrom stats p.adjust
#' @importFrom stats pbinom
#' @noRd
feature_extraction <- function(features, iter, data=NULL,
                               method="bonferroni", p_value=0.05,
                               prob.accept=0.5,
                               prob.reject=0.5,
                               trace=0, ...) {
  # TODO: empieza el bloque de caracteristicas paso 6
  # features=data.frame(hits=Reduce("+", r))
  if(length(features) >= length(data)){
    stop("size of all indexes in features vector is greater than the number of the colunmns in data")
  }
  accepted<-stats::p.adjust(
    stats::pbinom(features$hits-1, iter, prob.accept, lower.tail=FALSE),
    method=method) < p_value
  (features$status=="Tentative" & accepted) -> accepted

  rejected<-stats::p.adjust(
    stats::pbinom(features$hits, iter, prob.reject, lower.tail=TRUE),
    method=method) < p_value
  (features$status=="Tentative" & rejected) -> rejected

  features$status[accepted] <- "Accepted"
  features$status[rejected] <- "Rejected"
  features$rowids <- rownames(features)
  features$varnames <- colnames(data %>% select(-y))
  total <-  length(features$status)
  features <- features %>% na.omit()
  if(trace>0){
    nAcc <- length(features$status[features$status=="Accepted"])
    nLeft <- length(features$status[features$status=="Tentative"])
    nRej <- total - (nAcc + nLeft)
    message(
      sprintf("[%s] %s Accepted, %s Rejected, %s Tentative of %s vars",
              Sys.time(),nAcc, nRej, nLeft, total))
  }
  features
}
#' ksborutahits
#'
#' ksborutahits to  replicate _ksboruta_ method
#'
#' @inheritParams ksboruta
#' @param iter maximum number of iterations
#' @param method adjust method to tune [`stats::p.adjust()`]
#' @param p_value `number` probability of obtaining test results
#' @param trace verbose. 0 means no tracing, 1 means decision about each attribute, 2 means the same as 1, plus reporting step by step, 3 show all and var history
#' @param runs minimum number of iterations. Default value should be used.
#' @param \ldots further arguments passed to `ksboruta`
#'
#' @import future
#' @import future.apply
#'
#' @examples
#' # basic usage of ksborutahits
#' data <- simulated2$train
#' mtry <- sqrt(length(data))
#' weights <- simulated2$weights
#' ksborutahits(iter=50, data=data, ntree = 100, mtry = mtry, weights = weights, trace=TRUE)
#'
#' @export
ksborutahits <- function(
    iter=100,
    method="bonferroni",
    p_value=0.05,
    data=NULL,
    ntree, mtry, weights,
    runs=20, trace=3, ...) {

  #Timer starts... now!
  timeStart<-Sys.time()

  #Convert x into a data.frame
  if(!is.data.frame(data))
    data<-data.frame(data)

  if(runs < 20)
    stop('maxRuns must be greater than 20.')

  if(trace>0){
    message(sprintf("[%s] start", timeStart))
  }

  options(future.globals.maxSize = +Inf) # improve performance
  results <- future.apply::future_lapply(
    seq_len(runs),
    function(i) {
      # if(trace > 1) message(sprintf('iter [%s]', i))
      do.call(
        ksboruta,
        c(
          list(
            data = data, ntree = ntree,
            mtry = mtry, weights = weights,
            trace = trace
          )
        )
      )
    },
    future.seed = TRUE,
    future.conditions = "message",
    future.globals = FALSE,
    future.stdout = FALSE
  )
  hits=Reduce("+", results) # FIXME: improve with fold from future.apply
  ##Initiate state
  nfeatures=ncol(data) - 1 # minus target variable
  features <- data.frame(hits=hits) %>%
    mutate(status=rep("Tentative",nfeatures))
  #Resultados de 20 variables
  features <- feature_extraction(
    features,
    iter= runs,
    data=data,
    method=method,
    p_value=p_value,
    trace = trace
  )
  # Notifying user of our progress
  if(trace>1){
    history <- names(data %>% select(-c('y')))
    history <- as.data.frame(history)
    history[,dim(history)[2] + 1] <- features$status
    message(sprintf('[%s] add i%s to history', Sys.time(), length(history) -1 ))
  }
  # Seleccionar solo variables en competencia
  nwfeatures <- features[features$status!="Rejected",]
  vars <- append("y", nwfeatures$varnames)
  rowids <- nwfeatures$rowids
  # Nuevo dataset y nuevos pesos
  nwdata <- data %>% select(as.factor(vars))
  nwweights <- cbind(weights[as.integer(rowids)])
  nwhits <- hits

  if(trace>1){
    timeTaken=Sys.time()-timeStart
    message(sprintf('[%s] block of %s iterations is completed ...', Sys.time(), runs))
    message(sprintf('[%s] go to loop, runtime %s ...', Sys.time(), timeTaken))
  }
  # HACK: llamar boruta: iteracion run + 1 to stop condition
  while(
    "Tentative" %in% nwfeatures$status
    && (runs + 1-> runs) < iter
    ){ ### max iter == 100 o no hay tentativas (todas aceptadas o rechazadas)
    if(trace>0){
      message(sprintf('[%s] iteration %s', Sys.time(), runs))
    }
    nwresult <- ksboruta(
      data = nwdata,
      ntree = ntree,
      mtry = sqrt(length(nwdata)),
      weights = nwweights
    )
    temp <- rep(NA, length(data) - 1)
    temp[as.integer(rowids)] <- nwresult
    # nwresult <- append(rep(NA, length(data)-length(nwdata)), nwresult)
    results <- append(list(temp), results)
    nwhits=Reduce("+", results) # FIXME: improve with fold from future.apply
    features$hits <- nwhits
    nwfeatures <- feature_extraction(
      features[as.integer(rowids),],
      iter=runs,
      data=nwdata,
      method=method,
      p_value=p_value,
      trace = trace
    )
    features[as.integer(rowids),] <- nwfeatures
    if(trace>2){
      message(
        sprintf('[%s] until now i%s has been rejected', Sys.time(),
                length(features$status[features$status=="Rejected"])))
    }
    if(trace>1){
      history[,dim(history)[2] + 1] <- features$status
      message(sprintf('[%s] add i%s to history', Sys.time(), length(history)-1))
    }
    nwfeatures <- features[features$status!="Rejected",]
    vars <- append("y", nwfeatures$varnames)
    rowids <- nwfeatures$rowids
    nwdata <- nwdata %>% select(one_of(vars))
    nwweights <- weights[as.integer(rowids)]
  }
  timeTaken=Sys.time()-timeStart

  if(trace>0){
    message(sprintf('[%s] elapsed time %s', Sys.time(), timeTaken))
  }
  if(trace>2){
    histvalues <- history %>% select(-c('history'))
    colnames(history) <- c("history", paste("i", 1:ncol(histvalues),sep=""))
    history$decision <- rep("Rejected", nrow(history))
    history$decision[as.integer(rowids)] <- "Accepted"
    print(history)
  }
  return(list(data=nwdata, history=history))
}

