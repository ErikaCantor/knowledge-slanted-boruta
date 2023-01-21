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
  nfeatures=ncol(data) - 1 # minus target variable
  ##Initiate state
  features <- data.frame(hits=features) %>%
    mutate(status=rep("Tentative",nfeatures))

  accepted<-stats::p.adjust(
    stats::pbinom(features$hits-1, iter, prob.accept, lower.tail=FALSE),
    method=method) < p_value

  rejected<-stats::p.adjust(
    stats::pbinom(features$hits, iter, prob.reject, lower.tail=TRUE),
    method=method) < p_value

  features$status[accepted] <- "Accepted"
  features$status[rejected] <- "Rejected"
  features$rowids <- rownames(features)
  features$varnames <- colnames(data %>% select(-y))
  if(trace>0){
    nAcc <- length(features$varnames[accepted])
    nRej <- length(features$varnames[rejected])
    nLeft <- length(nfeatures - (nAcc + nRej))
    message(sprintf(">Confirmed %s attribute%s: [%s]",
                    nAcc,ifelse(nAcc==1,'','s'),stringify(features$varnames[accepted])))
    message(sprintf(">Rejected %s attribute%s: [%s]",
                    nRej,ifelse(nAcc==1,'','s'),stringify(features$varnames[rejected])))
    message(sprintf(">Undetermined %s attribute%s", nLeft, ifelse(nAcc==1,'','s')))
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
    runs=20, trace=0, ...) {

  #Timer starts... now!
  timeStart<-Sys.time()

  #Convert x into a data.frame
  if(!is.data.frame(data))
    data<-data.frame(data)

  if(runs < 20)
    stop('maxRuns must be greater than 20.')

  options(future.globals.maxSize = +Inf) # improve performance
  results <- future.apply::future_lapply(
    seq_len(runs-1),
    function(i) {
      if(trace == 2) message(sprintf('iter [%s]', i))
      do.call(
        ksboruta,
        c(
          list(
            data = data, ntree = ntree,
            mtry = mtry, weights = weights,
            trace = trace
          ),
          list(...)
        )
      )
    },
    future.seed = TRUE,
    future.conditions = "message",
    future.globals = FALSE,
    future.stdout = TRUE
  )
  hits=Reduce("+", results) # FIXME: improve with fold from future.apply
  # Notifying user of our progress
  if(trace>1)
    message(sprintf('main loop start, iter [%s] ...', runs))
  ## Iterative state
  if(trace>1)
    message(sprintf('iter [%s]',runs))

  history <- list()
  features <- feature_extraction(
    hits,
    iter= runs,
    data=data,
    method=method,
    p_value=p_value,
    trace = trace
  )

  features <- features[features$status!="Rejected",]
  vars <- append("y", features$varnames)
  rowids <- features$rowids

  nwdata <- data %>% select(as.factor(vars))
  nwweights <- weights[as.factor(rowids)]
  nwhits <- hits

  # HACK: llamar boruta: iteracion run + 1 to stop condition
  while(any(features$status=="Tentative")
        && (runs+1-> runs) < iter + 1){ ### max iter == 100 o no hay tentativas (todas aceptadas o rechazadas)
    if(trace)
      message(sprintf('iter [%s]',runs))

    nwresult <- ksboruta(
      data = nwdata,
      ntree = ntree,
      mtry = sqrt(length(nwdata)),
      weights = nwweights
    )
    results <- append(results, nwresult)
    nwhits <- Reduce("+", results)

    features <- feature_extraction(
      nwhits[as.factor(rowids)],
      iter=runs,
      data=nwdata,
      method=method,
      p_value=p_value,
      trace = trace
    )
    features <- features[features$status!="Rejected",]
    vars <- append("y", features$varnames)
    rowids <- features$rowids
    nwdata <- nwdata %>% select(one_of(vars))
    nwweights <- weights[as.factor(rowids)]
  }
  timeTaken=Sys.time()-timeStart
  if(trace>0){
    cat(
      paste("performed ",runs," iterations in ",timeTaken," secs.\n")
    )
    cat(
      paste(">", length(nwdata),"attributes confirmed important: ",
            stringify(colnames(nwdata)),"\n")
    )
    unimportant <- data[!(data %in% nwdata)]
    cat(
      paste(">", length(unimportant),"attributes unimportant: ",
            stringify(colnames(unimportant)),"\n")
    )
  }
  return(nwdata)
}
