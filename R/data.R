#' Pull trait simulated and test data
#' This function read trait data
#'
#'
#' @param name the dataset to source.
#' @return a raw dataset available in R
#' @family simulated_data
#' @export
#' @author Erika Cantor
#' @examples
#'
#' # to get a list of all available data within the package
#' pulldata()
#'
#' # to import a dataset called _simulated1_
#'
pulldata <- function(name = NULL) {
  libpath <- system.file( "data", package="kslboruta")
  available <- sub(".RData", "", dir(sub("", "", libpath)))

  if(is.null(name) || !name %in% available) {
    cat("Please choose a trait dataset to import! the following trait datasets are available: \n \t")
    cat(paste(available, collapse = "\n \t"))
    cat("\n")
  }else{
    tryCatch(
      source(system.file( "data", paste0(name,".RData"), package="kslboruta")),
        warning = function(war) {
          message("Direct call to data source failed. Please reinstall package and re-load data!")
        },
        error = function(err) {
          message("Direct call to data source failed. Please reinstall package and re-load data!")
        } )

    if(exists(name))  message(paste0("The dataset '", name, "' has successfully been downloaded! \n" ) )
  }
}
#'
#'
#' @title Simulated train-test data set
#' @name simulated1_raw
#'
#' @description A dataset containing train and test simulated values
#'
#' @author Erika Cantor
#'
#' @format A large list
#' \describe{
#'   \item{train}{dataframe with 27 obs. of 1001 variables}
#'   \item{test}{dataframe with 12 obs. of 1001 variables}
#' }
#'
#' @usage pulldata(simulated1)
#' @examples pulldata(simulated1)
#' @family simulated_data
#' @source \url{https://github.com/ErikaCantor/knowledge-slanted-boruta/data/simulated1.RData}
NULL
#' @title Simulated knowledge_slanted
#'
#' @name simulated2_raw
#'
#' @description Small simulated data sets where RF Approach can be performed
#'
#' @author Erika Cantor
#'
#' @format A large list
#' \describe{
#'   \item{train}{dataframe with 30 obs. of 5001 variables}
#'   \item{seed}{numeric array with seed values}
#'   \item{weights}{numeric array with seed values}
#'   \item{beta}{numeric array with seed values}
#'   \item{id.p}{numeric array with seed values}
#' }
#'
#' @usage pulldata(simulated2)
#' @examples pulldata(simulated2)
#' @family simulated_data
#' @source \url{https://github.com/ErikaCantor/knowledge-slanted-boruta/data/simulated2.RData}
NULL
