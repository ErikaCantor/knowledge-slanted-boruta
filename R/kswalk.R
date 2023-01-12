#' random walk prior
#' @description
#' 'rw_prior' perform random walk with restart
#'
#' @param data `data.frame`
#' @param seednodes `data.frame`
#' @return `random walk` object
#'
#' @seealso
#' [`random.walk`]
#'
#'
#' @import diffusr
#' @export
rw_prior<-function(data, graph, seednodes, ...){
  # number of genes or nodes
  p <- NULL
  if(hasArg(p)) p <- list(...)$p
  else p <- ncol(data)-1
  if( p > ncol(data)) stop(paste("p must be a number between 1 and", ncol(data)))

  # Set initial probability in all nodes
  p0 <- rep(0.00001, p)

  #Set initial probability equal to 1 for seed nodes
  nodes = setdiff(seq(1, p), seednodes)
  qtynodes=round(length(seednodes)*alfa)

  if(length(nodes)==0) newnodes=sample(seednodes, qtynodes)
  else newnodes=c(
    sample(seednodes, qtynodes),
    sample(nodes, length(seednodes)-qtynodes)
  )
  p0[nodes]<-1

  #computation of stationary distribution
  aseed <- 23
  r=0.3
  niter = 10000
  thresh = 1e-06
  if(hasArg(seed)) aseed <- list(...)$aseed
  set.seed(aseed)
  if(hasArg(r)) r <- list(...)$r
  if(hasArg(niter)) niter <- list(...)$niter
  if(hasArg(thresh)) thresh <- list(...)$thresh

  ## Probability of each node is a candidate gene.
  ## Candidate generated from the random walk proposal dist.
  candidates  <- diffusr::random.walk(p0, graph,r=r, niter = niter, thresh = thresh)
  class(candidates) <- "rw_priorized"
  return(candidates)
}




