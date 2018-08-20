#' get_priors
#' 
#' Use mcmc objects from \code{\link[functform]{drake_mcmc}} to get priors for importance sampling
#'
#' @param mcmc.obj mcmc object
#'
#' @return list
#'
#' @examples
#' 
#' 
#' 
#'
#' @export


get_priors <- function(mcmc.obj){
  mcmc <- extract(mcmc.obj)
  prior.values <- list()
  
  return(prior.values)
}
