#' get_mcmc_fit
#' 
#' A function which returns the fitted line based on mcmc est
#' 
#' @param mcmc.est estimated values given by get_mcmc_est
#' @param data data.frame with season, region, weeks(time) variable required 
#'
#' @return data.frame
#'
#' @examples
#' get_mle_fit(inits, data)
#'
#' @export

get_mcmc_fit <- function(mcmc.est, data){
  
  nseas <- length(unique(data$season))
  nreg <- length(unique(data$region))
  
  data$mcmc_fit <- NULL
  for(r in 1:nreg){
    for(s in 1:nseas){
      rowget = which(data$season == paste("Season", s) & data$region == paste("Region", r))
      est = mcmc.est[which(mcmc.est$season == paste("Season", s) & mcmc.est$region == paste("Region", r)),]
      data[rowget, "mcmc_fit"] = asg(x=data[rowget, "week"],
                                    beta1 = est$beta1,
                                    beta2 = est$beta2,
                                    mu = est$mu,
                                    h = est$h,
                                    sigma1 = est$sigma1,
                                    sigma2 = est$sigma2
      )
    }
  }
  return(data)
}
