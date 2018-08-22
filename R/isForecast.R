#' isForecast
#' 
#' Preforms both types of importance sampling forecasting
#'
#' @param y data from forecast season
#' @param n.weeks number of points from the forecast season to include
#' @param seas.fits matrix season fits to be used in sampling (see \code{\link{get_season_fit}})
#' @param priors priors from \code{\link{get_priors}} or vector of means
#' @param prior.type type of prior to use (default is "Unif)
#' @param n.iter number of iterations to run
#' @param tau scaling factor for est
#' @param sigma sd used in generating weights
#' @param samp_type Type of sampling procedure to be preformed
#'
#' @seealso \url{http://www.r-project.org}
#'
#' @return list
#'
#' @examples
#' 
#'
#' @export


isForecast <- function(y, n.weeks, seas.fits, priors, prior.type="Unif", n.iter, tau, sigma, samp_type="Transformation"){
  
  forecast <- weight <- weights <- NULL
  n <- length(y)
  n.fits <- nrow(seas.fits)
  cur.dat <- y[1:n.weeks]
  
  for(i in 1:n.iter){
    # Draw mu
    if(prior.type!="Unif"){
      samp.mu <- rnorm(n=1, mean=priors[1], sd=priors[2])
    }else{
      samp.mu <- runif(n=1, priors[1], priors[2]) 
    }
    
    # Draw a past forecast
    samp.fit.id <- sample(1:n.fits, size=1)
    
    # Get fit of given past forecast
    samp.fit <- boot::inv.logit(tau*asg(x=(1:30), mu=seas.fits[samp.fit.id,]$mu, h=seas.fits[samp.fit.id,]$h,
                                        beta1=seas.fits[samp.fit.id,]$beta1, beta2=seas.fits[samp.fit.id,]$beta2,
                                        sigma1=seas.fits[samp.fit.id,]$sigma1, sigma2=seas.fits[samp.fit.id,]$sigma2))
    
    if(samp_type=="Transformation"){
      samp.fit <- boot::inv.logit(tau*asg(x=(1:30 - samp.mu + which.max(samp.fit)), 
                                          mu=seas.fits[samp.fit.id,]$mu, h=seas.fits[samp.fit.id,]$h,
                                          beta1=seas.fits[samp.fit.id,]$beta1, beta2=seas.fits[samp.fit.id,]$beta2,
                                          sigma1=seas.fits[samp.fit.id,]$sigma1, sigma2=seas.fits[samp.fit.id,]$sigma2))
    }
    
    # Get weights
    for(j in 1:n.weeks){
      weights[j] = dnorm(y[j], mean=samp.fit[j], sd=sigma)
    }
    weight = prod(weights)
    
    tmp <- c(cur.dat, samp.fit[-c(1:n.weeks)], weight, samp.mu, samp.fit.id, i)
    forecast <- rbind(forecast, tmp)
  }
  
  row.names(forecast) <- NULL
  forecast <- data.frame(forecast)
  names(forecast) <- c(paste(1:30), "weight", "samp.mu", "samp.fit", "iteration")
  
  return(forecast)
}
