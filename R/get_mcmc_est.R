#' get_mcmc_est
#' 
#' grabs the fits from all sesons
#'
#' @param mcmc.fit mcmc fit to grab the seasons
#' @param funct Either mean or median (default="mean")
#'
#' @return data.frame
#'
#' @examples
#' 
#'
#' @export

get_mcmc_est <- function(mcmc.fit, funct="mean"){
  
  res = rstan::extract(mcmc.fit)
  
  # str res[iter,region,season,parameter]
  # parameter = 1:6 = (beta1, beta2, nu/h, mu, log(sigma1), log(sigma2))
  res = res$ctheta
  n.reg = dim(res)[2]
  n.seas = dim(res)[3]
  
  fits <- NULL
  
  for(r in 1:n.reg){
    for(s in 1:n.seas){
      
      if(funct=="mean"){
        tmpd = data.frame(
          season = paste("Season", s),
          region = paste("Region", r),
          mu=mean(exp(res[, r, s, 4])), 
          h=mean(res[, r, s, 3]),
          beta1=mean(res[, r, s, 1]), 
          beta2=mean(res[, r, s, 2]), 
          sigma1=mean(exp(res[, r, s, 5])), 
          sigma2=mean(exp(res[, r, s, 6]))
        )
        fits <- rbind(fits, tmpd)
      }else{
        tmpd = data.frame(
          season = paste("Season", s),
          region = paste("Region", r),
          mu=median(exp(res[, r, s, 4])), 
          h=median(res[, r, s, 3]),
          beta1=median(res[, r, s, 1]), 
          beta2=median(res[, r, s, 2]), 
          sigma1=median(exp(res[, r, s, 5])), 
          sigma2=median(exp(res[, r, s, 6]))
        )
        fits <- rbind(fits, tmpd)
      }
      
    }
  }
  
  return(fits)
}

