#' asg_mle
#' 
#' A function which finds and formats the mle of ASG ff
#' 
#' @param y response variable which follows binomial dist
#' @param x explanatory variable
#' @param count n in binomial dist
#' @param group groups of response
#' @param season seasons in response
#' @param inits initial values for optim (defualt=NULL)
#' @param ll likelihood to be optimized (defualt=ASG)
#'
#' @return A function
#'
#' @examples
#' asg_mle(y, x, num, group, seas)
#'
#' @export


asg_mle <- function(y, x, sd, group, season, inits=NULL, ll=NULL, correction=TRUE){
  
  log.lik <- function(y, sd, x, par){
    mu <- asg(x, boot::inv.logit(par[1]), boot::inv.logit(par[2]), par[3], boot::inv.logit(par[4]), par[5], par[6])
    ll <- sum( ((y-mu)^2) / (2*sd^2) )
    if(par[3] <= 0 | par[5] <= 0 | par[6] <= 0){ll <- -Inf}
    return(ll)
  }
  
  if(is.null(ll)){ll <- log.lik}
  if(is.null(inits)){inits <- c(-10, -10, 15, 0, 8, 8)}
  
  df <- data.frame(y=y, x=x, sd=sd, group=group, season=season)
  opt <- array(NA, dim=c(length(unique(df$group)), length(unique(df$season)), 6))
  
  for(s in 1:length(unique(df$season))){
    for(g in 1:length(unique(df$group))){
      dat <- subset(df, season==paste(unique(df$season)[s], sep="") & group==paste(unique(df$group)[g], sep=""))
      opts <- optim(par=inits, ll, y=dat$y, x=dat$x, sd=dat$sd)
      newopts <-  c(opts$par[1], opts$par[2], opts$par[4], log(opts$par[3]), log(opts$par[5]), log(opts$par[6]))
      if(correction==TRUE & g >= 2){
        whichoff <- abs(newopts - opt[unique(df$group)[g-1], unique(df$season)[s], ]) > 500
        newopts[whichoff] <- opt[unique(df$group)[g-1], unique(df$season)[s], ][whichoff]
      }
      
      opt[unique(df$group)[g], unique(df$season)[s], ] <- newopts
    }
  }
  
  return(opt)
}

