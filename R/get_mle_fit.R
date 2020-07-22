#' get_mle_fit
#' 
#' A function which returns the fitted line based on mle params
#' 
#' @param inits initial values given by 
#' @param data data.frame with season, region, weeks(time) variable required 
#'
#' @return data.frame
#'
#' @examples
#' get_mle_fit(inits, data)
#'
#' @export

get_mle_fit <- function(inits, data){
  dims <- dim(inits)
  data$mle_fit <- NULL
  for(r in 1:dims[1]){
    for(s in 1:dims[2]){
      rowget = which(data$season == paste("Season", s) & data$region == paste("Region", r))
      data[rowget, "mle_fit"] = asg(x=data[rowget, "week"],
                                    beta1 = inits[r,s,1],
                                    beta2 = inits[r,s,2],
                                    mu = exp(inits[r,s,4]),
                                    h = exp(inits[r,s,3]),
                                    sigma1 = exp(inits[r,s,5]),
                                    sigma2 = exp(inits[r,s,6])
      )
    }
  }
  return(data)
}
