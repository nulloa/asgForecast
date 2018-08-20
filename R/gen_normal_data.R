#' gen_normal_data
#' 
#' A function to generate simulated seasons
#' 
#' @param n.seas number of seasons to simulate
#' @param n.reg number of regions to simulate
#' @param weeks number of weeks in a season
#' @param mu vector of means to be sampled from (w/ replacement)
#' @param new.mu specific mean to be used in the last season
#' @param sd1 standard dev of data model
#' @param sd2 standard dev of the normal functional form
#' @param tau scaling factor (default=1)
#'
#' @return A data.frame
#'
#' @examples
#' tmpd=gen_normal_data(n.seas=2, n.reg=2, weeks=1:30, mu=c(11,17), sd1=0.015, sd2=3)
#' library(ggplot2)
#' ggplot(data=tmpd) + geom_point(aes(x=week, y=y)) + 
#' facet_grid(region~season) + geom_line(aes(x=week,y=meanfc))
#'
#' @export


gen_normal_data <- function(n.seas, n.reg, weeks, mu, new.mu=NULL, sd1, sd2, tau=1){
  
  n.weeks = length(weeks)
  simdata = NULL
  n.sims = n.seas*n.reg
  
  # Setup the means to be used in simulations
  if(is.null(new.mu)){
    if(length(mu)==1){
      sim.means=rep(mu, n.seas)
    }else{
      sim.means = sample(mu, n.seas, replace=TRUE)
    }
  }else{
    if(length(mu)==1){
      sim.means=c(rep(mu, (n.seas-1)), new.mu)
    }else{
      sim.means = c(sample(mu, (n.seas-1), replace=TRUE), new.mu)
    }
  }
  
  for(s in 1:n.seas){
    for(r in 1:n.reg){
      m = tau*dnorm(weeks, mean=sim.means[s], sd=sd2)
      simy = rnorm(weeks, mean=m, sd=sd1)
      simy[simy < 0] = 0
      
      tmp = data.frame(
        season = paste0("Season ", s),
        region = paste0("Region ", r),
        y = simy,
        week = weeks,
        meanfc = m,
        mu = sim.means[s]
        )
      simdata = rbind(simdata, tmp)
    }
  }
  return(simdata)
}
