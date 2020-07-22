#' asg
#' 
#' A function for the ff of the ASG Distribution
#' 
#' @param x explanatory variable
#' @param beta1 intercept in first half
#' @param beta2 intercept in second half
#' @param mu peak week
#' @param h peak
#' @param sigma1 variance in first half
#' @param sigma2 variance in second half
#'
#' @return A function
#'
#' @examples
#' asg(x=c(1:33), beta1=-4, beta2=-1, mu=15, h=10, sigma1=10, sigma2=15)
#'
#' @export



asg <- Vectorize(function(x, beta1, beta2, mu, h, sigma1, sigma2){
  top <- beta1 + (h)*exp(-((x - mu)^2)/(2*sigma1^2))
  bot <- beta2 + (h)*exp(-((x - mu)^2)/(2*sigma2^2))
  ifelse(x < mu,return(top),return(bot))
})