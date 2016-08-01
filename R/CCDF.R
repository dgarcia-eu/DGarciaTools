#' Cumulative Complementary Density Function (CCDF)
#'
#' This function computes the CCDF of the values in x, which is P(x>v),
#' i.e. the probability of finding a value strictly larger than v in the input vector x.
#' @param x numeric vector to calculate CCDF of
#' @param include.min whether to include a first point z=min(x)-1, which will have P(x>z)=1
#' @return 3 row matrix with a first row with the values of the vector v, the second one with
#' P(x>v) and the third one with the absolute count N of x>v.
#' @keywords density, power-law
#' @export
#' @examples
#' xs <- floor(runif(min=0,max=10,n=100))
#' CCDF(xs)
#' 
CCDF <- function(x, include.min=FALSE) {
  xvs <- sort(unique(x))  #sequence of possible values in x

  if (include.min)  
  {  xvs <- c(min(x)-1,xvs)  }
  
  CDvec <- sapply(xvs, FUN= function(y) {sum(x>y)}) # we count strictly larger entries
  total <- length(x)
  
  rbind(v=xvs, P=CDvec/total, N=CDvec)
  
  }