#' Plot a Cumulative Complementary Density Function (CCDF)
#'
#' This function calculates and plots a CCDF with logarithmic axes by default
#' @param x numeric vector to calculate CCDF of
#' @param include.min whether to include a first point z=min(x)-1, which will have P(x>z)=1 (default FALSE)
#' @param counts whether to plot absolute counts instead of probability (default FALSE)
#' @param log character string determining the axes to display logarithmically (default "xy", both axes)
#' @param type of plot (default line "l")
#' @param xlab label of x axis (default "x") 
#' @param ylab label of y axis (default "P(X>x)")
#' @param cex.lab size of the axis label (default 1.3)
#' @param cex.axis size of the axis values (default 1.3)
#' @param lwd width of the plot line (default 2)
#' @param ... additional parameters for plot function
#' @keywords density, power-law
#' @export
#' @examples
#' xs <- floor(runif(min=0,max=10,n=100))
#' plot.CCDF(xs)
#' 
plot.CCDF <- function(x, include.min=FALSE, counts=FALSE, log="xy", type="l", xlab="x", ylab="P(X>x)", cex.lab=1.3, cex.axis=1.3, lwd=2, ...) {
  CD <- CCDF(x, include.min)
  P <- CD["P",]
  v <- CD["v",]
  f <- TRUE
  if (counts)
    { P <- CD["N",] }
  if (log != "")
    { f <- P>0 & v > 0 }
  par(mar=c(4.5,4.5,0.5,0.5))
  options(scipen=3)
  plot(x=v[f], y=P[f], log=log, xlab=xlab, ylab=ylab, type=type, cex.lab=cex.lab, cex.axis=cex.axis, lwd=lwd, ...)
  }
  
