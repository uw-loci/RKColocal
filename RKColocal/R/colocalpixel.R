#' @title Spatial Adaptive Colocalization Analysis
#'
#' @description \code{colocalpixel} is used to analyze colocalization locally and identify correlated region
#'in dual-channel image. 
#' 
#' @details 
#' This function is based on new spatial adaptive colocalization analysis framework, which is able to quantify the degree of colocalization at each pixel level.
#' 
#' @seealso \code{\link{HeatMapZ}}
#' 
#' @family spatial colocalization analysis
#' @import rJava
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' 
#' @param X A numerical matrix, the intensity matrix of the first channel/red channel. 
#' @param Y A numerical matrix, the intensity matrix of the first channel/green channel.
#' @param alpha An numerical value, the type I error in multiple testing.
#' @param Thresholds, A vector of length 2, the input threshold for each channel when Manders' split colocalization coefficients and maximum truncated Kendall tau correlation coefficient are evaluated.
#' @param method A string, indicating which multiple testing is used. Now only \code{'bonferroni'} is supported.
#'
#' @return \code{colocalpixel} returns an object of class \code{colocalpixel}. 
#'  \code{\link{plot}} can be used to plot image with identification result.
#'  \code{\link{HeatMapZ}} can be used to plot heat map of Z values.
#'  
#' An object of class \code{colocalpixel} is a list containing following components:
#' \itemize{
#' \item \code{Zvalue} a numerical matrix to store the Z values of each pixel
#' \item \code{Pvalue} a numerical matrix to store the P values of each pixel
#' \item \code{SigPixel} a binary matrix to store the indicator of significance of each pixel
#' \item \code{Red} the original Red channel
#' \item \code{Green} the original green channel
#' }
#' 
#' @export
#' 
#' @examples 
#' X=matrix(runif(32*32),32,32)
#' Y=matrix(runif(32*32),32,32)
#' colocalpixel(X,Y)
#' 
#' @author Shulei Wang

colocalpixel <- function( X, Y, alpha = 0.05, Thresholds=NULL, method = 'bonferroni' ) {
  if (!is.matrix(X) ||!is.numeric(X))
    stop("X must be numeric matrix")
  if (!is.matrix(Y) ||!is.numeric(Y))
    stop("Y must be numeric matrix")
  if (nrow(X) != nrow(Y) || ncol(X) != ncol(Y))
    stop("size of X and Y doesn't match")
  if (max(X)>1 || min(X)<0)
    stop("the range of X must be [0,1]")
  if (max(Y)>1 || min(Y)<0)
    stop("the range of Y must be [0,1]")
  if (is.null(Thresholds))
  {
    ThresholdX = otsu(X)
    ThresholdY = otsu(Y)
  } else {
    if (!is.vector(Thresholds) ||!is.numeric(Thresholds))
      stop("Thresholds must be numeric vector")
    if (length(Thresholds)<2)
      stop("At least two elements in Thresholds")
    ThresholdX <- Thresholds[1]
    ThresholdY <- Thresholds[2]
  }
  
  
  ch1 <- .jarray(X,dispatch=TRUE)
  ch2 <- .jarray(Y,dispatch=TRUE)

  
  javaad <- .jnew("AdaptiveSmoothedKendallTau",ch1,ch2,ThresholdX,ThresholdY,check=TRUE,silent=FALSE)
  Re<-.jcall(javaad,"[[D","execute")
  # Re<-javaad$execute()
  mat=sapply(Re,.jevalArray)
  mat[is.na(mat)]=0
  mat[is.infinite(mat)]=0
  
  Zvalue = t(mat)
  Pvalue = pnorm(Zvalue, lower.tail = FALSE)
  
  if(method == 'bonferroni')
  {
    Thre = qnorm(alpha/nrow(X)/ncol(X) , lower.tail = FALSE)
    SigPixel = Zvalue
    SigPixel[Zvalue<=Thre] = 0
    SigPixel[Zvalue>Thre] = 1
  }
  else {
    warning("method should be one of \"bonferroni\"")
    Thre = qnorm(alpha/nrow(X)/ncol(X) , lower.tail = FALSE)
    SigPixel = Zvalue
    SigPixel[Zvalue<=Thre] = 0
    SigPixel[Zvalue>Thre] = 1
  }
  
  R=list(Zvalue = Zvalue, Pvalue = Pvalue, SigPixel = SigPixel, Red = X, Green = Y)
  class(R) <- "colocalpixel"
  return(R)
}

#' @title Plot Identification Result for an \code{colocalpixel} Object
#' 
#' @description This function is used to display the dual-channel image in \code{colocalpixel} with identification result.
#' 
#' @details The significance result are labelled in blue color 
#' 
#' @importFrom EBImage Image
#' @importFrom EBImage display
#' 
#' @param x an object of class \code{\link{colocalpixel}} 
#' @param ... Arguments to be passed to methods.
#' 
#' @return Invisible \code{NULL}
#' 
#' @examples  
#' X=matrix(runif(32*32),32,32)
#' Y=matrix(runif(32*32),32,32)
#' co <- colocalpixel(X,Y)
#' plot(co)  
#' 
#' @family spatial colocalization analysis
#' @seealso \code{\link{colocalpixel}} 
#' 
#' @export
#' 
#' @author Shulei Wang
plot.colocalpixel <- function( x,... )
{
  R = x$Red
  G = x$Green
  R = R / max(R)
  G = G / max(G)
  SigPixel = x$SigPixel
  M<-Image(abind(R,G,SigPixel,along = 3),colormode="Color")
  display(M)
}

#' @title Heat map of Z value for an \code{colocalpixel} Object
#' 
#' @description \code{HeatMapZ} is used to display the heat map of Z values in \code{colocalpixel}.
#' 
#' @details The positive value of Z value is labelled in blue color and negative value is labelled in red color.
#' 
#' @importFrom plotly plot_ly
#' @importFrom grDevices colorRamp
#' @importFrom grDevices colorRampPalette
#' 
#' @param object an object of class \code{\link{colocalpixel}} 
#' @param ... Arguments to be passed to methods.
#' 
#' @return Invisible \code{NULL}
#' 
#' @examples  
#' X=matrix(runif(32*32),32,32)
#' Y=matrix(runif(32*32),32,32)
#' co <- colocalpixel(X,Y)
#' HeatMapZ(co)  
#' 
#' @family spatial colocalization analysis
#' @seealso \code{\link{colocalpixel}} 
#' 
#' @export
#' 
#' @author Shulei Wang
HeatMapZ <- function( object,... )
{
  Zvalue = object$Zvalue
  matlist=unique(c(Zvalue))
  matmax=max(matlist)
  matmin=min(matlist)
  n=256
  if (matmax > -matmin)
  {
    colorbarg=colorRampPalette(c('white','blue'))(n)
    colorbarl=colorRampPalette(c('white','red'))(floor(n/matmax*(-matmin)))
  }
  else
  {
    colorbarg=colorRampPalette(c('white','blue'))(floor(n/(-matmin)*matmax))
    colorbarl=colorRampPalette(c('white','red'))(n)
  }
  colist=c(rev(colorbarl),colorbarg)
  colz=colorRamp(colist)
  plot_ly(z = apply(Zvalue,1,rev), colors = colz, type = "heatmap")
}

