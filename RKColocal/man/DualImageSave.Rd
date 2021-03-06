% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colocalvisual.R
\name{DualImageSave}
\alias{DualImageSave}
\title{Dual-Channel Image Writing}
\usage{
DualImageSave(X, Y, filepath = NULL, name, isSplit = FALSE,
  isScale = TRUE, mask = NULL)
}
\arguments{
\item{X}{A numerical matrix, the intensity matrix of the first channel/red channel.}

\item{Y}{A numerical matrix, the intensity matrix of the first channel/green channel.}

\item{filepath}{A string indicating where the file shall be saved.}

\item{name}{A string for the name of saved image.}

\item{isSplit}{An logical value. If it's \code{TRUE}, then two channels dispaly individually, and if \code{FALSE}, a image with merging two channels dispaly.}

\item{isScale}{An logical value to determine whether displayed image is scaled.}

\item{mask}{An logical matrix, where TRUE means pixel is involved into analysis and vice versa. Dimension of mask must be matched with one of image.}
}
\value{
Invisible \code{NULL}
}
\description{
\code{DualImageSave} is used to save dual channel image.
Each channel can be saved individually or two channels can be saved as single overlayed image.
The same with \code{\link{DualImageVisual}}, this function can also increase contrast ratio of plot by scaling.
If only region of interest is saved, mask can be used.
}
\details{
This function depends on \code{\link[EBImage]{writeImage}} mainly.
\code{X} and \code{Y} are preprocessed by scaling or masking firstly.
Then, tao images name of which end with 'red' and 'green' are saved if two channels are saved individually, or only one image is saved.
In addition, after scaling, the largest intensity in each channel is always 1.
}
\examples{
X<-matrix(runif(10*10),nrow=10,ncol=10)
Y<-matrix(runif(10*10),nrow=10,ncol=10)
DualImageSave(X,Y, name = 'sample.jpeg')

}
\seealso{
\code{\link{DualImageVisual}}
}
\author{
Shulei Wang
}
