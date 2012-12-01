\name{cluster_animation}
\alias{cluster_animation}
\title{Clustering data based with input algorithm and data stream and then animates it}

\description{
Clusters a number of input points from a data stream into a clustering object and then
produces an animation as an animated gif.
}

\usage{
cluster_animation(dsc,dsd,n=1,interval=.1, horizon=500, pointInterval=100, weights=FALSE, scale=c(1,10), outdir=NULL,...)
}

\arguments{
  \item{dsc}{a DSC object}
  \item{dsd}{a DSD object}
  \item{n}{the number of points to cluster}
  \item{interval}{the time interval between each frame}
  \item{horizon}{the max number of data points to be shown at a time}
  \item{pointInterval}{the number of new points needed before a new frame is created}
  \item{weights}{whether or not the macro-clusters should be different sizes depending on 
  	weight}
  \item{scale}{the scaling used for the weights}
  \item{outdir}{the location of where the gif should be saved}
  \item{...}{extra arguments for plot.}
}

\details{
Clusters input data. The underlying clustering in the DSC object is implicitly updated
as if the object was `passed by reference' in a traditional programming language.

The data points from the DSD object will be extracted one at a time. Although this operation
is slower, it always allows the clustering of \code{n} to infinity data points, as long as
the underlying stream clustering algorithm is capable of summarizing \code{n} amount of
data points.

After clustering the function generates an animated gif. If no \code{outdir} is specified,
the function will animate the progress while clustering. This can be particularly useful
to tune clustering algorithms on moving or evolving datasets.
}

\value{
The updated DSC object is returned invisibly for reassignment. To obtain the updated
clustering result, call \code{get_centers()} upon the DSC object.
}


\seealso{
	\code{\link{DSClusterer}}
	\code{\link{cluster}}
}

\examples{
dsc <- DSC_tNN(.1,alpha=.3,lambda=.01)
dsd <- DSD_GaussianMoving(t=20,n=20)

cluster_animation(dsc, dsd, 1100,interval=.01,
	pointInterval=10,microclusters=TRUE,outdir=getwd(),
	xlim=c(-.5,1.5),ylim=c(-.5,1.5))
}
