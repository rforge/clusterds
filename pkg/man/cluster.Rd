\name{cluster}
\alias{cluster}
\title{Clustering data based with input algorithm and data stream}

\description{
Clusters a number of input points from a data stream into a clustering object.
}

\usage{
cluster(dsc, dsd, n = 1, plot=FALSE, microclusters=FALSE, horizon=500, interval=100, sleep=0.5, ...)
}

\arguments{
  \item{dsc}{a DSC object}
  \item{dsd}{a DSD object}
  \item{n}{number of points to cluster}
  \item{plot}{whether or not a plot should be generated}
  \item{microclusters}{display microclusters on the plot}
  \item{horizon}{max number of points to be displayed from the dsd}
  \item{interval}{the rate of refreshing the plot}
  \item{sleep}{the length of time between each refresh}
  \item{...}{extra information needed by plot}
}

\details{
Clusters input data. The underlying clustering in the DSC object is implicitly updated
as if the object was `passed by reference' in a traditional programming language.

The data points from the DSD object will be extracted one at a time. Although this operation
is slower, it always allows the clustering of \code{n} to infinity data points, as long as
the underlying stream clustering algorithm is capable of summarizing \code{n} amount of
data points.
}

\value{
The updated DSC object is returned invisibly for reassignment. To obtain the updated
clustering result, call \code{get_centers()} upon the DSC object.
}


\seealso{
	\code{\link{DSClusterer}}
}

\examples{
dsc <- DSC_DenStream(initPoints=100)
dsd <- DSD_Gaussian_Static()
cluster(dsc, dsd, 1000)
get_centers(dsc)
}
