\name{DSC_DBSCAN}
\alias{DSC_DBSCAN}
\title{DataStreamClusterer: DBSCAN}
\description{Creates a new DataStreamClusterer with the DBSCAN algorithm}

\usage{
	DSC_DBSCAN(eps, MinPts = 5, scale = FALSE, method = c("hybrid", "raw", "dist"), seeds = TRUE, showplot = FALSE, countmode = NULL)
}

\arguments{
	\item{eps}{Reachability Distance.}
	\item{MinPts}{Reachability minimum no. of points.}
	\item{scale}{scale the data}
	\item{method}{"dist" treats data as distance matrix (relatively fast
	but memory expensive), "raw" treats data as raw data and avoids
	calculating a distance matrix (saves memory but may be slow), "hybrid"
	expects also raw data, but calculates partial distance matrices (very
	fast with moderate memory requirements ).}
	\item{seeds}{FALSE to not include the isseed-vector in the DBSCAN-object.}
	\item{showplot}{0 = no plot, 1 = plot per iteration, 2 = plot per subiteration.}
	\item{countmode}{NULL or vector of point numbers at which to report progress.}
}

\details{
Please refer to package \code{DBSCAN} for more details on the behavior of \code{DBSCAN}.
}

\value{
A list of class \code{DSC}, \code{DSC_R}, \code{DSC_Macro}, and \code{DSC_DBSCAN}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{RObj}{The underlying R object.}
}

\seealso{
	\code{\link{DSClusterer}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_Gaussian_Static(k=3, noise=0.05)

DBSCAN <- DSC_DBSCAN(eps = .05)
cluster(DBSCAN, dsd, 1000)
plot(DBSCAN,dsd)

# DBSCAN can also recluster microclusters
dsc <- DSC_ClusTree()
cluster(dsc, dsd, 1000)

recluster(DBSCAN,dsc)
plot(DBSCAN,dsd,microclusters=TRUE)

}
