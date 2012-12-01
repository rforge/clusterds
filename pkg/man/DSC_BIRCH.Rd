\name{DSC_BIRCH}
\alias{DSC_BIRCH}
\title{DataStreamClusterer: BIRCH}
\description{Creates a new DataStreamClusterer with the BIRCH algorithm}

\usage{
	DSC_BIRCH(radius, compact = radius, keeptree = TRUE, columns = NULL)
}

\arguments{
	\item{radius}{The closeness criterion.}
	\item{compact}{The The compactness criterion.}
	\item{keeptree}{A Boolean, whether to keep the CF tree in memory.}
	\item{columns}{The choice of variables to use if x is a file name or a connection (default is all
variables)}
}

\details{
Please refer to package \code{BIRCH} for more details. It is treated as a micro clusterer within
the \code{stream} package.
}

\value{
A list of class \code{DSC}, \code{DSC_R}, \code{DSC_Micro}, and \code{DSC_BIRCH}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{RObj}{The underlying R object.}
}

\seealso{
	\code{\link{DSClusterer}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

dsc <- DSC_BIRCH(radius = .05)
cluster(dsc, dsd, 1000)
dsc

# plot micro-clusters
plot(dsc,dsd)

# reclustering
kmeans <- DSC_KmeansW(3)
recluster(kmeans,dsc)
plot(kmeans,dsd,microclusters=TRUE)

}
