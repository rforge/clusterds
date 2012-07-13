\name{DSC_Birch}
\alias{DSC_Birch}
\title{DataStreamClusterer: Birch}
\description{Creates a new DataStreamClusterer with the Birch algorithm}

\usage{
	DSC_Birch(radius, compact=radius, keeptree = FALSE, columns = NULL)
}

\arguments{
	\item{radius}{The closeness criterion.}
	\item{compact}{The The compactness criterion.}
	\item{keeptree}{A Boolean, whether to keep the CF tree in memory.}
	\item{columns}{The choice of variables to use if x is a file name or a connection (default is all
variables)}
}

\details{
Please refer to package \code{birch} for more details.
}

\value{
A list of class \code{DSC}, \code{DSC_R}, and \code{DSC_Birch}. The list
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

dsc <- DSC_Birch(radius = .2)
cluster(dsc, dsd, 500)
dsc

# plot micro-clusters
plot(get_points(dsd,500), col="gray", main= "tNN")
centers <- get_centers(dsc)
points(centers, col="red", cex=5)

# reclustering
cl <- kmeans(centers, centers=3)
points(cl$centers, col="blue", cex=15)
}
