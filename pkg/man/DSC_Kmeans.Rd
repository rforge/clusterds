\name{DSC_Kmeans}
\alias{DSC_Kmeans}
\title{DataStreamClusterer: Kmeans}
\description{Creates a new DataStreamClusterer with the Kmeans algorithm}

\usage{
	DSC_Kmeans(centers, iter.max = 10, nstart = 1,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
}

\arguments{
	\item{centers}{either the number of clusters, say k, or a set of initial (distinct) cluster centres. If a number, a random set of (distinct) rows in x is chosen as the initial centres.}
	\item{iter.max}{the maximum number of iterations allowed.}
	\item{nstart}{if centers is a number, how many random sets should be chosen?}
	\item{algorithm}{character: may be abbreviated.}
}

\details{
Please refer to package \code{kmeans} for more details on the behavior of \code{kmeans}.
}

\value{
A list of class \code{DSC}, \code{DSC_R}, and \code{DSC_Kmeans}. The list
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

dsc <- DSC_Kmeans(centers = 3)
cluster(dsc, dsd, 500)
dsc

# plot micro-clusters
plot(get_points(dsd,500), col="gray", main= "tNN")
centers <- get_centers(dsc)
points(centers, col="red", cex=5)
}
