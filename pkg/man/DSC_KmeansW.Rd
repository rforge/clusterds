\name{DSC_KmeansW}
\alias{DSC_KmeansW}
\title{DataStreamClusterer: KmeansW}
\description{Creates a new DataStreamClusterer with the k-means algorithm with the option of including weights.}

\usage{
	DSC_KmeansW(k, iter.max = 10, nstart = 1)
}

\arguments{
	\item{k}{either the number of clusters, say k, or a set of initial (distinct) cluster centres. If a number, a random set of (distinct) rows in x is chosen as the initial centres.}
	\item{iter.max}{the maximum number of iterations allowed.}
	\item{nstart}{if centers is a number, how many random sets should be chosen?}
}

\details{
\code{KmeansW} behaves exactly like the traditional \code{Kmeans} with one exception. During the
clustering procedure a vector of weights can be included to achieve results for weighted data.

This is particularly useful when reclustering microclusters from other DSCs. By including the weight
for ech microcluster more accurate results can be obtained.
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

dsc <- DSC_Kmeans(k = 3)
cluster(dsc, dsd, 500)
dsc

# plot micro-clusters
plot(get_points(dsd,500), col="gray", main= "k-means")
centers <- get_centers(dsc)
points(centers, col="red", cex=5)
}
