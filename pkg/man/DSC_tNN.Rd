\name{DSC_tNN}
\alias{DSC_tNN}
\title{DataStreamClusterer: tNN}
\description{Creates a new DataStreamClusterer with the tNN (threshold Nearest Neighbor) algorithm}

\usage{
	DSC_tNN(r = 0.2, k = NULL, lambda = 0.01, minweight = 0.1,
                 noise = 0, alpha = 0.4, measure = "Euclidean", macro =
                 TRUE)
}

\arguments{
	\item{r}{The threshold in the nearest neighborhood algorithm.}
	\item{k}{The number of macro clusters to be returned if macro is true.}
	\item{lambda}{The lambda used in the fading function.}
	\item{minweight}{The minimum number of weight a micro-cluster
	    needs to have.}
	\item{noise}{The amount of noise that should be removed while clustering.}
	\item{alpha}{The minimum proportion of shared points between to clusters to warrant 
		combining them.}
	\item{measure}{The measure used to calculate cluster proximity.}
	\item{macro}{A flag that indicates if the macro clusters should be computed.}
}

\details{
The threshold Nearest Neighbor algorithm is used in the rEMM package cited
below. For each data point in the incoming stream, if it is below the threshold
value of dissimilarity value of any existing clusters (known as states in the
EMM algorithm), it joins that cluster. Otherwise, a new cluster is created to
accomodate the new data point.

\code{measure} defines what measure is used to measure the dissimilarity between clusters. See \code{dist} in \pkg{proxy} for a list of available measures. 
}

\value{
A list of class \code{DSC}, \code{DSC_R}, and \code{DSC_tNN}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{RObj}{The underlying R object.}
}

\references{ 
M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In: ICDM '04:
Proceedings of the Fourth IEEE International Conference on Data Mining, pp. 371-374.

M. Hahsler, M. H. Dunham (2010): rEMM: Extensible Markov Model for Data
Stream Clustering in R, Journal of Statistical Software, 35(5), 1-31, URL \url{http://www.jstatsoft.org/v35/i05/}
}

\seealso{
	\code{\link{DSClusterer}}
	\code{\link{DSC_tNN}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_Gaussian_Static(k=3, noise=0.05)

dsc <- DSC_tNN(r=0.05, lambda=.01, macro=FALSE)
cluster(dsc, dsd, 500)
dsc

# plot micro-clusters
plot(get_points(dsd,500), col="gray", main= "tNN")
centers <- get_microclusters(dsc)
points(centers, col="red", cex=5)

# reclustering
cl <- kmeans(centers, centers=3)
points(cl$centers, col="blue", cex=15)
}
