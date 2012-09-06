\name{DSC_tNN_Macro}
\alias{DSC_tNN_Macro}
\title{DataStreamClusterer: tNN Macro}
\description{Creates a new DataStreamClusterer with the tNN Macro (threshold Nearest Neighbor) algorithm}

\usage{
	DSC_tNN_Macro(threshold = 0.2, minPoints = 2, measure = "euclidean",
	centroids = identical(tolower(measure), "euclidean"), lambda=0, beta=0.5)
}

\arguments{
	\item{threshold}{The threshold in the nearest neighborhood algorithm.}
	\item{minPoints}{The minimum number of points a micro-cluster
	    needs to have.}
	\item{measure}{The measure used to calculate cluster proximity.}
	\item{centroids}{A flag that indicates if centroids are used for clusters.}
	\item{lambda}{The lambda used in the fading function.}
	\item{beta}{The measure used to remove clusters in the fading function.}
}

\details{
The threshold Nearest Neighbor algorithm is used in the rEMM package cited
below. For each data point in the incoming stream, if it is below the threshold
value of dissimilarity value of any existing clusters (known as states in the
EMM algorithm), it joins that cluster. Otherwise, a new cluster is created to
accomodate the new data point.

\code{measure} defines what measure is used to measure the dissimilarity between clusters. See \code{dist} in \pkg{proxy} for a list of available measures. 

This is different from \code{DSD_tNN} because it provides macro clustering as well.
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
dsc <- DSC_tNN_Macro()
dsd <- DSD_Gaussian_Static(k = 2, d = 2, mu = rbind(c(.5,.5),c(1.5,1.5)))

cluster(dsc,dsd,1000)

x <- get_points(dsd,1000)

#center of clusters in red
plot(dsc,dsd)

#location of microclusters in black
points(get_microclusters(dsc))

#moving data
dsc <- DSC_tNN_Macro(threshold=.3,lambda=0.02,minPoints=10)
dsd <- DSD_Gaussian_Moving(t=100,n=10)

cluster(dsc,dsd,3000)
}
