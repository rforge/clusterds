\name{DSC_tNN}
\alias{DSC_tNN}
\title{DataStreamClusterer: tNN}
\description{Creates a new DataStreamClusterer with the tNN (threshold Nearest Neighbor) algorithm}

\usage{
	DSC_tNN(threshold = 0.2, measure = "euclidean",
	centroids = identical(tolower(measure), "euclidean"), lambda=0)
}

\arguments{
	\item{threshold}{The threshold in the nearest neighborhood algorithm.}
	\item{measure}{The measure used to calculate cluster proximity.}
	\item{centroids}{A flag that indicates if centroids are used for clusters.}
	\item{lambda}{The lambda used in the fading function.}
}

\details{
The threshold Nearest Neighbor algorithm is used in the rEMM package cited below. For each data point in the incoming stream, if it is below the threshold value of dissimilarity value of any existing clusters (known as states in the EMM algorithm), it joins that cluster. Otherwise, a new cluster is created to accomodate the new data point.

\code{measure} defines what measure is used to measure the dissimilarity between clusters 
}

\value{
A list of class \code{DSC}, \code{DSC_R}, and \code{DSC_tNN}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{clusterFun}{The cluster function specific to DSC_tNN}
	\item{RObj}{The underlying R object associated with DSC_R objects.}
}

\references{ 
M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In: ICDM '04:
Proceedings of the Fourth IEEE International Conference on Data Mining, pp. 371-374.

M. Hahsler, M. H. Dunham (2010): rEMM: Extensible Markov Model for Data
Stream Clustering in R, Journal of Statistical Software, 35(5), 1-31, URL \url{http://www.jstatsoft.org/v35/i05/}
}

\seealso{
	\code{\link{DSClusterer}}%,
	%\code{\link{DSC_MOA}},
	%\code{\link{DSC_DenStream}},
	%\code{\link{DSC_CobWeb}},
	%\code{\link{DSC_StreamKM}},
	%\code{\link{DSC_CluStream}},
	%\code{\link{DSC_ClusTree}}
}

\examples{
dsc <- DSC_tNN()
dsd <- DSD_Gaussian_Static()
cluster(dsc, dsd, 100)
get_centers(dsc)
}
