\name{DSC_CluStream}
\alias{DSC_CluStream}
\title{DataStreamClusterer: CluStream}
\description{Creates a new DataStreamClusterer with the CluStream algorithm}

\usage{
DSC_CluStream(horizon = 1000, maxNumKernels = 100, kernelRadiFactor = 2)
}

\arguments{
  \item{horizon}{Defines the time window to be used in CluStream}
  \item{maxNumKernels}{Defines the maximum number of kernels used in CluStream}
  \item{kernelRadiFactor}{Kernel radius factor.}
}

\details{
CluStream was originally introduced in Aggarwal's paper cited below. 
Like most data stream clustering algorithms, CluStream summarizes the 
incoming data points into micro-clusters. At any given time, it can be 
assumed there are q micro-clusters maintained by CluStream. This means 
that when a new micro-cluster is created, another one must be deleted, 
and the opposite is true for when two micro-clusters merge with one 
another. A significant amount of thought in CluStream is given to the 
temporal structure of data streams. Snapshots of calculated micro-clusters 
are stored in a "pyramidal time frame" pattern. This allows the user to 
turn back to a specific location in the data stream to mine information 
from a particular time period.

The \code{timeWindow} parameter changes the granularity at which the snapshots 
are taken for the micro-cluster summary maintanence. \code{maxNumKernels} 
is analogous to the q micro-clusters that are maintained within the algorithm.
}

\value{
A list of class \code{DSC}, \code{DSC_MOA}, and \code{DSC_CluStream}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}

\references{
Aggarwal CC, Han J, Wang J, Yu PS (2003). "A Framework for Clustering 
Evolving Data Streams." In "Proceedings of the International 
Conference on Very Large Data Bases (VLDB '03)," pp. 81-92.

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T (2010). 
"MOA: Massive Online Analysis, a Framework for Stream Classification and 
Clustering." In "Journal of Machine Learning Research (JMLR) Workshop and Conference Proceedings,"
}

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_MOA}},
}

\examples{
dsd <- DSD_Gaussian_Static(k=3, noise=.05)

dsc <- DSC_CluStream()
cluster(dsc, dsd, 500)
dsc

# plot micro-clusters
plot(get_points(dsd, 500), col="gray", main = "CluStream")
centers <- get_centers(dsc)
points(centers, col="red", cex=5)

# reclustering
cl <- kmeans(centers, centers=3)
points(cl$centers, col="blue", cex=15)

}
