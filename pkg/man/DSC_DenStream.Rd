\name{DSC_DenStream}
\alias{DSC_DenStream}
\title{DataStreamClusterer: DenStream}
\description{Creates a new DataStreamClusterer with the DenStream algorithm}

\usage{
DSC_DenStream(horizon = 1000, epsilon = 0.1, minPoints = 10, beta = 0.001, mu = 1, initPoints = 100, lambda = NULL)
}

\arguments{
  \item{horizon}{Range of the (time) window.}
  \item{epsilon}{defines the epsilon neighborhood in which the density of each micro-cluster is calculated (i.e.,
  the radius).}
  \item{minPoints}{minimum number of points a micro-cluster must have.}
  \item{beta}{the outlier threshold, used in conjunction with \code{mu}
  (range: 0 to 1)} \item{mu}{the minimum weight a micro-cluster must have
  within the \code{epsilon} neighborhood (range: 0 to max(double))}
  \item{initPoints}{number of points to use for initialization}
  \item{lambda}{number used to override computed lambda}
}

\details{
DSC_DenStream is based on the DenStream algorithm. DenStream is a density
based clustering algorithm that summarizes the incoming data points into
micro-clusters based upon the frequency of data points in a particular
location within a defined boundary (i.e., their density). DenStream
includes a fading function that causes the weight of data points to
decrease exponentially as t increases. This means that temporal structures
within the data stream are also visible using this algorithm. 

The creators of DenStream use a dbscan variation to recluster the
micro-cluster into the final clustering.
}
\value{
A list of class \code{DSC}, \code{DSC_MOA}, \code{DSC_Micro}, and \code{DSC_DenStream}. The
list contains the following items:
	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}

\references{
Cao F, Ester M, Qian W, Zhou A (2006). Density-Based Clustering over an
Evolving Data Stream with Noise. In Proceedings of the 2006 SIAM
International Conference on Data Mining, pp 326-337. SIAM. 

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
(2010).  MOA: Massive Online Analysis, a Framework for Stream
Classification and Clustering. In Journal of Machine Learning Research
(JMLR) Workshop and Conference Proceedings.
}
\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_MOA}}
}
\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

dsc <- DSC_DenStream(epsilon=.05,minPoints=3,initPoints=100)
cluster(dsc, dsd, 1000)
dsc

# plot micro-clusters
plot(dsc,dsd)

# reclustering
kmeans <- DSC_KmeansW(3)
recluster(kmeans,dsc)
plot(kmeans,dsd,microclusters=TRUE)

}
