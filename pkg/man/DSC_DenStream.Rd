\name{DSC_DenStream}
\alias{DSC_DenStream}
\title{DenStream Data Stream Clusterer}
\description{
Class implements the DenStream cluster algorithm for data streams.
}
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
Interface to the DenStream implementation in MOA.
}
\value{
An object of class \code{DSC_DenStream} (subclass of
\code{DSC}, \code{DSC_MOA}, \code{DSC_Micro})
}

\references{
Cao F, Ester M, Qian W, Zhou A (2006). Density-Based Clustering over an
Evolving Data Stream with Noise. In Proceedings of the 2006 SIAM
International Conference on Data Mining, pp 326-337. SIAM. 

Bifet A, Holmes G, Pfahringer B, Kranen P, Kremer H, Jansen T, Seidl T
(2010).  MOA: Massive Online Analysis, a Framework for Stream
Classification and Clustering. In Journal of Machine Learning Research
(JMLR).
}
\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}},
	\code{\link{DSC_MOA}}
}
\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

dsc <- DSC_DenStream(epsilon=.05,minPoints=3,initPoints=100)
cluster(dsc, dsd, 500)
dsc

# plot micro-clusters
plot(dsc, dsd)

# reclustering
kmeans <- DSC_KmeansW(3)
recluster(kmeans,dsc)
plot(kmeans, dsd)
}
