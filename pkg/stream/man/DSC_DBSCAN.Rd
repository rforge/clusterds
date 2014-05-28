\name{DSC_DBSCAN}
\alias{DSC_DBSCAN}
\title{DBSCAN Macro-clusterer}
\description{
    Implements the DBSCAN algorithm for reclustering
         micro-clusterings.
}

\usage{
	DSC_DBSCAN(eps, MinPts = 5, weighted = TRUE)
}

\arguments{
	\item{eps}{radius of the eps-neighborhood.}
	\item{MinPts}{minimum number of points required in the eps-neighborhood.}
  \item{weighted}{logical indicating if a weighted version of DBSCAN should be used. }
}

\details{
DBSCAN is a weighted extended version of the implementation in \pkg{fpc} where
each micro-cluster center considered a pseudo point. For weighting we use
in the MinPts comparison the sum of weights of the micro-cluster instead of the 
number.

DBSCAN first finds core points based on the number of other points
in its eps-neighborhood. Then core points are joined into clusters using 
reachability (overlapping eps-neighborhoods). 
}

\value{
An object of class \code{DSC_DBSCAN} (a subclass of \code{DSC}, 
\code{DSC_R}, \code{DSC_Macro}).
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Macro}}
}

\references{
Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu (1996). A density-based algorithm for discovering clusters in large spatial databases with noise. In Evangelos Simoudis, Jiawei Han, Usama M. Fayyad. Proceedings of the Second International Conference on Knowledge Discovery and Data Mining (KDD-96). AAAI Press. pp. 226-231.
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_Gaussians(k=3, noise=0.05)

# Use DBSCAN to recluster micro clusters (a sample)
dsc <- DSC_Sample(k=100)
cluster(dsc, dsd, 500)

DBSCAN <- DSC_DBSCAN(eps = .05)
recluster(DBSCAN,dsc)
plot(DBSCAN, dsd)

# For comparison we can cluster some data with DBSCAN directly
# Note: DBSCAN is not suitable for data streams since it passes over the data
# several times.
DBSCAN <- DSC_DBSCAN(eps = .05)
cluster(DBSCAN, dsd, 500)
plot(DBSCAN, dsd)
}
