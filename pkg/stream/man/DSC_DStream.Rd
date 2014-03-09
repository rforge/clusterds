\name{DSC_DStream}
\alias{DSC_DStream}
\alias{dstream}
\alias{d-stream}
\alias{D-Stream}
\title{D-Stream Data Stream Clustering Algorithm}
\description{
    Implements the D-Stream data stream clustering algorithm.}

\usage{
DSC_DStream(gridsize = 0.1, d=NA_integer_, lambda = 1e-3, 
   gaptime=1000L, Cm=1, Cl=.5, attraction=FALSE, epsilon=gridsize*.25)
}

\arguments{
	\item{gridsize}{Size of grid cells. Vector of length 
		\code{d} (dimensionality of the data). If only
		one value is specified then the same size is used for 
		all dimensions.}
	\item{d}{dimensionality of the data. If unspecified then it is taken from the 
    first clustered data point.}
	\item{lambda}{The lambda used in the fading function (Note: in the paper the 
    authors use denote the decay factor as lambda, while here the decay factor
    will be computed from lambda using \eqn{2^-lambda}).}
	\item{gaptime}{grid density are decayed and sporadic grids are removed every
    gaptime points.}
	\item{Cm}{threshold used to detect dense grids (Cm >= 1).}
	\item{Cl}{threshold to detect transitional and sporadic grids (0 > Cl > 1).}
  \item{attraction}{compute and store information about the attraction between
    adjacent grids.}
  \item{epsilon}{overlap parameter for attraction.}
}

%\details{
%}

\value{
An object of class \code{DSC_DStream} (subclass of  \code{DSC}, \code{DSC_R}, \code{DSC_Micro}). 
}

\references{ 
Yixin Chen and Li Tu. 2007. Density-based clustering for real-time stream data. In Proceedings of the 13th ACM SIGKDD international conference on Knowledge discovery and data mining (KDD '07). ACM, New York, NY, USA, 133-142.

Li Tu and Yixin Chen. 2009. Stream data clustering based on grid density and attraction. ACM Trans. Knowl. Discov. Data 3, 3, Article 12 (July 2009), 27 pages.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSC_Micro}}
}

\examples{
set.seed(0)
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

dstream <- DSC_DStream(gridsize=.05)
cluster(dstream, dsd, 500)

# see micro-clusters
nclusters(dstream)
head(get_centers(dstream))

# plot
plot(dstream, dsd)
plot(dstream, image=TRUE)

# reclustering: standard D-Stream uses reachability (use gridsize as h)
macro <- DSC_Hierarchical(h=.05+1e-3, method="single")
recluster(macro, dstream)
plot(macro, dsd)
}