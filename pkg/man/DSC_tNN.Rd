\name{DSC_tNN}
\alias{DSC_tNN}
\title{DataStreamClusterer: tNN}
\description{Creates a new DataStreamClusterer with the tNN (threshold Nearest Neighbor) algorithm}

\usage{
	DSC_tNN(threshold = 0.2, measure = "euclidean",
	centroids = identical(tolower(measure), "euclidean"), lambda=0)
}

\arguments{
	\item{threshold}{}
	\item{measure}{}
	\item{centroids}{}
	\item{lambda}{}
}

\details{
}

\value{
	\item{description}{The name of the algorithm in the DSC object.}
	\item{clusterFun}{The cluster function specific to DSC_tNN}
	\item{RObj}{The underlying R object associated with DSC_R objects.}
}

%%\references{ } TODO: need to reference the original paper

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_MOA}},
	\code{\link{DSC_DenStream}},
	\code{\link{DSC_CobWeb}},
	\code{\link{DSC_StreamKM}},
	\code{\link{DSC_Clustream}},
	\code{\link{DSC_ClusTree}}
}

\examples{
dsc <- DSC_tNN()
dsd <- DSD_Gaussian_Static()
cluster(dsc, dsd, 5000)
plot(dsc)
}
