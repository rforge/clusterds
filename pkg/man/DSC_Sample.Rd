\name{DSC_Sample}
\alias{DSC_Sample}
\title{DataStreamClusterer: Sample}
\description{Creates a set of micro clusters by taking a random sampling from a DSD.}

\usage{
	DSC_Sample(size = 100)
}

\arguments{
	\item{size}{the number of points to be sampled from the stream.}
}

\details{
This object performs a simple sampling procedure to generate a fixed number of micro-clusters. This
is by far the simplest method to generate microcluster, but it is prone to lose some of the details
of the original data set.
}

\value{
A list of class \code{DSC}, \code{DSC_R}, \code{DSC_Micro}, and \code{DSC_Sample}. The list
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

dsc <- DSC_Sample(100)
cluster(dsc, dsd, 1000)
dsc

# plot micro-clusters
plot(dsc,dsd)

# reclustering (Kmeans is sufficient because DSC_Sample does not return weights)
kmeans <- DSC_Kmeans(3)
recluster(kmeans,dsc)
plot(kmeans,dsd,microclusters=TRUE)

}
