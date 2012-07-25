\name{get_microclusters}
\alias{get_microclusters}
\title{Get Micro-clusters}
\description{Retrieves the micro-clusters within a DSC object.}

\usage{
	get_microclusters(x)
}

\arguments{
	\item{x}{the DSC from which the micro-clusters are being retrieved from.}
}

\details{
	This function is used for DSCs that are used to recluster data (/code{DSC_Kmeans}, 
	/code{DSC_DBSCAN}, /code{DSC_Hierarchical}, etc). When this function is called it
	will return the original set of micro-clusters used to generate the clusters.
}

\value{
A matrix containing the points of all of the micro-clusters that generated the clusters.
}

\seealso{
	\code{\link{DSClusterer}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_Gaussian_Static(k=3, noise=0.05)

dsc <- DSC_Dbscan(eps = .1)
cluster(dsc, dsd, 500)

get_microclusters(dsc)

}
