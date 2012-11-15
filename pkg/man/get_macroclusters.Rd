\name{get_macroclusters}
\alias{get_macroclusters}
\title{Get Macro-clusters}
\description{Retrieves the macro-clusters within a DSC object.}

\usage{
	get_macroclusters(x)
}

\arguments{
	\item{x}{the DSC from which the macro-clusters are being retrieved from. Must be a \code{DSC_Macro}}
}

\details{
	This function is used for DSCs that are used to recluster data (/code{DSC_Kmeans}, 
	/code{DSC_DBSCAN}, /code{DSC_Hierarchical}, etc). When this function is called it
	will return the original set of macro-clusters used to generate the clusters.
}

\value{
A matrix containing the points of all of the macro-clusters that generated the clusters.
}

\seealso{
	\code{\link{DSClusterer}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_Gaussian_Static(k=3, noise=0.05)

dsc <- DSC_Kmeans(3)
cluster(dsc, dsd, 1000)

get_macroclusters(dsc)

}
