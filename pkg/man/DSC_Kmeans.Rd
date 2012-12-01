\name{DSC_Kmeans}
\alias{DSC_Kmeans}
\title{DataStreamClusterer: Kmeans}
\description{Creates a new DataStreamClusterer with the k-means algorithm.}

\usage{
	DSC_Kmeans(k, iter.max = 10, nstart = 1,algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
}

\arguments{
	\item{k}{either the number of clusters, say k, or a set of initial (distinct) cluster centres. If a number, a random set of (distinct) rows in x is chosen as the initial centres.}
	\item{iter.max}{the maximum number of iterations allowed.}
	\item{nstart}{if centers is a number, how many random sets should be chosen?}
	\item{algorithm}{character: may be abbreviated.}
}

\details{
Please refer to package \code{kmeans} for more details on the behavior of \code{kmeans}.
}

\value{
A list of class \code{DSC}, \code{DSC_R}, \code{DSC_Macro}, and \code{DSC_Kmeans}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{RObj}{The underlying R object.}
}

\seealso{
	\code{\link{DSClusterer}}
	\code{\link{DSC_KmeansW}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

kmeans <- DSC_Kmeans(3)
cluster(kmeans, dsd, 1000)
plot(kmeans,dsd)

# Kmeans can also recluster microclusters, but it is suggested to use KmeansW instead
dsc <- DSC_ClusTree()
cluster(dsc, dsd, 1000)

recluster(kmeans,dsc)
plot(kmeans,dsd,microclusters=TRUE)
}
