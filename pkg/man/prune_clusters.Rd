\name{prune_clusters}
\alias{prune_clusters}
\title{prune_clusters}

\description{
Prunes a quantile of the clusters of a given DSC.
}

\usage{
	prune_clusters(dsc, threshold= 0.05, prob = TRUE)
}

\arguments{
	\item{dsc}{The DSC object to be pruned.}
	\item{threshold}{The numeric vector of probabilities for the quantile.}
	\item{prob}{A flog to determine if quantiles should be used.}
}

\details{
This method is used to remove clusters that have been deemed as noise.
It first orders the weight of all of the clusters and then performs \code{quantile}
to determine which clusters should be pruned from the DSC.

It then returns a \code{DSC_Static} with the remaining clusters.
}

\value{
A \code{DSC_Static} is returned. It possesses the remaining centers, the original
microclusters, and the weights to the remaining centers.
}

\seealso{
	\code{\link{DSC_Static}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_Gaussian_Static(k=3, noise=0.05)

dsc <- DSC_tNN(threshold=0.05, lambda=.01)
cluster(dsc, dsd, 500)
dsc

# prune the data and collect the DSC_Static
dsc_static <- prune_clusters(dsc)

#plot the results
plot(get_centers(dsc_static))
}