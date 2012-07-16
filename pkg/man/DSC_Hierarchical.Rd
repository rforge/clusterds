\name{DSC_Hierarchical}
\alias{DSC_Hierarchical}
\title{DataStreamClusterer: Hierarchical}
\description{Creates a new DataStreamClusterer with the Hierarchical algorithm}

\usage{
	DSC_Hierarchical(k, method = "complete", members = NULL)
}

\arguments{
	\item{k}{The number of clusters returned by the \code{DSC}.}
	\item{method}{the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward", "single", "complete", "average", "mcquitty", "median" or "centroid".}
	\item{members}{\code{NULL} or a vector with length size of d.}
}

\details{
Please refer to package \code{Hierarchical} for more details on the behavior of the algorithm.
}

\value{
A list of class \code{DSC}, \code{DSC_R}, and \code{DSC_Hierarchical}. The list
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

dsc <- DSC_Hierarchical(k = 3)
cluster(dsc, dsd, 500)
dsc
}
