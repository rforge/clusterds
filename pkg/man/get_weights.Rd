\name{get_weights}
\alias{get_weights}
\title{Get Weights}
\description{Get the weights of the clusters in the DSC}

\usage{
	get_weights(x, scale)
}

\arguments{
	\item{x}{The DSC object the weights are being requested from.}
	\item{scale}{a boolean determining whether or not the results should sum up to zero}
}

\details{
Each DSC object has a unique way for calculating weights, but they all are
called through the generic function, \code{get_weights()}. This is done by
using the S3 class system. 

This function ensures weights are properly returned to be used with objects like
\code{DSC_KmeansW}.

It will return the weight of the points \code{get_centers()} returns.
}

\value{
A vector containing the weight of each micro-cluster or macro-cluster.
}

\seealso{
	\code{\link{DSClusterer}}
}

\examples{
# 3 clusters with 5% noise
dsd <- DSD_GaussianStatic(k=3, noise=0.05)

dsc <- DSC_CluStream()
cluster(dsc, dsd, 1000)

get_weights(dsc)

}