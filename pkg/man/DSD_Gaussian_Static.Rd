\name{DSD_Gaussian_Static}
\alias{DSD_Gaussian_Static}
\title{DataStreamData: Static data stream}
\description{
A DataStreamData that generates random data based upon either a 
defined list covariance matrices or randomly generated covariance
matrices.
}

\usage{
DSD_Gaussian_Static(k=2, d=2, mu, sigma, p, noise=0, noise_range)
}

\arguments{
  \item{k}{Determines the number of clusters.}
  \item{d}{Determines the number of dimensions.}
  \item{mu}{A matrix of means for each dimension of each cluster.}
  \item{sigma}{A list of length \code{k} of covariance matrices.}
  \item{p}{A vector of probabilities that determines the likelihood of
		   generated a data point from a particular cluster.}
  \item{noise}{Noise probability between 0 and 1.
  Noise is uniformly distributed within noise range (see below).
  } 
  \item{noise_range}{A matrix with d rows and 2 columns. The first column
    contains the minimum values and the second column contains the maximum
    values for noise.}
}

\details{
\code{DSD_Gaussian_Static} creates a mixture of \code{k} \code{d}-dimensional
static Gaussians in approximately unit space.
The centers \code{mu} and the covariance matrices \code{sigma}
can be supplied or will be randomly generates. The probability vector \code{p}
defines for each cluster the probability that the next data point will
be chosen from it (defaults to equal probability).
}

\value{
Returns a \code{DSD_Gaussian_Static} object which is a list of the defined
params. The params are either passed in from the function or
created internally. They include: 

	\item{description}{A brief description of the DSD object.}
	\item{k}{The number of clusters.}
	\item{d}{The number of dimensions.}
	\item{mu}{The matrix of means of the dimensions in each cluster.}
	\item{sigma}{The covariance matrix.}
	\item{p}{The probability vector for the clusters.}
	\item{noise}{A flag that determines if or if not noise is generated.}
}
	   
\seealso{
	\code{\link{DSData}}
}

\examples{
# create data stream with three clusters in 2D
dsd1 <- DSD_Gaussian_Static(k=3, d=2)

# plotting the data
plot(dsd1)

# create data stream with specified clusters and 20% noise
dsd2 <- DSD_Gaussian_Static(k=2, d=2, mu=rbind(c(-.5,-.5), c(.5,.5)), 
    noise=0.2, noise_range=rbind(c(-1,1),c(-1,1)))
plot(dsd2)



}
