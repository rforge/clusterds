\name{DSD_Static}
\alias{DSD_Static}
\title{DataStreamData: Static datastream}
\description{A DataStream that generates random data based upon either a 
			 defined covariance matrix or a randomly generated covariance
			 matrix.}
\usage{DSD_Static(k=3, d=2) #leaving the other params as defaults}
\arguments{
  \item{k}{Determines the number of clusters.}
  \item{d}{Determines the number of dimensions.}
  \item{mu}{A matrix of means for each dimension of each cluster.}
  \item{sigma}{The covariance matrix.}
  \item{p}{A vector of probabilities that determines the likelihood of
		   generated a data point from a particular cluster.}
}

%%\details{} TODO

\value{Returns a DSD_Static object which is a list of the defined
	   params. The params are either passed in from the function or
	   created internally. They include: the number of clusters k,
	   the number of dimensions d, the means mu, the covariance
	   matrix sigma, and probability p.}
	   
\seealso{
 \code{\link{DSD_MOA}},
 \code{\link{DSD_ReadStream}}
}
\examples{
# other params will be generated internally
dsd <- DSD_Static(k=2, d=2)
dsc <- DSC_DenStream()

# obtaining some data
sample <- getPoints(dsd, 500)

# clustering some data, this will take some time
cluster(dsc, dsd, 15000)

# plotting the data
plot(sample, xlab='x', ylab='y', pch=4, cex=.5)
points(getCenters(dsc),col='red', cex=3,lwd=2)
}
