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
  \item{sigma}{A list of covariance matrices.}
  \item{p}{A vector of probabilities that determines the likelihood of
		   generated a data point from a particular cluster.}
  \item{noise}{Noise is generated in the stream based on this parameter. 
    Noise is the probability of a data point being noise.}
  \item{noise_range}{A matrix with d rows and 2 columns. The first column
    contains the minimum values and the second column contains the maximum
    values for noise. Noise is uniformly distributed within noise range.}
}

\details{
\code{DSD_Gaussian_Static} is a general purpose DSD generator for stream data. 
It has been implemented entirely in R, so there is no computational 
overhead with communicating to the Java Runtime Interface (JRI) or 
native C code. An important characteristic of \code{DSD_Gaussian_Static} 
is that once it has been initialized according to the input parameters 
defined, the defined clusters will not move (i.e., they are static). 
This means that \code{DSD_Gaussian_Static} is not an ideal DSD for examining 
the temporal structure of data streams for drastic changes such as 
the splitting or merging of clusters. 

Its initialization function accepts 5 main parameters: the number 
of clusters \code{k}, the number of dimensions \code{d}, a matrix of means
\code{mu}, a covariance matrix \code{sigma}, and a probability vector \code{p}. 

By default, \code{DSD_Gaussian_Static} generates 2 dimensionality data in 2
different clusters, but the user is able to define any number of clusters 
with any number of dimensions. Additionally, the user may define 
\code{mu}, \code{sigma}, and \code{p}, but if left undefined the constructor
will generate these values automatically. When \code{get_points()} is called on 
\code{DSD_Gaussian_Static}, the data points are generated using the
\code{mvrnorm()} function, making it important to seed the random number
generator to reproduce the experiment. This can be done with \code{set.seed()}.
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
	\item{p}{The probability vector for the cluters.}
	\item{noise}{A flag that determines if or if not noise is generated.}
}
	   
\seealso{
	\code{\link{DSData}}
}

\examples{
# create data stream with three clusters in 2D
dsd1 <- DSD_Gaussian_Static(k=3, d=2)

# obtaining some data
sample <- get_points(dsd1, 500)

# plotting the data
plot(sample)

# create data stream with specified clusters and 20% noise
dsd2 <- DSD_Gaussian_Static(k=2, d=2, mu=rbind(c(-.5,-.5), c(.5,.5)), 
    noise=0.2, noise_range=rbind(c(-1,1),c(-1,1)))
plot(get_points(dsd2, 500))



}
