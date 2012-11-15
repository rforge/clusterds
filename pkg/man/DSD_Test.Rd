\name{DSD_Test}
\alias{DSD_Test}
\title{DataStreamData: Test data stream}
\description{
A DataStreamData that generates a data stream in the shape of two bars and two gaussians clusters.}

\usage{
DSD_Test(noise = 0)
}

\arguments{
  \item{noise}{The amount of noise that should be added to the output.}
}

\details{
\code{DSD_Test} is a DSD generator for stream data. 
It has been implemented entirely in R, so there is no computational 
overhead with communicating to the Java Runtime Interface (JRI) or 
native C code. This DSD will produce a two guassian clusters on the right
with two bar clusters on the left.
}

\value{
Returns a \code{DSD_Data} object which is a list of the defined
params. The params are either passed in from the function or
created internally. They include: 

	\item{description}{A brief description of the DSD object.}
	\item{k}{The number of clusters.}
	\item{d}{The number of dimensions.}
}
	   
\seealso{
	\code{\link{DSData}}
}

\examples{
# create data stream with three clusters in 2D
dsd <- DSD_Test()

# plotting the data
plot(dsd)


}
