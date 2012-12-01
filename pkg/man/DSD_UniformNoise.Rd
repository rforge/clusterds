\name{DSD_UniformNoise}
\alias{DSD_UniformNoise}
\title{DataStreamData: Uniform Noise Generator}
\description{
This generator produces uniform noise in a d-dimensional unit (hyper) cube.
}

\usage{
DSD_UniformNoise(d=2)
}

\arguments{
  \item{d}{Determines the number of dimensions.}
}

%\details{
%}

\value{
Returns a \code{DSD_UniformNoise} object.
}
	   
\seealso{
	\code{\link{DSData}}
}

\examples{
# create data stream with three clusters in 2D
dsd <- DSD_UniformNoise(d=2)
plot(dsd, n=100)
}
