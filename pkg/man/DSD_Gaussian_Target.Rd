\name{DSD_Target}
\alias{DSD_Target}
\title{DataStreamData: Target data stream}
\description{
A DataStreamData that generates a data stream in the shape of a target. It has
a single gaussian cluster in the center and a ring that surrounds it.}

\usage{
DSD_Target()
}

\details{
\code{DSD_Target} is a DSD generator for stream data. 
It has been implemented entirely in R, so there is no computational 
overhead with communicating to the Java Runtime Interface (JRI) or 
native C code. This DSD will produce a singular guassian cluster in the
center with a ring around it.
}

\value{
Returns a \code{DSD_Target} object which is a list of the defined
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
dsd1 <- DSD_Target()

# obtaining some data
sample <- get_points(dsd1, 500)

# plotting the data
plot(sample)


}
