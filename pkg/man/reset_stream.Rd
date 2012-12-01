\name{reset_stream}
\alias{reset_stream}
\title{reset_stream}

\description{
Resets the counter in a DSD object to the beginning.
}

\usage{
	reset_stream(dsd)
}

\arguments{
	\item{dsd}{Either a \code{DSD_ReadStream} or \code{DSD_Wrapper} object.}
}

\details{
Resets the counter of the stream object that is passed in. For 
\code{DSD_ReadStream} objects, this is done by calling \code{seek()} on the
underlying connection. For \code{DSD_Wrapper}, the counter stored in the
environment variable is moved back to 1.
}

\seealso{
	\code{\link{DSD_ReadStream}},
	\code{\link{DSD_Wrapper}},
	\code{\link{write_stream}}
}

\examples{
# initializing the objects
dsd <- DSD_GaussianStatic(k=3, d=2)
replayer <- DSD_Wrapper(get_points(dsd, 100), k=3)
dsc1 <- DSC_CluStream()
dsc2 <- DSC_DenStream()

# clustering the same data in 2 DSC objects
cluster(dsc1, replayer, 100)
reset_stream(replayer) # resetting the dsd to its original state
cluster(dsc2, replayer, 100)
}
