\name{DSD_Wrapper}
\alias{DSD_Wrapper}
\title{DataStreamData: Wrapper (a wrapper for data frames and matrices)}
\description{
A DataStreamData class that wraps either a data frame or matrix in memory and
provides access to the data as a data stream. 
The data can either be looped or replayed manually to give
the exact same data to 2 different DataStreamTask objects.
}

\usage{DSD_Wrapper(x, k, loop=FALSE)}

\arguments{
	\item{x}{A data frame or matrix with the data to be used in the stream.}
	\item{k}{Optional: The number of clusters}
	\item{loop}{A flag that tells the stream to loop or 
				not to loop over the data frame.}
}

\details{
The \code{DSD_Wrapper} class is designed to be a wrapper class for data that is held in memory in either a data frame or matrix form.

It works like the other DSD classes--by calling \code{get_points()} to retrieve data from the stream. The function \code{reset_stream()} can be used to move the counter back to the beginning of the stream. This is an important feature to be able to replay stream data for multiple clusters as shown in the example below.

The value returned is a \code{list} of class \code{DSD}, and \code{DSD_Wrapper}. The important items with the list are \code{strm}, the data being wrapped, \code{state} the environment which contains the counter, and \code{d}, the number of dimensions in the stream.
}

\value{
Returns a \code{DSD_Wrapper} object which is a list of the defined parameters. The parameters are either passed in from the function or created internally. They include: 

	\item{description}{The name of the class of the DSD object.}
	\item{strm}{The data frame or matrix that the stream is wrapping.}
	\item{state}{The environment variable which holds the counter for the data frame (accessed through state$counter).}
	\item{d}{The number of dimensions (ncol(strm).}
	\item{k}{The number of clusters (may not be defined).}
	\item{loop}{The flag that determines if looping is or is not enabled.}
}

\seealso{
%%	\code{\link{DSD_MOA}},
%%	\code{\link{DSD_ReadStream}},
%%	\code{\link{DSD_Gaussian_Dynamic}},
%%	\code{\link{DSD_Gaussian_Static}},
%%	\code{\link{write_stream}},
	\code{\link{reset_stream}}
}

\examples{
dsd <- DSD_Gaussian_Static(k=3, d=2)
d <- get_points(dsd, 500)
head(d)

# creating the DSD_Wrapper from d
replayer <- DSD_Wrapper(d, k=3)
replayer

# creating 2 clusterers of different algorithms
dsc1 <- DSC_CluStream()
dsc2 <- DSC_DenStream(initPoints=100)

# clustering the same data in 2 DSC objects
cluster(dsc1, replayer, 500)
reset_stream(replayer) # resetting the replayer to the first position
cluster(dsc2, replayer, 500)
}
