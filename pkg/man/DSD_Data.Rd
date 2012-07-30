\name{DSD_Data}
\alias{DSD_Data}
\title{DataStreamData: Data (a wrapper for data sets within the mlbench library)}
\description{
A DataStreamData class that wraps data from the mlbench library and
provides access to the data as a data stream. 
The data can either be looped or replayed manually to give
the exact same data to 2 different DataStreamTask objects.
}

\usage{DSD_Data(data, loop = FALSE)}

\arguments{
	\item{data}{The name of the dataset from mlbench}
	\item{loop}{A flag that tells the stream to loop or 
				not to loop over the data frame.}
}

\details{
The \code{DSD_Data} class is designed to be a wrapper class for data that is
held in memory in either a data frame or matrix form. It functions nearly identical
to \code{DSD_Wrapper}.

It works like the other DSD classes--by calling \code{get_points()} to retrieve
data from the stream. The function \code{reset_stream()} can be used to move
the counter back to the beginning of the stream. This is an important feature
to be able to replay stream data for multiple clusters as shown in the example
below.

The value returned is a \code{list} of class \code{DSD}, and
\code{DSD_Data}. The important items with the list are \code{strm}, the data
being wrapped, \code{state} the environment which contains the counter, and
\code{d}, the number of dimensions in the stream.

The supported data sets are the following: \code{BostonHousing}, \code{BostonHousing2}, \code{BreastCancer}, \code{DNA}, \code{Glass}, \code{Ionosphere}, \code{LetterRecognition}, \code{Ozone}, \code{PimaIndiansDiabete}, \code{Satellite}, \code{Servo}, \code{Shuttle}, \code{Sonar}, \code{Soybean}, \code{Vehicle}, \code{Vowel}, \code{Zoo}, \code{HouseVotes84}.
}

\value{
Returns a \code{DSD_Data} object which is a list of the defined parameters. The parameters are either passed in from the function or created internally. They include: 

	\item{description}{The name of the class of the DSD object.}
	\item{strm}{The data frame or matrix that the stream is wrapping.}
	\item{state}{The environment variable which holds the counter for the data frame (accessed through state$counter).}
	\item{d}{The number of dimensions (ncol(strm).}
	\item{k}{The number of clusters (may not be defined).}
	\item{loop}{The flag that determines if looping is or is not enabled.}
}

\seealso{
	\code{\link{DSD_Wrapper}},
	\code{\link{reset_stream}}
}

\examples{
# creating the DSD_Data
dsd <- DSD_Data("Shuttle")
dsd

plot(get_points(dsd,n=1000))
}
