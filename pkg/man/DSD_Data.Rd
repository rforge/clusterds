\name{DSD_Data}
\alias{DSD_Data}
\title{A Wrapper for Data Sets from mlbench}
\description{
Provides a convenient wrapper for data sets from the mlbench package.
}

\usage{DSD_Data(data, loop = FALSE, random = FALSE, scale = FALSE)}

\arguments{
	\item{data}{The name of the dataset from mlbench. If missing then
	    a list of all available data sets is shown and returned.}
	\item{loop}{A flag that tells the stream to loop or 
				not to loop over the data frame.}
	\item{random}{A flag that determines if the data should be in a random order.}
	\item{scale}{A flag that determines if the data should be scaled.}
}

\details{
The \code{DSD_Data} class is designed to be a wrapper class for data that is
held in memory in either a data frame or matrix form. It functions nearly
identical to \code{DSD_Wrapper}.

Call \code{DSD_Data} with a missing value for data to get a list of
all available data sets.
}

\value{
Returns a \code{DSD_Data} object which 
is also of class \code{DSD_Wrapper}.
}

\seealso{
	\code{\link{DSD_Wrapper}},
	\code{\link{reset_stream}}
}

\examples{
# creating the DSD_Data
dsd <- DSD_Data("Shuttle")
dsd

plot(dsd, n=100)
}
