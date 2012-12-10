\name{DSD_ReadStream}
\alias{DSD_ReadStream}
\alias{close_stream}
\title{Read a Data Stream from File}

\description{
A DSD class that reads a data stream from a file or any R connection.
}

\usage{
DSD_ReadStream(x, sep=",", take=NULL, class=NULL, 
    center=FALSE, scale=FALSE, loop=FALSE)
close_stream{dsd}
}

\arguments{
	\item{x}{An open connection, or a file path/URL to be opened as a
	connection.}
	\item{sep}{The character string that separates dimensions in data
	points in the stream.}
	\item{take}{indices of columns to extract from the file.}
	\item{class}{column index for the class attribute.}
	\item{center}{FALSE or a numeric vector of length equal to
	          the number of columns used for centering. }
	\item{scale}{FALSE or a numeric vector of length equal to
	          the number of columns used for scaling.}
	\item{loop}{If enabled, the object will loop through the stream when
	the end has been reached. If disabled, the object will warn the user
	upon reaching the end.}
	\item{dsd}{A object of class \code{DSD_ReadStream}.}
}

\details{
\code{DSD_ReadStream} uses \code{read.table()} to read in data from an R
connection. The connection is responsible for maintaining where the stream
is currently being read from. In general, the connections will consist of files
stored on disk but have many other possibilities (see \code{\link{connection}}).

The position in the file can be reset to the begining using \code{reset_stream()}.
The connection can be closed using \code{close_stream()}.
}

\value{
    An object of class \code{DSD_ReadStream} (subclass of \code{DSD_R},
    \code{DSD}).
}

\seealso{
	\code{\link{DSD}},
	\code{\link{reset_stream}}

}

\examples{
# creating data and writing it to disk
dsd <- DSD_GaussianStatic(k=3, d=5)
write_stream(dsd, "data.txt", n=100, sep=",")

# reading the same data back (as a loop)
dsd2 <- DSD_ReadStream("data.txt", sep=",", loop=TRUE)
dsd2

# clean up
file.remove("data.txt")
}
