\name{DSD_ReadStream}
\alias{DSD_ReadStream}
\title{Read a Data Stream from File}

\description{
A DSD class that reads a data stream from a file or any R connection.
}

\usage{
DSD_ReadStream(x, sep=",", loop=FALSE)
}

\arguments{
	\item{x}{An open connection, or a file path/URL to be opened as a
	connection.}
	\item{sep}{The character string that separates dimensions in data
	points in the stream.}
	\item{loop}{If enabled, the object will loop through the stream when
	the end has been reached. If disabled, the object will warn the user
	upon reaching the end.}
}

\details{
\code{DSD_ReadStream} uses \code{read.table()} to read in data from an R
connection. The connection is responsible for maintaining where the stream
is currently being read from. In general, the connections will consist of files
stored on disk but have many other possibilities (see \code{\link{connection}}).

The \code{get_points()} method can pass additional parameters to \code{read.table()}
to alter how the reading is done. By default, the \code{comment.char} is set to
an empty string for performance reasons.
}

\value{
    An object of class \code{DSD_ReadStream} (subclass of \code{DSD_R},
    \code{DSD}).
}

\seealso{
	\code{\link{DSD}}
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
