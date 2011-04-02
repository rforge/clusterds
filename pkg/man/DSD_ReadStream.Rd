\name{DSD_ReadStream}
\alias{DSD_ReadStream}
\title{DataStreamData: ReadStream (from an open connection or a file name)}

\description{
A DSD object that reads a data stream from an R connection.
}

\usage{
DSD_ReadStream(x, sep=",", loop=FALSE)
}

\arguments{
	\item{x}{An open connection, or a file path/URL to be opened as a connection.}
	\item{sep}{The character string that separates dimensions in data points in the stream.}
	\item{loop}{If enabled, the object will loop through the stream when the
			    end has been reached. If disabled, the object will warn the
				user upon reaching the end.}
}

\details{
\code{DSD_ReadStream} uses \code{read.table()} to read in data from an R
connection. The connection is responsible for maintaining where the stream
is currently being read from. In general, the connections will consist of files
stored on disk but have many other possibilities (see \code{\link{connection}}).

The \code{get_points()} method can pass additional params to \code{read.table()}
to alter how the reading is done. By default, the \code{comment.char} is set to
an empty string for performance reasons.
}

\value{
	\item{description}{The name of the class of the DSD object.}
	\item{con}{The connection where the data stream is being read from.}
	\item{sep}{The character string that separates dimensions in data points in the stream.}
	\item{loop}{A flag that determines whether or not the stream will loop.}
}

\seealso{
	\code{\link{DSD_MOA}},
	\code{\link{DSD_ReadStream}},
	\code{\link{DSD_Gaussian_Static}},
%%	\code{\link{DSD_Gaussian_Dynamic}},
	\code{\link{DSD_DataFrame}},
	\code{\link{write_stream}}
	\code{\link{reset_stream}}
}

\examples{
# creating data and writing it to disk
dsd <- DSD_Gaussian_Static(k=3, d=5)
write_stream(dsd, "dsd_data.txt", n=100, sep=",")

# reading the same data back in
dsd2 <- DSD_ReadStream("dsd_data.txt", sep=",")
}
