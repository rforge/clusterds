\name{DSD_ReadStream}
\alias{DSD_ReadStream}
\title{DataStreamData: ReadStream (from an open connection or a file name)}

\description{

}

\usage{
DSD_ReadStream(x, sep=",", loop=FALSE)
}

\arguments{
	\item{x}{An open connection, or a file path/URL to be opened as a connection.}
	\item{sep}{The character string that separates dimensions in data points in the stream.}
	\item{loop}{}
}
\details{
}
\value{
	\item{description}{The name of the class of the DSD object.}
	\item{con}{The connection where the data stream is being read from.}
	\item{sep}{The character string that separates dimensions in data points in the stream.}
	\item{loop}{}
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
