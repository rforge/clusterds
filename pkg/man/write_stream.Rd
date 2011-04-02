\name{write_stream}
\alias{write_stream}
\title{write_stream}
\description{Writes points to a connection from a DSD object.}

\usage{
	write_stream(dsd, con, n=100, sep=",", col.names=FALSE,
		row.names=FALSE, ...)
}

\arguments{
	\item{dsd}{The DSD object that will generate the data points for output.}
	\item{con}{The R connection to be written to.}
	\item{n}{The number of data points to be written to the connection.}
	\item{sep}{The character that will seperate attributes in a data point.}
	\item{col.names}{A flag that determines if column names will be output.}
	\item{row.names}{A flag that determines if row names will be output.}
	\item{...}{Additional parameters that are passed to \code{write.table()}.}
}

\details{
}

\value{
There is no value returned from this operation.
}

\seealso{
	\code{\link{write.table}},
	\code{\link{DSD_Gaussian_Static}},
	\code{\link{DSD_ReadStream}},
%%	\code{\link{DSD_Gaussian_Dynamic}},
	\code{\link{DSD_DataFrame}},
	\code{\link{DSD_MOA}},
}

\examples{
# creating data and writing it to disk
dsd <- DSD_Gaussian_Static(k=3, d=5)
write_stream(dsd, "dsd_data.txt", n=100, sep=",")
}
