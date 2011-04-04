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
\code{write_stream()} accepts either a \code{connection} directly, or the file name to be written to. The \code{sep} parameter defines how the dimensions in each data point are separated. Behind the scenes we are using the \code{write.table()} function to write the data to disk. We are able to pass additional parameters to this function to alter how the data is written. By default, the column names and row names are hidden.

Additionally, the parameter \code{comment.char} in \code{write.table} is hidden due to performance reasons. 
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
