\name{DSD_DataFrame}
\alias{DSD_DataFrame}
\title{DataStreamData: DataFrame (a wrapper for data frames and matrices)}
\description{

}
\usage{
DSD_DataFrame(df, k)
}
\arguments{
	\item{df}{A data frame or matrix with the data.}
	\item{k}{Optional: The number of clusters}
}
\details{
}
\value{
	\item{description}{The name of the class of the DSD object.}
	\item{strm}{The data frame or matrix that the stream is wrapping.}
	\item{state}{The environment variable which holds the counter for the data frame.}
	\item{d}{The number of dimensions (ncol(strm).}
	\item{k}{The number of clusters (may not be defined).}
}
%%\seealso{
%%}
\examples{
}
