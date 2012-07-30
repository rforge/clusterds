\name{DSD_mlbench}
\alias{DSD_mlbench}
\title{DataStreamData: mlbench (provides a wrapper for mlbench datagens)}
\description{
A DataStreamData class that wraps datagens found within mlbench and
provides access to the data as a data stream. 
}

\usage{DSD_mlbench(method,...)}

\arguments{
	\item{method}{The name of the mlbench datagen.}
	\item{..,}{Parameters for the mlbench datagen.}
}

\details{
The \code{DSD_mlbench} class is designed to be a wrapper class for data created
by datagens in the mlbench library.

It works like the other DSD classes--by calling \code{get_points()} to retrieve
data from the stream. The following mlbench datagens that are supported are the
following: \code{2dnormals}, \code{cassini}, \code{circle}, \code{cuboids}, \code{friedman1}, \code{friedman2}, \code{friedman3}, \code{hypercube}, \code{peak}, \code{ringnorm}, \code{shapes}, \code{simplex}, \code{smiley}, \code{spirals}, \code{threenorm}, \code{twonorm}, \code{waveform}, \code{xor}.
}

\value{
Returns a \code{DSD_Wrapper} object which is a list of the defined parameters. The parameters are either passed in from the function or created internally. They include: 

	\item{description}{The name of the class of the DSD object.}
	\item{method}{The name of the mlbench datagen.}
	\item{variables}{The variables for the mlbench data gen.}
}

\examples{
dsd <- DSD_mlbench(method="cassini")
points <- get_points(dsd,n=1000,assignment=TRUE)

plot(points,col=attr(points,"assignment"))
}
