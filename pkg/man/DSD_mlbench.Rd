\name{DSD_mlbench}
\alias{DSD_mlbench}
\title{mlbench Data Stream Generator}
\description{
A data stream generator class that wraps data generators found in mlbench. 
}

\usage{DSD_mlbench(method, ...)}

\arguments{
	\item{method}{The name of the mlbench datagen.}
	\item{...}{Parameters for the mlbench datagen.}
}

\details{
The \code{DSD_mlbench} class is designed to be a wrapper class for data created
by data generators in the mlbench library.

Call \code{DSD_mlbench} with missing method to get a list of available 
methods.
}

\value{
Returns a \code{DSD_mlbench} object (subclass of \code{DSD_R}, \code{DSD})
 which is a list of the defined parameters. The parameters are either passed
in from the function or created internally. They include: 

	\item{description}{The name of the class of the DSD object.}
	\item{method}{The name of the mlbench datagen.}
	\item{variables}{The variables for the mlbench data gen.}
}
\seealso{
        \code{\link{DSD}}
}

\examples{
dsd <- DSD_mlbench(method="cassini")

plot(dsd, n=500)
}
