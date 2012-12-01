\name{get_points}
\alias{get_points}
\title{get_points}

\description{
Gets points from a DSD object.
}

\usage{
	get_points(x, n=1, ...)
}

\arguments{
	\item{x}{The DSD object.}
	\item{n}{The number of data points being requested.}
	\item{...}{Additional parameters to pass to \code{get_points()}}
}

\details{
Each DSD object has a unique way for returning data points, but they all are
called through the generic function, \code{get_points()}. This is done by
using the S3 class system. See the man page for the specific DSD class on
the semantics for each implementation of \code{get_points()}.
}

\value{
Returns a matrix of \code{x$d} columns and \code{n} rows.
}

\seealso{
	\code{\link{DSData}}
}

\examples{
dsd <- DSD_GaussianStatic()
d <- get_points(dsd, 100)
}
