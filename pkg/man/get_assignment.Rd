\name{get_assignment}
\alias{get_assignment}
\title{get_assignment}
\description{Get the assignment of points for a particular \code{DSC}.}

\usage{
	get_assignment(dsc,points,n)
}

\arguments{
	\item{dsc}{The DSC object the assignments are being requested from.}
	\item{points}{A matrix containing a list of points that need to be assigned.}
	\item{n}{The number of points being assigned to the DSC object}
}

\details{
Each DSC object has a unique way for returning assignments, but they all are
called through the generic function, \code{get_assignment()}. This is done by
using the S3 class system. 

This function ensures the assignment for each point is correctly calculated for
each DSC object.
}

\value{
A vector containing the assignment of each point.
}

\seealso{
	\code{\link{DSClusterer}}
}
