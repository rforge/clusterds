\name{get_centers}
\alias{get_centers}
\title{get_centers}

\description{
Gets the centers of micro-clusters (if available) from a DSC object
}

\usage{
	get_centers(x, ...)
}

\arguments{
	\item{x}{The DSC object the centers are being requested from.}
	\item{...}{Additional parameters to pass to \code{get_centers()}.}
}

\details{
Each DSC object has a unique way for returning data points, but they all are
called through the generic function, \code{get_centers()}. This is done by
using the S3 class system. 

All of the \code{DSC_MOA} classes use the same
function that first checks for the existence of micro-clusters (as deemed in
the MOA framework), extracts those micro-clusters, then manually extracts the
center from each of those micro-clusters and packages them into a matrix.
}

\value{
A matrix of data that contains the centers of the micro-clusters (these can be
either the centroids or medoids depending on the algorithm). There are some
DSC classes that do not generate micro-clusters--an error will be given in these
cases.

The number of columns in the matrix is dependent on the data stream that was used as input data to the DSC object, and the number of rows will differ upon the algorithm being used.
}

\seealso{
	\code{\link{DSClusterer}},
	%\code{\link{DSC_CobWeb}},
	%\code{\link{DSC_ClusTree}},
	%\code{\link{DSC_StreamKM}},
	%\code{\link{DSC_DenStream}},
	%\code{\link{DSC_CluStream}},
	\code{\link{DSData}}
}

\examples{
# setting up the objects
dsd <- DSD_Gaussian_Static()
dsc <- DSC_DenStream(initPoints=100)
cluster(dsc, dsd, 500)

# getting the centers
d <- get_centers(dsc)
}
