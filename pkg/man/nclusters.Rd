\name{nclusters}
\alias{nclusters}
\title{nclusters}
\description{Returns the number of micro-clusters from the DSC object.}

\usage{
	nclusters(x)
}

\arguments{
	\item{x}{A DSC object.}
}

\details{
This function will return the number of micro-clusters from the DSC object if there are any, or if they are supported. This is done by calling \code{get_centers()} and retrieving the number of rows in the return matrix.
}

\value{
An integer; the number of micro-clusters in the clustering.
}

\seealso{
	\code{\link{DSClusterer}}%,
	%\code{\link{DSC_CobWeb}},
	%\code{\link{DSC_ClusTree}},
	%\code{\link{DSC_StreamKM}},
	%\code{\link{DSC_DenStream}},
	%\code{\link{DSC_CluStream}},
	\code{\link{get_centers}}
}

\examples{
# setting up the objects
dsd <- DSD_Gaussian_Static()
dsc <- DSC_DenStream(initPoints=100)
cluster(dsc, dsd, 500)

# retrieving the results
get_centers(dsc)
nclusters(dsc)
}
