\name{get_copy}
\alias{get_copy}
\title{get_copy}

\description{
Gets the centers of micro-clusters (if available) from a DSC object
}

\usage{
	get_copy(x)
}

\arguments{
	\item{x}{The DSC object being copied.}
}

\details{
Each DSC object is passed by reference. To create a new DSC object this method
needs to be used to create an individual DSC object.

This is helpful when dealing with a macro clusterer because it allows you to avoid
overriding useful data.
}

\value{
A copy of the original DSC.
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
dsc <- DSC_Kmeans(k = 3)
cluster(dsc, dsd, 500)

# getting the copy
dsc_copy <- get_copy(dsc)
}
