\name{DSC_ClusTree}
\alias{DSC_ClusTree}
\title{DataStreamClusterer: ClusTree}

\description{
Creates a new DataStreamClusterer with the ClusTree algorithm
}

\usage{
	DSC_ClusTree(timeWindow = 1000, maxHeight=8)
}

\arguments{
	\item{timeWindow}{The timeWindow parameter used in MOA.}
	\item{maxHeight}{The maximum height of the tree.}
}

\details{
The authors of ClusTree define it as a "parameter free algorithm". Their 
innovation automatically adjusts to the speed of the incoming points generated
by a data stream. ClusTree is able to detect concept drift and outliers in
the stream being operated upon.
}

\value{
A list of class \code{DSC}, \code{DSC_MOA}, and \code{DSC_ClusTree}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}

\references{
Philipp Kranen, Ira Assent, Corinna Baldauf, and Thomas Seidl. 2009.
Self-Adaptive Anytime Stream Clustering. In Proceedings of the 2009 Ninth IEEE
International Conference on Data Mining (ICDM '09). IEEE Computer Society,
Washington, DC, USA, 249-258. DOI=10.1109/ICDM.2009.47
\url{http://dx.doi.org/10.1109/ICDM.2009.47}
}

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_MOA}}%,
	%\code{\link{DSC_DenStream}},
	%\code{\link{DSC_CobWeb}},
	%\code{\link{DSC_StreamKM}},
	%\code{\link{DSC_CluStream}}
}

\examples{
dsc <- DSC_ClusTree()
dsd <- DSD_Gaussian_Static()
cluster(dsc, dsd, 100)
dsc

plot(get_points(dsd,1000), col="gray")
points(get_centers(dsc), col="red", cex=5)
}
