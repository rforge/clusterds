\name{DSD_MOA}
\alias{DSD_MOA}
\title{DataStreamData - MOA}

\description{
A DataStreamData that generates random data based upon the
RandomRBFGenerator implemented in MOA.
}

\usage{
DSD_MOA(k=3, d=2, avgRadius=0, modelSeed=1, instanceSeed=1)
}

\arguments{
	\item{k}{The number of clusters.}
	\item{d}{The dimensionality of the data.}
	\item{avgRadius}{The average radius of the micro-clusters.}
	\item{modelSeed}{Random seed for the model.}
	\item{instanceSeed}{Random seed for the instances.}
}

\details{
}

\value{
	\item{description}{The name of the class of the DSD object.}
	\item{options}{The CLI params defined when creating the DSD object.}
	\item{javaObj}{The underlying Java object associated with DSD_MOA objects.}
}

\references{
Albert Bifet, Geoff Holmes, Bernhard Pfahringer, Philipp Kranen, Hardy Kremer,
Timm Jansen, Thomas Seidl. Journal of Machine Learning Research (JMLR)
Workshop and Conference Proceedings. Volume 11: Workshop on Applications of
Pattern Analysis (2010).
}

\seealso{
	\code{\link{DSD_Gaussian_Static}},
	\code{\link{DSD_ReadStream}},
%%	\code{\link{DSD_Gaussian_Dynamic}},
	\code{\link{DSD_DataFrame}},
	\code{\link{write_stream}}
}

\examples{
}
