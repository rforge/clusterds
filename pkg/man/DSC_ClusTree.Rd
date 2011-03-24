\name{DSC_ClusTree}
\alias{DSC_ClusTree}
\title{DataStreamClusterer: ClusTree}
\description{Creates a new DataStreamClusterer with the ClusTree algorithm}

\usage{
	DSC_ClusTree(timeWindow = 1000, minHeight=8)
}

\arguments{
	\item{timeWindow}{}
	\item{minHeight}{}
}

\details{
}

\value{
	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}

%%\references{ } TODO: need to reference the original paper

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_MOA}},
	\code{\link{DSC_DenStream}},
	\code{\link{DSC_CobWeb}},
	\code{\link{DSC_StreamKM}},
	\code{\link{DSC_Clustream}}
}

\examples{
dsc <- DSC_ClusTree()
dsd <- DSD_Gaussian_Static()
cluster(dsc, dsd, 5000)
plot(dsc)
}
