\name{DSC_StreamKM}
\alias{DSC_StreamKM}
\title{DataStreamClusterer: StreamKM}
\description{Creates a new DataStreamClusterer with the StreamKM algorithm}

\usage{
	DSC_StreamKM(sizeCoreset=100, k=5, width=1000, randomSeed=1)
}

\arguments{
	\item{sizeCoreset}{}
	\item{k}{}
	\item{width}{}
	\item{randomSeed}{}
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
	\code{\link{DSC_Clustream}},
	\code{\link{DSC_ClusTree}}

}

\examples{
dsc <- DSC_StreamKM()
dsd <- DSD_Gaussian_Static()
cluster(dsc, dsd, 5000)
plot(dsc)
}
