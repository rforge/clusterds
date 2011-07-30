\name{DSC_CobWeb}
\alias{DSC_CobWeb}
\title{DataStreamClusterer: CobWeb}
\description{Creates a new DataStreamClusterer with the CobWeb algorithm}

\usage{
DSC_CobWeb(acuity = 1, cutoff = 0.002, randomSeed = NULL)
}

\arguments{
  \item{acuity}{The acuity used in the underlying implementation.}
  \item{cutoff}{The cut off.}
  \item{randomSeed}{The random seed used by the Java implementation 
  (\code{NULL} creates a random seed).}
}

\details{
An incremental clustering technique using classification trees applied to data streams.
}

\value{
A list of class \code{DSC}, \code{DSC_MOA}, and \code{DSC_CobWeb}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}

\references{
Douglas H. Fisher. 1987. Knowledge Acquisition Via Incremental Conceptual
Clustering. Mach. Learn. 2, 2 (September 1987), 139-172.
}

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_MOA}}
}

\examples{
dsc <- DSC_CobWeb()
dsd <- DSD_Gaussian_Static()
cluster(dsc, dsd, 500)

#CobWeb doesn't implement micro-clusters
}
