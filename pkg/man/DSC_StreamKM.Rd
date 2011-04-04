\name{DSC_StreamKM}
\alias{DSC_StreamKM}
\title{DataStreamClusterer: StreamKM}
\description{Creates a new DataStreamClusterer with the StreamKM++ algorithm}

\usage{
DSC_StreamKM(sizeCoreset=100, k=5, width=1000, randomSeed=1)
}

\arguments{
	\item{sizeCoreset}{The size of the coreset tree.}
	\item{k}{The number of clusters.}
	\item{width}{}
	\item{randomSeed}{The random seed used in the k-means algorithm.}
}

\details{
Applies the k-means++ algorithm to data streams. Rather than use all of the stream data,
a random sampling is taken then the k-means++ algorithm is performed on the sample.
The authors also propose a new data structure called a coreset tree to reduce the
sampling time during the construction of the coreset.
}

\value{
A list of class \code{DSC}, \code{DSC_MOA}, and \code{DSC_StreamKM}. The list
contains the following items:

	\item{description}{The name of the algorithm in the DSC object.}
	\item{options}{The CLI params defined when creating the DSC object.}
	\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}

\references{
Marcel R. Ackermann, Christiane Lammersen, Marcus M{\"a}rtens, Christoph Raupach,
Christian Sohler, Kamil Swierkot. StreamKM++: A Clustering Algorithm for Data
Streams. In Proceedings of the 12th Workshop on Algorithm Engineering and
Experiments (ALENEX '10), pp. 173-187, Society for Industrial and Applied
Mathematics, 2010. 
}

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_MOA}},
	\code{\link{DSC_DenStream}},
	\code{\link{DSC_CobWeb}},
	\code{\link{DSC_Clustream}},
	\code{\link{DSC_ClusTree}}
}

\examples{
#dsc <- DSC_StreamKM()
#dsd <- DSD_Gaussian_Static()
#cluster(dsc, dsd, 5000)
#plot(dsc)
}
