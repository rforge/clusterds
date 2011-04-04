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
There are an assortment of parameters available for the underlying MOA data structure,
however, we have currently limited the available parameters to the arguments above.
Currently the modelSeed and instanceSeed are set to default values every time a 
\code{DSD_MOA} is created, therefore the generated data will be the same. Therefore,
it is important to set the seed manually when different data is needed.
}

\value{
	\item{description}{The name of the class of the DSD object.}
	\item{options}{The CLI params defined when creating the DSD object.}
	\item{javaObj}{The underlying Java object associated with DSD_MOA objects.}
}

\references{
MOA: Massive Online Analysis, a Framework for Stream Classification and Clustering
Albert Bifet, Geoff Holmes, Bernhard Pfahringer, Philipp Kranen, Hardy Kremer, Timm Jansen, Thomas Seidl.
Journal of Machine Learning Research (JMLR) Workshop and Conference Proceedings.
Volume 11: Workshop on Applications of Pattern Analysis (2010).
}

\seealso{
	\code{\link{DSD_Gaussian_Static}},
	\code{\link{DSD_ReadStream}},
%%	\code{\link{DSD_Gaussian_Dynamic}},
	\code{\link{DSD_DataFrame}},
	\code{\link{write_stream}}
}

\examples{
dsc <- DSC_DenStream(initPoints=100)
dsd <- DSD_MOA()
cluster(dsc, dsd, 1000)
get_centers(dsc)
}
