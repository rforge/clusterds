\name{DSD_MOA}
\alias{DSD_MOA}
\title{DSD_RandomRBFGeneratorEvents}

\description{
A DataStreamData that generates random data based upon the
DSD_RandomRBFGeneratorEvents implemented in MOA.
}

\usage{
DSD_RandomRBFGeneratorEvents(k=3, d=2, avgRadius=0, modelSeed=1, instanceSeed=1)
}

\arguments{
	\item{k}{The number of clusters.}
	\item{d}{The dimensionality of the data.}
	\item{avgRadius}{The average radius of the micro-clusters.}
	\item{modelSeed}{Random seed for the model.}
	\item{instanceSeed}{Random seed for the instances.}
}

\details{
There are an assortment of parameters available for the underlying MOA data
structure, however, we have currently limited the available parameters to
the arguments above.  Currently the modelSeed and instanceSeed are set to
default values every time a \code{DSD_MOA} is created, therefore the
generated data will be the same. Because of this, it is important to set
the seed manually when different data is needed.

The default behavior of \code{DSD_MOA} is to create a data stream with 3
clusters and concept drift.  The locations of the clusters will change
slightly, and they will merge with one another as time progresses.  MOA is
a good DSD to use for testing an algorithms behavior on dynamic data.
}

\value{
	\item{description}{The name of the class of the DSD object.}
	\item{options}{The CLI params defined when creating the DSD object.}
	\item{javaObj}{The underlying Java object associated with DSD_MOA objects.}
}

\references{
MOA: Massive Online Analysis, a Framework for Stream Classification and
Clustering Albert Bifet, Geoff Holmes, Bernhard Pfahringer, Philipp Kranen,
Hardy Kremer, Timm Jansen, Thomas Seidl.  Journal of Machine Learning
Research (JMLR) Workshop and Conference Proceedings.  Volume 11: Workshop
on Applications of Pattern Analysis (2010).
}
\seealso{
	\code{\link{DSData}}
}
\examples{
dsd <- DSD_RandomRBFGeneratorEvents()
dsd

plot(get_points(dsd, 500))
}
