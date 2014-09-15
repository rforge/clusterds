\name{update}
\alias{update}
\alias{update.DSC}
\alias{cluster}
\title{Update a Data Stream Clustering Model}

\description{
Update a clustering model by clustering a number of input points from a data stream into a clustering object.
}

\usage{
\method{update}{DSC}(object, dsd, n = 1, verbose = FALSE, ...)
cluster(dsc, dsd, n = 1, verbose = FALSE, ...)
}

\arguments{
  \item{object}{an object of a subclass of DST (data stream mining task).}
  \item{dsc}{a DSC object (clusterer).}
  \item{dsd}{a DSD object (data stream).}
  \item{n}{number of points to cluster.}
  \item{verbose}{report progress.}
  \item{...}{extra arguments for clusterer.}
}

\details{
\code{update} takes n times a single 
data points out of the DSD updates the model in \code{object}. 
Note that update directly 
modifies the object (which is a reference class) and 
thus the result does not need to be reassigned to the object name.

\code{cluster} is the low level implementation of updating a
data stream clustering model and is called by \code{update}.
}

\value{
The updated model is returned invisibly for reassignment
(however, this is not necessary).  

To obtain the updated model for a \code{DSC} (data stream clustering
model), call \code{get_centers()} on the DSC object.
}

\seealso{
	\code{\link{DSC}},
	\code{\link{DSD}}, and
	\code{\link{animation}} for producing an animation of the clustering process.
}

\examples{
dsd <- DSD_Gaussians(k=3)
dsc <- DSC_DStream(gridsize=.05)  

update(dsc, dsd, 500)
plot(dsc, dsd)
}
