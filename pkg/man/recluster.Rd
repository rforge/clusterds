\name{recluster}
\alias{recluster}
\title{Re-clustering micro-clusters}

\description{Use a macro clustering algorithm to recluster micro-clusters into a final clustering.
}

\usage{
recluster(macro, dsc, ...)
}

\arguments{
  \item{macro}{a macro clustering algorithm (class "DSC_Macro")}
  \item{dsc}{a DSC object containing micro-clusters. }
  \item{...}{additional arguments passed on.}
}

\details{
Takes centers and weights of the micro-clusters and applies the macro clustering algorithm.
}

\value{
The object macro is altered and contains the clustering.
}


%\seealso{
%}

\examples{
### create a data stream and a micro-clustering
dsd <- DSD_Gaussian_Static(k=3)

micro <- DSC_DenStream(initPoints=100)
cluster(micro, dsd, 1000)

### recluster using weighted k-means
macro <- DSC_KmeansW(k=3)
recluster(macro, micro)


### plot clustering
plot(get_points(dsd, 1000), col="gray", cex=.5)
### show micro-clusters (size is proportianal to each  cluster's weight)
points(get_centers(micro), col="blue", cex=get_weights(micro, c(2,5)), lwd=2)
### show macro-clusters
points(get_centers(macro), col="red", cex=10, lwd=2)
}
