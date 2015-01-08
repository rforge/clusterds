\name{DSC_StreamKM}
\alias{DSC_StreamKM}
\title{DataStreamClusterer: StreamKM}
\description{Creates a new DataStreamClusterer with the StreamKM++ algorithm}

\usage{
DSC_StreamKM(sizeCoreset=100, k=5, width=1000, randomSeed=NULL)
}

\arguments{
	\item{sizeCoreset}{The size of the coreset.}
	\item{k}{The number of clusters to compute.}
	\item{width}{Size of Window for training learner.}
	\item{randomSeed}{The random seed used in Java (default is a
	random seed).}
}

\details{
    Applies the k-means++ algorithm to data streams. Rather than use all of
    the stream data, a random sampling is taken then the k-means++
    algorithm is performed on the sample.  The authors also propose a new
    data structure called a coreset tree to reduce the sampling time during
    the construction of the coreset.
}

\value{
A list of class \code{DSC}, \code{DSC_MOA}, and \code{DSC_StreamKM}. The list
contains the following items:

\item{description}{The name of the algorithm in the DSC object.}
\item{options}{The CLI params defined when creating the DSC object.}
\item{javaObj}{The underlying Java object associated with DSC_MOA objects.}
}

\references{
Marcel R. Ackermann, Christiane Lammersen, Marcus M{\"a}rtens, Christoph
Raupach, Christian Sohler, Kamil Swierkot. StreamKM++: A Clustering
Algorithm for Data Streams. In Proceedings of the 12th Workshop on
Algorithm Engineering and Experiments (ALENEX '10), pp. 173-187, Society
for Industrial and Applied Mathematics, 2010. 
}

\seealso{
	\code{\link{DSClusterer}},
	\code{\link{DSC_MOA}}
}

\examples{
# 3 clusters with 5% noise
stream <- DSD_Gaussians(k=3, d=2, noise=0.05)

streamkm <- DSC_StreamKM(k=3, sizeCoreset=100, width=100)
streamkm <- DSC_StreamKM(k=5, sizeCoreset=10000, width=100000)
update(streamkm, stream, 500)
streamkm

# plot micro-clusters
plot(streamkm, stream)
  
# plot the micro-cluster assignment area
plot(denstream, stream, assignment=TRUE, weights=FALSE)

# show macro-clusters (using density reachability with epsilon x offline)
plot(denstream, stream, type="both")

# reclustering. DenStream micro-clusters with k-means instead
km <- DSC_Kmeans(k=3, weighted=TRUE)
recluster(km, denstream)
plot(km, stream, type="both")
dsd <- DSD_Gaussian_Static(k=3, noise=.05)

dsc <- DSC_StreamKM(sizeCore=10, k=3, width=100)
cluster(dsc, dsd, 500)
dsc

# plot micro-clusters
plot(get_points(dsd, 500), col="gray", main = "StreamKM")
centers <- get_centers(dsc)
points(centers, col="red", cex=5)

# reclustering
cl <- kmeans(centers, centers=3)
points(cl$centers, col="blue", cex=15)




}
