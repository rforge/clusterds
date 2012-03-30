\name{Recluster}
\alias{Recluster}
\title{Recluster}

\description{
Takes micro-clusters and creates macro-clusters using various clustering algorithms.
}

\usage{
recluster(dsc,method = "kmeans",k = 2,eps = .1)
}

\arguments{
	\item{dsc}{The DSC object that the user wishes to recluster.}
	\item{method}{The type of reclustering the user wants to use.}
	\item{k}{The number of macro-clusters for kmeans and hierarchical clustering.}
	\item{eps}{Epsilon for dbscan}
}

\details{
	\code{recluster} allows the user to recluster a set of micro-clusters. By default kmeans is used to 	recluster the data, but hierarchical and dbscan can also be used.
}

\value{
	\item{assignment}{The assignment of the new recluster.}
	\item{details}{The full details returned by each method.}
}

\examples{
	#Create stream
	dsd <- DSD_Gaussian_Static(k=3, d=2)
	
	d <- get_points(dsd,1000, assignment = TRUE)
	
	#Create cluster
	x <-DSC_tNN(, threshold=.4)
	cluster(x, dsd, 1000)
	
	x <- recluster(x,method="kmeans",k=3)
}
