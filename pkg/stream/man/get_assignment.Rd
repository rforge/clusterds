\name{get_assignment}
\alias{get_assignment}
\title{Assignment Data Points to Clusters}
\description{Get the assignment of data points to clusters in a \code{DSC}
(nearest-neighbor).}

\usage{
get_assignment(dsc, points, type=c("auto", "micro", "macro"), 
  method="Euclidean", threshold=NULL, ...)
}

\arguments{
	\item{dsc}{The DSC object with the clusters for assignment.}
	\item{points}{The points to be assigned as a data.frame.}
	\item{type}{Use micro- or macro-clusters in DSC for assignment. Auto
	used the class of dsc to decide.}
	\item{method}{Distance measure used for nearest-neighbor assignment 
  (see \code{dist} in \pkg{proxy} for available measures).}
	\item{threshold}{Assign only points with a distance of less than \code{threshold}
    to a cluster. Points which cannot be assigned return an assignment of \code{NA}.}
	\item{...}{Additional arguments are passed on.}
}

\details{
Each data point is assigned to its nearest cluster using the specified
distance measure.
}


\value{
A vector containing the assignment of each point. \code{NA} means that a
  data point was not assigned to a cluster.
}

\seealso{
	\code{\link{DSC}}
}

\examples{
dsd <- DSD_Gaussians(k=3, d=2, noise=.05)

dsc <- DSC_tNN(r=.05)
cluster(dsc, dsd, n=100)

# find nearest neighbor assignment for the next 10 points to 
# micro-clusters in dsc
points <- get_points(dsd, n=10)
get_assignment(dsc, points)
  
# only assign points which are within threshold around a cluster 
get_assignment(dsc, points, threshold = .05)

plot(dsc, dsd, weights=FALSE)
points(points, pch=is.na(get_assignment(dsc, points, threshold = .05))+2L)
# Note: unassigned points are shown as black plus signs
}