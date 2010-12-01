\name{get_centroids}
\alias{get_centroids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Returning the centroids from a Clustering instance}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_centroids(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{ClusterDS} object.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix that holds the centroids for each microcluster in the Clustering.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## creating a datastream and clusterer
ds <- create_rStream()
d <- DenStream()

## clustering some data
cluster(clusterer=d, x=ds, numPoints=10000)

## plots the microclusters
plot(get_centroids(d))
}
