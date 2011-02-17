\name{cluster}
\alias{cluster}
\title{Clustering data based with input algorithm and data stream}
\description{Clusters a number of input points from a data stream into a clustering object.}
\usage{
cluster(clusterer, x, numPoints = 10000)
}
\arguments{
  \item{clusterer}{a DSC object}
  \item{x}{a DSD object}
  \item{numPoints}{number of points}
}
\details{
  Clusters input data. The underlying Java object in the DSC object is updated
  from the data stream data.
}
\value{  No return value, the input DSC object is updated (similar to being passed by reference)}
%%\references{ ~put references to the literature/web site here ~}
%%\author{ ~~who you are~~}
%%\note{ ~~further notes~~}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
dsc <- DSC_DenStream()
dsd <- DSD_moa()
cluster(dsc, dsd, 100000)
plot(getCenters(dsc))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
