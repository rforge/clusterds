\name{DSC_DenStream}
\alias{DSC_DenStream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DataStreamClusterer: DenStream}
\description{Creates a new DataStreamClusterer with the DenStream algorithm}
\usage{
DSC_DenStream(epsilon = 0.1, minPoints = 10, lambda = 0.006, beta = 0.001, mu = 1, initPoints = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{epsilon}{-e (defines the epsilon neighborhood, range: 0 to 1)}
  \item{minPoints}{-p (min. num. points a cluster must have)}
  \item{lambda}{-l (range: 0 to 1)}
  \item{beta}{-b (range: 0 to 1)}
  \item{mu}{-m (range: 0 to max(double))}
  \item{initPoints}{-i (number of points to use for initialization)}
}
\details{}
\value{
  \item{description}{The name of the algorithm}
  \item{options}{The CLI parameters that are passed to Java}
  \item{javaObj}{The underlying MOA object created in Java}
}
\references{ ~put references to the literature/web site here ~}
\author{ ~~who you are~~}
\note{ ~~further notes~~}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dsc <- DSC_DenStream()
dsd <- DSD_moa()
cluster(dsc, dsd, 100000)
plot(getCenters(dsc))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
