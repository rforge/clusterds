\name{cluster}
\alias{cluster}
\title{Clustering data based with input algorithm and data stream}
\description{Clusters a number of input points from a data stream into a clustering object.}
\usage{
cluster(dsc, dsd, n = 10000)
}
\arguments{
  \item{dsc}{a DSC object}
  \item{dsd}{a DSD object}
  \item{n}{number of points to cluster}
}
\details{
  Clusters input data. The underlying clustering in the DSC object is implicitly updated
  as if the object was 'passed by reference' in a traditional programming language.
}
\value{ The updated DSC object is returned invisibly for reassignment. }
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
plot(dsc)
}
