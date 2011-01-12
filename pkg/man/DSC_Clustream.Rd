\name{DSC_Clustream}
\alias{DSC_Clustream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DataStreamClusterer: Clustream}
\description{Creates a new DataStreamClusterer with the Clustream algorithm}
\usage{
DSC_Clustream(timeWindow = 1000, maxNumKernels = 100)
}
\arguments{
  \item{timeWindow}{Defines the time window to be used in Clustream}
  \item{maxNumKernels}{Defines the maximum number of kernels used in Clustream}
}
\details{}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{ ~put references to the literature/web site here ~}
\author{ ~~who you are~~}
\note{ ~~further notes~~}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dsc <- DSC_Clustream()
dsd <- DSD_moa()
cluster(dsc, dsd, 100000)
plot(getCenters(dsc))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
