\name{DSC_CobWeb}
\alias{DSC_CobWeb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DataStreamClusterer: CobWeb}
\description{Creates a new DataStreamClusterer with the CobWeb algorithm}
\usage{
DSC_CobWeb(acuity = 1, cutoff = 0.002, randomSeed = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acuity}{}
  \item{cutoff}{}
  \item{randomSeed}{}
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
dsc <- DSC_CobWeb()
dsd <- DSD_moa()
cluster(dsc, dsd, 100000)
plot(getCenters(dsc))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
