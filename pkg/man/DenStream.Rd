\name{DenStream}
\alias{DenStream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creating a DenStream instance}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DenStream(epsilon = 0.1, minPoints = 10, lambda = 0.006, beta = 0.001, mu = 1, initPoints = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{epsilon}{
%%     ~~Describe \code{epsilon} here~~
}
  \item{minPoints}{
%%     ~~Describe \code{minPoints} here~~
}
  \item{lambda}{
%%     ~~Describe \code{lambda} here~~
}
  \item{beta}{
%%     ~~Describe \code{beta} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{initPoints}{
%%     ~~Describe \code{initPoints} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (epsilon = 0.1, minPoints = 10, lambda = 0.006, beta = 0.001, 
    mu = 1, initPoints = 10000) 
{
    if (epsilon <= 0 || epsilon >= 1) 
        stop("invalid epsilon, range: 0 to 1 exclusive")
    if (minPoints < 10) 
        stop("invalid minPoints, must be > 0")
    if (lambda <= 0 || lambda >= 1) 
        stop("invalid lambda, range: 0 to 1 exclusive")
    if (beta <= 0 || beta >= 1) 
        stop("invalid beta, range: 0 to 1 exclusive")
    if (mu <= 0) 
        stop("invalid mu, must be > 0")
    if (initPoints < 0) 
        stop("invalid initPoints, must be > 0")
    paramList <- list(e = epsilon, p = minPoints, l = lambda, 
        b = beta, m = mu, i = initPoints)
    cliParams <- convertParams(paramList)
    clusterer <- .jnew("moa/clusterers/denstream/DenStream")
    .jcall(clusterer, "V", "prepareForUse")
    options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
    .jcall(options, "V", "setViaCLIString", cliParams)
    l <- list(description = "DenStream", options = cliParams, 
        javaObj = clusterer)
    class(l) <- "ClusterDS"
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
