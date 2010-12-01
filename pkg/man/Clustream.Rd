\name{Clustream}
\alias{Clustream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Clustream(timeWindow = 1000, maxNumKernels = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timeWindow}{
%%     ~~Describe \code{timeWindow} here~~
}
  \item{maxNumKernels}{
%%     ~~Describe \code{maxNumKernels} here~~
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
function (timeWindow = 1000, maxNumKernels = 100) 
{
    if (timeWindow < 0) 
        stop("invalid timeWindow, must be > 0")
    if (maxNumKernels < 0) 
        stop("invalid maxNumKernels, must be > 0")
    paramList <- list(t = timeWindow, k = maxNumKernels)
    cliParams <- convertParams(paramList)
    clusterer <- .jnew("moa/clusterers/clustream/Clustream")
    .jcall(clusterer, "V", "prepareForUse")
    options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
    .jcall(options, "V", "setViaCLIString", cliParams)
    l <- list(description = "Clustream", options = cliParams, 
        javaObj = clusterer)
    class(l) <- "ClusterDS"
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
