\name{DSD_Static}
\alias{DSD_Static}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DSD_Static(mu = c(0.5, 0.5), sd = c(0.2, 0.2), clusterProb = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
}
  \item{clusterProb}{
%%     ~~Describe \code{clusterProb} here~~
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
function (mu = c(0.5, 0.5), sd = c(0.2, 0.2), clusterProb = 1) 
{
    mu <- as.matrix(mu)
    sd <- as.matrix(sd)
    if (ncol(mu) != ncol(sd) || nrow(mu) != nrow(sd)) {
        stop("mu matrix is a different size than sd matrix")
    }
    if (length(clusterProb) != ncol(mu)) {
        prob <- 1/ncol(mu)
        clusterProb <- as.vector(array(prob, ncol(mu)))
    }
    l <- list(Description = "Static R Data Stream", mu = mu, 
        sd = sd, clusterProb = clusterProb)
    class(l) <- "staticDS"
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
