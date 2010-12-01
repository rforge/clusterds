\name{get_centroids}
\alias{get_centroids}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_centroids(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, ...) 
{
    if (.jcall(x$javaObj, "Z", "implementsMicroClusterer")) {
        mClustering <- .jcall(x$javaObj, "Lmoa/cluster/Clustering;", 
            "getMicroClusteringResult")
        mClusters <- .jcall(mClustering, "Lmoa/core/AutoExpandVector;", 
            "getClustering")
        length <- .jcall(mClusters, "I", "size")
        if (length < 2) 
            stop("not enough microclusters, please cluster more data")
        m <- matrix(ncol = 2, nrow = length)
        colnames(m) <- c("x", "y")
        for (i in 1:length) {
            mCluster <- .jcall(mClusters, "Ljava/lang/Object;", 
                "get", i - 1L)
            mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")
            center <- .jcall(mCluster, "[D", "getCenter")
            m[i, 1] = center[1]
            m[i, 2] = center[2]
        }
        m
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
