\name{DSD_ReadStream}
\alias{DSD_ReadStream}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{DataStreamData: ReadStream (from an open connection)}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DSD_ReadStream(x, delimiter = ",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{delimiter}{
%%     ~~Describe \code{delimiter} here~~
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
function (x, delimiter = ",") 
{
    if (class(x) == "character") {
        x <- file(x)
        open(x)
    }
    else if (class(x) != "connection") {
        stop("please pass a valid connection")
    }
    else if (isOpen(x) == "FALSE") {
        open(x)
    }
    l <- list(Description = "File Data Stream", con = x, delimiter = delimiter)
    class(l) <- "fDS"
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
