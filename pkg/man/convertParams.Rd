\name{convertParams}
\alias{convertParams}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converting CLI style params to a list}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convertParams(paramList = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paramList}{
%%     ~~Describe \code{paramList} here~~
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
function (paramList = list()) 
{
    length <- length(paramList)
    if (length == 0) 
        stop("invalid param list")
    cliParams <- ""
    for (i in 1:length) {
        cliParams <- paste(cliParams, "-", names(paramList[i]), 
            " ", paramList[[i]], " ", sep = "")
    }
    cliParams <- substr(cliParams, 1, nchar(cliParams) - 1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
