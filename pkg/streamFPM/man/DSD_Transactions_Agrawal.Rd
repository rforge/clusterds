\name{DSD_Transactions_Agrawal}
\alias{DSD_Transactions_Agrawal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DSD Agrawal
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DSD_Transactions_Agrawal(type = c("integer"), setSize = 50, maxTransactionSize = 10, distribution = "", ..., verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{setSize}{
%%     ~~Describe \code{setSize} here~~
}
  \item{maxTransactionSize}{
%%     ~~Describe \code{maxTransactionSize} here~~
}
  \item{distribution}{
%%     ~~Describe \code{distribution} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (type = c("integer"), setSize = 50, maxTransactionSize = 10, 
    distribution = "", ..., verbose = FALSE) 
{
    nItems <- setSize
    patterns <- random.patterns(nItems = nItems, method = "agrawal", 
        ..., verbose = verbose)
    l <- list(description = "Agrawal Transaction Data Stream", 
        type = type, setSize = setSize, maxTransactionSize = maxTransactionSize, 
        distribution = distribution, patterns = patterns)
    class(l) <- c("DSD_Agrawal", "DSD_Transactions", "DSD_List", 
        "DSD_R", "DSD")
    l
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
