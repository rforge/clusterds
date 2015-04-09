\name{DSD_Transactions_Random}
\alias{DSD_Transactions_Random}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Transaction Data Stream Generator
}
\description{
A datastream generator that produces random transactions within a specified range.
}
\usage{
DSD_Transactions_Random <- function(type=c("integer"), setSize=50, maxTransactionSize=10, 
                                    prob = function(set) rep(1/set, times=set),
                                    size = function(maxSize) sample(1:maxSize, 1) )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
  The type of transaction data to produce. Currently integer is the only option
}
  \item{setSize}{
  The size of the set to create transactions from
}
  \item{maxTransactionSize}{
  The maximum size for any one transaction
}
  \item{prob}{
    function describing the probability for each item in the itemset
  }
  \item{size}{
    function describing the probably size of any individual transaction
  }
}
\details{
DSD_Transactions_Random creates a DSD that generates random transactions with a specified set size and max transaction size. It allows for the use of custom functions for the probability of each number in that range, and the probable length of each transaction.
}
\value{
Returns a \code{DSD_Transactions_Random} 
object (subclass of 
\code{DSD_Transactions}, \code{DSD}) which is a list of the defined
params. The params are either passed in from the function or
created internally
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
#create random transaction datastream
dsd <- DSD_Transactions_Random()

#create with a set size of 50 and a max transaction size of 10
dsd <- DSD_Transactions_Random(setSize=50, maxTransactionSize=10)

#create with custom size function
dsd <- DSD_Transactions_Random(setSize= 50, maxTransactionSize= 10 , size=function(max) rexp(max))

#get a transaction from the datastream
transaction <- get_points(dsd)

transaction
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

