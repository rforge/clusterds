\name{DSD_Transactions_Agrawal}
\alias{DSD_Transactions_Agrawal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DSD Agrawal
}
\description{
A datastream generator that produces semi-random transactions within a specified range. Some items will be more frequent by design. Algorithm originally developed for IBM by Agrawal.
}
\usage{
DSD_Transactions_Agrawal(type = c("integer"), setSize = 50,
  maxTransactionSize = 10, ..., verbose = FALSE)
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
  \item{verbose}{
  How much information the algorithm prints to the console.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a \code{DSD_Transactions_Agrawal} 
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
\dontrun{
#create agrawal transaction datastream
dsd <- DSD_Transactions_Agrawal()

#create with a set size of 50 and a max transaction size of 10
dsd <- DSD_Transactions_Agrawal(setSize=50, maxTransactionSize=10)

#create with custom size function
dsd <- DSD_Transactions_Agrawal(setSize= 50, maxTransactionSize= 10 , size=function(max) rexp(max))

#get a transaction from the datastream
transaction <- get_points(dsd)

transaction
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
