\name{DST_EstDec}
\alias{DST_EstDec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{estDec Recent Frequent Pattern Miner}
\description{
 Implements the estDec algorithm for finding recent frequent patterns in transactional datastreams.
}
\usage{
DST_EstDec(decayRate = 0.2, Dk = 0, minsup = 0.3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{decayRate}{
  The decay rate for patterns found. As this number increases, the patterns found by estDec get more recent,
  and if it is close to 0, patterns found cover more of the transaction stream's history.
}
  \item{Dk}{
%%     ~~Describe \code{Dk} here~~
}
  \item{minsup}{
  The minimum support needed for a itemset to be considered frequent.
}
}
\details{
estDec finds frequent patterns over a transaction or list datastream. It only keeps track of itemsets that it considers recently frequent based off of the given decayRate and minsup.
}
\value{
    An object of class \code{DST_EstDec} (subclass of \code{DST}
}
\references{
}
\author{
  
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

#create datastream
dsd <- DSD_Transactions_Random(setSize=10, maxTransactionSize=4)

#create estDec
dst <- DST_EstDec()

#update estDec with new values from data stream
update(dst, dsd, 5)

#get most frequent itemsets
patterns <- get_patterns.DST_EstDec(dst)

#get the top ten most frequent patterns
topN.DST_Patterns(patterns, n=10)


}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
