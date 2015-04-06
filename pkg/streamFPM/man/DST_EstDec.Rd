\name{DST_EstDec}
\alias{DST_EstDec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{estDec Recent Frequent Pattern Miner}
\description{
 Implements the estDec algorithm for finding recent frequent patterns in transactional datastreams.
}
\usage{
DST_EstDec <- function(decayRate = 0.99, minsup = 0.1, 
                       insertSupport = NULL, 
                       pruningSupport = NULL,
                       datatype="character") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{decayRate}{
  The decay rate for patterns found. As this number increases, the patterns found by estDec get more recent,
  and if it is close to 0, patterns found cover more of the transaction stream's history.
  }
  \item{minsup}{
  The minimum support needed for a itemset to be considered frequent.
  }
  \item{insertSupport}{
   The minimum estimated support an itemset needs to be inserted into the tree. If no value is specified, it is set at 60\% of \code{minsup}.
  }
  \item{pruningSupport}{
    The minimum support an itemset needs to not be removed from the tree. If no value is specified, it is set at 60\% of \code{minsup}.
  }
  \item{datatype}{
    The datatype used to represent items. The options are "integer" and "character". This depends on the kind of data the DSD used produces.
  }
}
\details{
estDec finds frequent patterns over a transaction or list datastream. It only keeps track of itemsets that it considers recently frequent based off of the given decayRate and minsup.
}
\value{
    An object of class \code{DST_EstDec} (subclass of \code{DST}
}
\references{
  Chang, J., Lee, W. (2003) Finding Recent Frequent Itemsets Adaptively over Online Data Streams.
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
patterns <- get_patterns(dst)

#get the top ten most frequent patterns
topN.DST_Patterns(patterns, n=10)


}
