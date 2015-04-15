\name{Patterns-class}
\docType{class}
\alias{Patterns-class}
\alias{Patterns}
%
\alias{summary,Patterns-method}
\alias{print,Patterns-method}
\alias{as.vector,Patterns-method}

\title{Class ``Patterns'' --- }
\description{
  The \code{Patterns} class represents frequent itemsets found by a frequent pattern mining \code{\link{DST}}s, such as \code{\link{DST_EstDec}}.
}
\section{Slots}{
  \describe{
    \item{\code{transactionInfo}:}{a data.frame
      with vectors of the same length as the number of
      transactions.  Each vector can hold additional information, e.g.,
      store transaction IDs or user IDs for each transaction.}
    \item{\code{data}:}{object of class
      \code{\link[Matrix:ngCMatrix-class]{ngCMatrix}} to store the
      binary incidence matrix (see
      \code{\linkS4class{itemMatrix}} class)} 
    \item{\code{itemInfo}:}{a data.frame to store 
      item labels (see \code{\linkS4class{itemMatrix}} class)}
  }
}
\section{Methods}{
  \describe{
  %  \item{[}{\code{signature(x = "transactions")};
  %    extracts a subset from the incidence matrix.  The first argument
  %    extracts transactions and the second argument can be used to
  %    extract a subset of items (using item IDs)}
    \item{as.itemsets}{\code{signature(patterns, ...)};
      produces an \code{itemsets} object from the \pkg{arules} package.
    }
    \item{as.vector}{\code{signature(patterns, ...)};
      produces a vector of counts with itemsets as names.
    }
    \item{getSets}{\code{signature(patterns)};
      produces a list of the itemsets countained in the \code{Patterns} class, with each itemset represented by a vector of item.
    }
    \item{summary}{\code{signature(patterns)};
    shows a summary of the \code{Patterns} class.
    }
    \item{topN}{\code{signature(patterns, n = 1)};
      returns a vector of the top \code{n} most frequent itemsets.
    }    
    \item{print}{\code{signature(patterns)}; 
      represents the itemsets in a printable form 
  }
}
}
\examples{
#create datastream
dsd <- DSD_Transactions_Random(setSize=10, maxTransactionSize=4)
#create estDec
dst <- DST_EstDec(datatype = "integer")
#update estDec with new values from data stream
update(dst, dsd, 20)

#get most frequent itemsets from the DST
patterns <- get_patterns(dst)

#show a summary of the patterns
summary(patterns)

#coerce the patterns into a list of vectors
as.vector(patterns)

#coerce into arules itemsets
library(arules)
as.itemsets(patterns)

#get the top ten most frequent patterns
topN(patterns, n=10)
}
\keyword{classes}
