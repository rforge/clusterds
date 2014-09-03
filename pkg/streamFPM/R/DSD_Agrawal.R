#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011, 2012 Michael Hahsler, Christian Buchta, 
#			Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.



##*******************************************************
## Function random.transactions
##
## Generate a random transaction data set. 

library(arules)



DSD_Agrawal <- function(type=c("integer"), setSize=50, maxTransactionSize=10, distribution = "",
    ...,
    verbose = FALSE) {
    nItems <- setSize
    
    patterns <- 
      random.patterns(nItems = nItems, 
        method = "agrawal", ..., verbose = verbose) 

   # return(.random.transactions_agrawal(nItems = nItems, 
  #         nTrans = nTrans, ..., verbose = verbose))

  l <- list(description = "Agrawal Transaction Data Stream",
            type=type,
            setSize=setSize,
            maxTransactionSize=maxTransactionSize,
            distribution=distribution,
            patterns=patterns)
  class(l) <- c("DSD_Agrawal","DSD_R","DSD")
  l
}


##******************************************************************
## R Implementation of the IBM Quest transaction data generator
##
## described in R. Agrawal, R. Srikant, "Fast Algorithms for Mining 
##	Association Rules," Procs. 20th int'l Conf. Very Large DB, 1994 
##

##
## nTrans ... number of transactions
## lTrans ... agv. length of transactions
##
## nItems ... number of items
##
## nPats  ... number of patterns (potential maximal frequent itemsets)
## lPats  ... avg. length of patterns
##
## corr   ... correlation between consecutive patterns 
## cmean  ... mean of the corruption level (norm distr.)
## cvar   ... variance of the corruption level
##
##
## the length the transactions and patterns (potential maximal frequ. itemsets,
##	PMFIs) follow a Poisson distribution with mean ltans and lPats
##
## the weights of the patterns are chosen from a exponential distribution with
##	a mean of 1
##
## corr (chance of an item in a pattern to be also in the next pattern) 
##	is set by default to 0.5
##


## create patterns

random.patterns <- function(
    nItems, 
    nPats = 2000, 
    method = NULL, # method is unused for now
    lPats = 4,
    corr = 0.5,
    cmean = 0.5,
    cvar = 0.1, 
    iWeight = NULL,
    verbose = FALSE) {

    ## iWeight are used for item selection to build PMFIs
    ## the original implementation used exponential weights (the default here).
    if(is.null(iWeight)) {
        iWeight <- rexp(nItems, rate = 1)
        iWeight <- iWeight / sum(iWeight)
    }


    ## pattern lengths (we want no empty patterns)
    ## that's how they for it in the code to get no 0 lenght patterns
    pLengths <- rpois(nPats, lPats - 1) + 1

    ## pattern weights (weights need to sum up to 1)
    pWeights <- rexp(nPats, rate = 1)
    pWeights <- pWeights / sum(pWeights)

    ## corruption levels (cannot be neg.)
    pCorrupts <-  rnorm(nPats, mean = cmean, sd = sqrt(cvar)) 
    pCorrupts[pCorrupts < 0] <- 0
    pCorrupts[pCorrupts > 1] <- 1

    ## PMFIs
    patterns <- list()

    for (i in 1:nPats) {
        if(verbose) cat("Creating pattern #", i, "\n")
        pattern <- c()

        if(i > 1) {
            ## correlation: take some items from the previous pattern
            ## in the paper they say the mean of the exp dist. is corr but
            ## in the implementation they used 1 in the following way: 
            nTake <- min(c(trunc(pLengths[i] * corr * rexp(1, rate=1) + 0.5),
                    pLengths[i-1], pLengths[i])) 

            if(nTake > 0) {
                take <- sample(1:pLengths[i-1], nTake)
                pattern <- patterns[[i-1]][take]
            }
        }

        ## fill rest random items using iWeight 
        if(is.null(pattern)) take <- sample(c(1:nItems), 
            pLengths[i], prob = iWeight)
        else take <- sample(c(1:nItems)[-pattern], 
            pLengths[i]-length(pattern), prob = iWeight[-pattern])

        pattern <- sort(c(pattern, take))
        patterns[[i]] <- pattern
    }

    ## create itemMatrix w/o recoding
    new("itemsets", 
        items   = encode(patterns, 
            paste("item",as.character(1:nItems), sep="")), 
        quality = data.frame(pWeights = pWeights, pCorrupts = pCorrupts))
}


## create transactions
get_points.DSD_Agrawal <- function(x, n=1, assignment = FALSE,...)
{
    patterns <- x$patterns
    nItems <- x$setSize
    nTrans <- n
    lTrans <- x$maxTransactionSize/2
    if(!is.null(x$setSize) && x$setSize != nitems(items(x$patterns)))
    stop("nItems in patterns and the given nItems do not match!\n")


    ## get patterns and weights from arg
    pWeights <- quality(x$patterns)$pWeights
    pCorrupts <- quality(x$patterns)$pCorrupts
    nPatterns <- length(x$patterns)
    patterns <- LIST(items(x$patterns), decode = FALSE)

    ## transaction lengths
    ## that's how AS do it to get no transaction of length 0
    tLengths <- rpois(nTrans, lTrans -1) + 1;

    ## transactions
    #transactions <- list(); 

    trans <- c()

    while (length(trans) < tLengths) {

        ## choose pattern with weights
        j <- sample(1:nPatterns, 1, prob = pWeights)
        patternToAdd <- patterns[[j]]

        ## corrupting pattern
        ## corruption level is norm distr. 
        if(pCorrupts[j] == 1) next

        patLen <- length(patternToAdd)
        
        ##while (runif(1) < pCorrupts[j] && patLen > 0) patLen <- patLen -1
        ## do it the fast way -- results in a geometric distribution
        if(pCorrupts[j] >0) {
            patLen <- patLen - rgeom(1, 1 - pCorrupts[j])
            if(patLen < 1) next
        }

        ## get out 50% of the cases if transaction would be overfull
        ## we depart from AS by not allowing to generate empty transactions
        if (length(trans) > 0 
            &&(length(trans) + patLen) > tLengths 
            && runif(1) > 0.5) break 

        ## pick the items and add them to the transactions
        patternToAdd <- patternToAdd[sample(1:length(patternToAdd), patLen)]
        trans <- unique(sort(c(trans, patternToAdd)))  
    }

    return(trans)
}

###
