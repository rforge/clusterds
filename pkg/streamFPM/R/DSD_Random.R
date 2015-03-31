#data stream generator

#want stream data
#each point will be integers or strings
#start with integers

#class of type DSD_Transactions

# A new DSD class (e.g., myDSD) needs the following:
## 1. a constructor function. myDSD <- function(PARAMETERS) which
## returns an object with the class  c("DSD_myDSD","DSD_R","DSD")
## 2. get_points.myDSD <- function(x, n=1, ...)


#setSize is the number of items to pull from,
#maxTransactionSize is the largest transaction you can have
#prob = probability for each item, other options include rexp(setSize)
#size = size for each individual transaction
DSD_Transactions_Random <- function(type=c("integer"), setSize=50, maxTransactionSize=10, 
                                    prob = function(set) rep(1/set, times=set),
                                    size = function(maxSize) sample(1:maxSize, 1) ) {
  
  if(setSize < maxTransactionSize){ stop("maxTransactionSize cannot be larger than setSize")}
  
  # creating the DSD object
  l <- list(description = "Random Transaction Data Stream",
            type=type,
            setSize=setSize,
            maxTransactionSize=maxTransactionSize,
            prob=prob(setSize),
            size=size)
  
  class(l) <- c("DSD_Transactions_Random", "DSD_Transactions", "DSD_List", "DSD_R","DSD")
  
  l
  
}


#n = number of transactions
#x = DSD object
#returns a, list of transactions
get_points.DSD_Transactions_Random <- function(x, n=1, assignment = FALSE,...) {
  ### gaussians at (3,2.5) and (3,-2.5)
  ### bars at (-3,2.8) and (-3,-2.8)
  
  a <- vector("list", n)
  for (i in 1:n) {
    length <- x$size(x$maxTransactionSize)
    a[[i]] <- sample(1:x$setSize, length, replace=FALSE, prob = x$prob)
  }
  
  return(a)
  
}


#FIXME print function
