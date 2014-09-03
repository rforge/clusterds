#data stream generator

#want stream data
#each point will be integers or strings
#start with integers

# A new DSD class (e.g., myDSD) needs the following:
## 1. a constructor function. myDSD <- function(PARAMETERS) which
## returns an object with the class  c("DSD_myDSD","DSD_R","DSD")
## 2. get_points.myDSD <- function(x, n=1, ...)



DSD_Transactions <- function(type=c("integer"), setSize=50, maxTransactionSize=10, distribution="") {
  
  if(setSize < maxTransactionSize){ stop("maxTransactionSize cannot be larger than setSize")}
  
  # creating the DSD object
  l <- list(description = "Random Transaction Data Stream",
            type=type,
            setSize=setSize,
            maxTransactionSize=maxTransactionSize,
            distribution=distribution)
  class(l) <- c("DSD_Transactions","DSD_R","DSD")
  l
}


#n = number of transactions
#x = DSD object
get_points.DSD_Transactions <- function(x, n=1, assignment = FALSE,...) {
  ### gaussians at (3,2.5) and (3,-2.5)
  ### bars at (-3,2.8) and (-3,-2.8)
  
  a <- vector("list", n)
  for (i in 1:n) {
    length <- sample(1:x$maxTransactionSize)
    a[[i]] <- sample(1:x$setSize, length, replace=FALSE, prob=rexp(x$setSize))
  }
  
  return(a)
  
}
