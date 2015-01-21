#estDec

#input: datastream D
#datastream <- DSD_Transactions_Random(c(integer))
#output: A complete set of recent freq itemsets Lk

DST_EstDec <- function(decayRate = 0.2, Dk = 0.0 , minsup = 0.3, datatype="integer") {
  
  decayRate <- decayRate #given decay rate
  #rt <- new(RTrie) #monitoring lattice
  minsup <- minsup #FIXME change this
  Dk <- 0.0 #total number of transactions in the lattice. (computed using decay rate)
  TID <-0L  # = k
  desc <- "estDec"
  dataType <- datatype
  hash <- hash()
  
  estDec<- estDec$new(decayRate, minsup, Dk, TID, dataType)
  structure(list(description = desc,
       RObj = estDec, wordHash = hash),
    class = c("DST_EstDec", "DST"))
}


estDec <- setRefClass("estDec",
  fields = list(
   ### parameters
    rt = "ANY",
    decayRate  		= "numeric",
    minsup			= "numeric",
    Dk = "numeric",
    TID = "integer",
    dataType = "character",
    wordCount = "integer"
   
  ),
  
  methods = list(
   initialize = function(decayRate, minsup, Dk, TID, dataType) {
     rt 		<<-  new(RTrie)
     decayRate	<<-  decayRate
     minsup	<<- minsup
     Dk     <<- Dk
     TID    <<- TID
     dataType <<- dataType
     wordCount <<- 1L
     
     .self
   }
   
  ),
)

update.DST_EstDec <- function(dst, dsd, iterations=1) {

    for(i in 1:iterations){
      
      #print(paste0("iteration: ", i))
      
      #gets the next transaction
      Tk <- get_points(dsd)
      
      if (dst$RObj$dataType == "character") {
        if (class(Tk) == "character") {
          if(has.key(Tk, dst$wordHash)) {
            Tk <- dst$wordHash[[Tk]]
          }
          else {
            dst$wordHash[[Tk]] <- dst$RObj$wordCount
            
            Tk <- dst$RObj$wordCount
            
            dst$RObj$wordCount = dst$RObj$wordCount + 1L
            
          }
        }
        else {
          print("DSD generated point datatype does not match datatype set for estDec")
        }
      }
      
      #updates the TransactionID, which is also the total # of transactions ever seen
      dst$RObj$TID <- dst$RObj$TID + 1L;
      
      #Ck(e) is the current count of an itemset e which is the number of trans
      #that contain the itemset among the k transactions
      #Sk(e) = current support of itemset e = count(e) / Dk
      #decay-base  b
      #decay-base-life  h
      #decay = b^-(1/h) (b>1, h>=1, b^-1 <= d < 1)
      
      #parameter updating phase
      dst$RObj$Dk <- (dst$RObj$Dk * dst$RObj$decayRate) + 1.0 #FIXME
      
      #updates all sets
      dst$RObj$rt$updateAllSets(Tk[[1]], dst$RObj$TID, dst$RObj$decayRate, dst$RObj$minsup, dst$RObj$Dk)
      


    }
    
    #Frequent itemset selection phase
    #Lk <- NULL
    
    #  for(each itemset e in ML) {
    #    cnt <- cnt*d^(k-MRtid); err = err * d^(k-MRtid); MRtid = k;
    #    if (cnt/|D|k >= Smin)
    #      Lk = Lk U e
    #  }
    
}


#returns frequent sets
get_patterns.DST_EstDec <- function(dst, ...) {
  
    #FIXME: implement stuff for "character" datatype
  
  if (dst$RObj$dataType == "character") {
    
    wordValues <- sort(values(dst$RObj$wordCount))
    patterns <- dst$RObj$rt$getFrequentItemsets()
    counts <- patterns[[length(patterns)]]
    patterns <- patterns[-length(patterns)]
    
    for (i in 1:length(patterns)) {
      indices <- findInterval(patterns[[i]], wordValues)
      patterns[[i]] <- attr(wordValues[indices], "names")
    }
    
    names(counts) <- lapply(patterns, paste, collapse= ",")
    attr(patterns, 'counts') <- counts
    
    class(patterns) <- "DST_Patterns"
    patterns
  }
  
  patterns <- dst$RObj$rt$getFrequentItemsets()
  counts <- patterns[[length(patterns)]]
  patterns <- patterns[-length(patterns)]
  names(counts) <- lapply(patterns, paste, collapse= ",")
  
  attr(patterns, 'counts') <- counts
  
  class(patterns) <- "DST_Patterns"
  patterns
}


print.DST_EstDec <- function(x, ...) {
  cat(paste(x$description), "\n")
  cat("Class:", paste(class(x), collapse=", "), "\n") 
  
}



#  if(!is(nc <- try(nclusters(x, type="micro"), silent=TRUE), "try-error")) 
#    cat(paste('Number of micro-clusters:', nc, '\n'))
#  if(!is(nc <- try(nclusters(x, type="macro"), silent=TRUE), "try-error")) 
#    cat(paste('Number of macro-clusters:', nc, '\n'))

