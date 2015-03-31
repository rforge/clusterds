#estDec

#input: datastream D
#datastream <- DSD_Transactions_Random(c(integer))
#output: A complete set of recent freq itemsets Lk

DST_EstDec <- function(decayRate = 0.2, minsup = 0.8, insertSupport = NULL, 
                       pruningSupport = NULL, datatype="character") {
  
  decayRate <- decayRate #given decay rate
  #rt <- new(RTrie) #monitoring lattice
  minsup <- minsup #FIXME change this
  Dk <- 0.0 #total number of transactions in the lattice. (computed using decay rate)
  TID <-0L  # = k
  desc <- "estDec"
  dataType <- datatype
  hash <- hash()
  insertSupport <- insertSupport
  pruningSupport <- pruningSupport
  
  if (is.null(insertSupport)) {
    insertSupport <- minsup*0.8
  }
  if (is.null(pruningSupport)) {
    pruningSupport <- minsup*0.7
  }
  
  estDec<- estDec$new(decayRate, minsup, Dk, TID, insertSupport, pruningSupport, dataType)
  
  e <- structure(list(description = desc,
       RObj = estDec, wordHash = hash),
    class = c("DST_EstDec", "DST"))
  
  #sets the support and decay rate for the data structure
  e$RObj$rt$updateParameters(decayRate, minsup, insertSupport, pruningSupport)
  
  e
}


estDec <- setRefClass("estDec",
  fields = list(
   ### parameters
    rt = "ANY",
    decayRate  		= "numeric",
    minsup			= "numeric",
    Dk = "numeric",
    TID = "integer",
    insertSupport = "numeric",
    pruningSupport = "numeric",
    dataType = "character",
    wordCount = "integer"
   
  ),
  
  methods = list(
   initialize = function(decayRate, minsup, Dk, TID, insertSupport, pruningSupport, dataType) {
     rt 		<<-  new(RTrie)
     decayRate	<<-  decayRate
     minsup	<<- minsup
     Dk     <<- Dk
     TID    <<- TID
     insertSupport <<- insertSupport
     pruningSupport <<- pruningSupport
     dataType <<- dataType
     wordCount <<- 1L
     
     .self
   }
   
  ),
)

update.DST_EstDec <- function(dst, dsd, n=1) {

    for(i in 1:n){
      if(n%%50 == 0){
        print(i)
      }
      #print(paste0("iteration: ", i))
      
      #gets the next transaction
      Tk <- get_points(dsd)[[1]]
      
      
        #print(Tk)
      
      
      if (dst$RObj$dataType == "character") {
        
          #make sure empty strings are removed from list
          Tk <- Tk[Tk != ""]
          Tk_ints <- rep(0L, length(Tk))
          
          for(i in 1:length(Tk)) {
              #print(Tk[i])
              if( has.key(key = Tk[i], hash = dst$wordHash) ) {
                Tk_ints[i] <- dst$wordHash[[ Tk[i] ]]
              }
              else {
                dst$wordHash[[ Tk[i] ]] <- dst$RObj$wordCount
                
                Tk_ints[i] <- dst$RObj$wordCount
                
                dst$RObj$wordCount <- dst$RObj$wordCount + 1L
                
              }
          } #end forloop
         
          Tk <- Tk_ints
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
      dst$RObj$rt$updateAllSets(Tk, dst$RObj$TID, dst$RObj$decayRate, dst$RObj$minsup, dst$RObj$Dk)
      
    }
    
}


#returns frequent sets
get_patterns.DST_EstDec <- function(dst, decode=FALSE) {
  
  if (dst$RObj$dataType == "character") {
    
    #gets words and their values from the hash table
    wordValues <- sort(values(dst$wordHash))
    
    #gets frequent patterns, with counts as last row
    patterns <- dst$RObj$rt$getFrequentItemsets()
    
    #separates counts into dif variable and removes last row
    counts <- patterns[[length(patterns)]]
    patterns <- patterns[-length(patterns)]
    patternsNumbers <- patterns
    
    #replaces the numbers in patterns with their names
    for (i in 1L:length(patterns)) {
      indices <- findInterval(patterns[[i]], wordValues)
      patterns[[i]] <- attr(wordValues[indices], "names")
    }
    
    names(counts) <- lapply(patterns, paste, collapse= ",")
    
    if(decode == FALSE) {
      attr(counts, "decodeTable") <- as.list(dst$wordHash)
      attr(counts, "sets") <- patternsNumbers
      class(counts) <- "DST_Patterns"
      return(counts)
    }

    #attr(patterns, 'counts') <- counts
    #class(patterns) <- "DST_Patterns"
    #return(patterns)
    
    attr(counts, "sets") <- patterns
    class(counts) <- "DST_Patterns"
    return(counts)
  }
  
  patterns <- dst$RObj$rt$getFrequentItemsets()
  counts <- patterns[[length(patterns)]]
  patterns <- patterns[-length(patterns)]
  names(counts) <- lapply(patterns, paste, collapse= ",")
  
  #attr(patterns, 'counts') <- counts
  #class(patterns) <- "DST_Patterns"
  #return(patterns)
  
  attr(counts, "sets") <- patterns
  
  class(counts) <- "DST_Patterns"
  return(counts)
  
}


print.DST_EstDec <- function(x, ...) {
  cat(paste(x$description), "\n")
  cat("Class:", paste(class(x), collapse=", "), "\n")
  #FIXME add how many patterns are there
  
}


