#estDec

#input: datastream D
#datastream <- DSD_Transactions_Random(c(integer))
#output: A complete set of recent freq itemsets Lk

DST_EstDec <- function(decayRate = 0.99, minsup = 0.1, insertSupport = NULL, 
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
    insertSupport <- minsup*0.7
  }
  if (is.null(pruningSupport)) {
    pruningSupport <- minsup*0.6
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

update.DST_EstDec <- function(object, dsd, n=1, ...) {

    for(i in 1:n){
      #if(i %% 50 == 0)
      #print(paste0("iteration: ", i))
      
      #gets the next transaction
      Tk <- get_points(dsd)[[1]]
      
      
      #print(Tk)
      
      
      if (object$RObj$dataType == "character") {
        
          #make sure empty strings are removed from list
          Tk <- Tk[Tk != ""]
          Tk_ints <- rep(0L, length(Tk))
          #print(Tk)
          if(length(Tk) > 0) {
            for(i in 1:length(Tk)) {
                #print(Tk[i])
                if(!is.na(Tk[i])){ 
                  if( has.key(key = Tk[i], hash = object$wordHash) ) {
                    Tk_ints[i] <- object$wordHash[[ Tk[i] ]]
                  }
                  else {
                    object$wordHash[[ Tk[i] ]] <- object$RObj$wordCount
                    
                    Tk_ints[i] <- object$RObj$wordCount
                    
                    object$RObj$wordCount <- object$RObj$wordCount + 1L
                    
                  }
                }
            } #end forloop
          }
          Tk_ints <- Tk_ints[Tk_ints != 0L]
          Tk <- Tk_ints
      }
      
      #updates the TransactionID, which is also the total # of transactions ever seen
      object$RObj$TID <- object$RObj$TID + 1L;
      
      #Ck(e) is the current count of an itemset e which is the number of trans
      #that contain the itemset among the k transactions
      #Sk(e) = current support of itemset e = count(e) / Dk
      #decay-base  b
      #decay-base-life  h
      #decay = b^-(1/h) (b>1, h>=1, b^-1 <= d < 1)
      
      #parameter updating phase
      object$RObj$Dk <- (object$RObj$Dk * object$RObj$decayRate) + 1.0 #FIXME
      
      #updates all sets
      object$RObj$rt$updateAllSets(Tk, object$RObj$TID, object$RObj$decayRate, object$RObj$minsup, object$RObj$Dk)
      
    }
    
}


#returns frequent sets
get_patterns.DST_EstDec <- function(dst, decode=FALSE, ...) {
  
  if (dst$RObj$dataType == "character") {
    
    #gets words and their values from the hash table
    wordValues <- sort(values(dst$wordHash))
    
    #gets frequent patterns, with counts as last row
    patterns <- dst$RObj$rt$getFrequentItemsets(dst$RObj$Dk)
    
    #separates counts into dif variable and removes last row
    error <- patterns[[length(patterns)]]
    patterns <- patterns[-length(patterns)]
    
    counts <- patterns[[length(patterns)]]
    #counts <- counts/dst$RObj$Dk
    patterns <- patterns[-length(patterns)]
    patternsNumbers <- patterns
    
    #replaces the numbers in patterns with their names
    for (i in 1L:length(patterns)) {
      indices <- findInterval(patterns[[i]], wordValues)
      patterns[[i]] <- attr(wordValues[indices], "names")
    }

    names(counts) <- lapply(patterns, function(x) { paste(c("{", paste(x, collapse=", "), "}"), collapse = "") })
    
    if(decode == FALSE) {
      attr(counts, "decode_table") <- as.list(dst$wordHash)
      attr(counts, "total_transactions") <- dst$RObj$Dk
      attr(counts, "sets") <- patternsNumbers
      attr(counts, "error") <- error
      class(counts) <- "Patterns"
      return(counts)
    }

    #attr(patterns, 'counts') <- counts
    #class(patterns) <- "DST_Patterns"
    #return(patterns)
    attr(counts, "total_transactions") <- dst$RObj$Dk
    attr(counts, "sets") <- patterns
    attr(counts, "error") <- error
    class(counts) <- "Patterns"
    return(counts)
  }
  
  #if not character
  
  patterns <- dst$RObj$rt$getFrequentItemsets(dst$RObj$Dk)
  
  error <- patterns[[length(patterns)]]
  patterns <- patterns[-length(patterns)]
  
  counts <- patterns[[length(patterns)]]
  #counts <- counts/dst$RObj$Dk
  patterns <- patterns[-length(patterns)]
  
  names(counts) <- lapply(patterns, function(x) { paste(c("{", paste(x, collapse=", "), "}"), collapse = "") })
  
  #attr(patterns, 'counts') <- counts
  #class(patterns) <- "DST_Patterns"
  #return(patterns)
  
  attr(counts, "sets") <- patterns
  attr(counts, "total_transactions") <- dst$RObj$Dk
  attr(counts, "error") <- error
  
  class(counts) <- "Patterns"
  return(counts)
  
}


print.DST_EstDec <- function(x, ...) {
  cat(paste(x$description), "\n")
  cat("Class:", paste(class(x), collapse=", "), "\n")
  #FIXME add how many patterns are there
  
}


