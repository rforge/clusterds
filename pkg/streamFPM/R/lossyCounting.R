#LossyCounting

#input: datastream D
#datastream <- DSD_Transactions_Random(c(integer))
#output: A complete set of recent freq itemsets Lk

DST_LossyCounting <- function(error=0.1, datatype = "integer") {
  
  #FIXME add checks. error <1
  
  DH <- hash()
  N <- 0L         #number of items processed
  width <- 1/error    #width of bucket
  error <- error
  b_current <- 1L      #current bucket
  desc <- "Lossy Counting"
  dataType <- datatype
  
  LossyCounting <- LossyCounting$new(error, b_current, width, N, dataType)
  structure(list(description = desc,
                 RObj = LossyCounting, DH = DH),
            class = c("DST_LossyCounting", "DST"))
}


LossyCounting <- setRefClass("LossyCounting",
      fields = list(
        ### parameters
        error = "numeric",
        b_current	= "integer",
        width = "numeric",
        N = "integer",
        dataType = "character"
        
      ),
                      
    methods = list(
      initialize = function(error, b_current, width, N, dataType) {
        error	<<-  error
        b_current	<<- b_current
        width     <<- width
        N    <<- N
        dataType <<- dataType
        
        .self
      }
      
    ),
)

update.DST_LossyCounting <- function(object, dsd, n=1, ...) {
  
  for(i in 1:n){
    
    #print(paste0("iteration: ", i))
    
    #gets the next transaction
    Tk <- get_points(dsd)[[1]]
    if(object$RObj$dataType == "character") {
      Tk <- Tk[Tk != ""]
    }
    
    #print(Tk)
    for(x in Tk) {
      object$RObj$N <- object$RObj$N+1L
      
      ### insert phase ###
      if(object$RObj$dataType == "integer") {
        #if x is in DH
        if (has.key(toString(x), object$DH)){
          #increase count of item in DH by 1    
          object$DH[[toString(x)]][1] <- object$DH[[toString(x)]][1] + 1
          
          
        }
        else {
          #insert(x, 1, b_current-1) into DH
          object$DH[[toString(x)]] <- c(freq = 1, err = object$RObj$b_current - 1)
        }
      }
      else {  #if dataType is character
        #if x is in DH
        if (has.key(x, object$DH)){
          #increase count of item in DH by 1    
          object$DH[[x]][1] <- object$DH[[x]][1] + 1
          
          
        }
        else {
          #insert(x, 1, b_current-1) into DH
          object$DH[[x]] <- c(freq = 1, err = object$RObj$b_current - 1)
        }
        
      }
      
      ### delete phase ###
      
      #if N mod w == 0
      if( object$RObj$N %% object$RObj$width == 0) {
        #bucket boundary reached. delete infreq items
        
        
        #finds all elements in D where the f_i + delta <= b_current
        #removes them
        keys <- keys(object$DH)
        
        removed <- sapply(1:length(keys), 
                    function(x, b_current) {
                      if(object$DH[[keys[x]]][1] + object$DH[[keys[x]]][2] <= b_current) {
                        delete(keys[x], object$DH)
                        return(keys[x])
                      }
                    }, b_current = object$RObj$b_current)
        
        
        object$RObj$b_current <- object$RObj$b_current + 1L     #new bucket
        
      }
    }
  }
}


#returns frequent sets
get_patterns.DST_LossyCounting <- function(dst, minsup = 0.1, decode=FALSE, ...) {
  
  #$c_i >= (s - e)N$.
  
  threshold <- (minsup - dst$RObj$error)*dst$RObj$N
  
  keys <- keys(dst$DH)
  frequent <- sapply(1:length(keys), 
                    function(x, thres) {
                      if(dst$DH[[keys[x]]][1] >= thres) {
                        return(keys[x])
                      }
                    }, thres = threshold)
  frequent <- frequent[!sapply(frequent, is.null)]
  
  patterns <- vector(mode="integer", length=length(frequent))
  error <- vector(mode="integer", length=length(frequent))
  names <- vector(mode="character", length=length(frequent))
  for(i in 1:length(frequent)) {
    patterns[i] <- dst$DH[[ frequent[[i]][1] ]][1]
    error[i] <- dst$DH[[ frequent[[i]][1] ]][2]
    names[i] <- frequent[[i]][1] 
  }
  names(patterns) <- names
  attr(patterns, "sets") <- as.list(names)
  attr(patterns, "error") <- error
  
  class(patterns) <- "Patterns"
  
}

print.DST_LossyCounting <- function(x, ...) {
  cat(paste(x$description), "\n")
  cat("Class:", paste(class(x), collapse=", "), "\n")
  #FIXME add how many patterns are there
  
}



