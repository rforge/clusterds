#numbers <- sample(1:20, 5000, replace=TRUE, prob=rexp(20))
#DH <- nonLossyCounting(data=numbers)

library(hash)

nonLossyCounting<- function(data) {
  DH <- hash()
  
  N <- 0;           #number of items processed
  
  
  for(x in data) {
    
    N <- N+1
    
    ### insert phase ###
    
    #if x is in D
    if (has.key(toString(x), DH)){
      #increase count of item in DH by 1    
      DH[[toString(x)]][1] <- DH[[toString(x)]][1] + 1
      
      
    }
    else {
      #insert(x, 1) into D
      DH[[toString(x)]] <- c(freq = 1)
    }
  }
  return(DH)
}
