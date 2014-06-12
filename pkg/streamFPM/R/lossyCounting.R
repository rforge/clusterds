#numbers <- sample(1:20, 5000, replace=TRUE, prob=rexp(20))
#lossyCounting(data=numbers)

library(hash)

lossyCounting<- function(data, error=0.1) {
  DH <- hash()
  
  N <- 0;           #number of items processed
  
  w <- 1/error
  
  b_current <- 1
  
  
  for(x in data) {
    
    N <- N+1
  
   ### insert phase ###
   
    #if x is in D
    if (has.key(toString(x), DH)){
      #increase count of item in DH by 1    
      DH[[toString(x)]][1] <- DH[[toString(x)]][1] + 1
      
      
    }
    else {
      #insert(x, 1, b_current-1) into D
      DH[[toString(x)]] <- c(freq = 1, err = b_current - 1)
    }
  
   ### delete phase ###
    
   #if N mod w == 0
   if( N%%w == 0) {
     #bucket boundary reached. delete infreq items
     
     
     #finds all elements in D where the f_i + delta <= b_current
     #removes them
     keys <- keys(DH)
     
     removed <- sapply(1:length(keys), function(x, b_current) {
       if(DH[[keys[x]]][1] + DH[[keys[x]]][2] <= b_current) {
         delete(keys[x], DH)
         return(keys[x])
       }
     }, b_current = b_current)
     
  
     b_current <- b_current + 1     #new bucket
     
   }
  
  }  #end while loop (end of stream)

  
  return(DH)
}


#output
#keys <- keys(DH)
#for (i in 1:length(keys)) {
#  if(DH[[keys[i]]][1] >= (s-e) * N) {
#    print(keys[i])
#    print(DH[[keys[i]]][1])
#  }
#}

