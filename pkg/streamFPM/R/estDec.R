#estDec

#input: datastream D
#datastream <- DSD_Transactions_Random(c(integer))
#output: A complete set of recent freq itemsets Lk

DST_EstDec <- function(decayRate = 0.2, Dk = 0.0 , minsup = 0.3) {
  
  decayRate <- decayRate #given decay rate
  #rt <- new(RTrie) #monitoring lattice
  minsup <- minsup #FIXME change this
  Dk <- 0.0 #something
  TID <-0L  # = k
  desc <- "estDec"
  
  estDec<- estDec$new(decayRate, minsup, Dk, TID)
  structure(list(description = desc,
       RObj = estDec),
    class = c("DST_EstDec", "DST"))
}


estDec <- setRefClass("estDec",
  fields = list(
   ### parameters
    rt = "ANY",
    decayRate  		= "numeric",
    minsup			= "numeric",
    Dk = "numeric",
    TID = "integer"
   
  ),
  
  methods = list(
   initialize = function(decayRate, minsup, Dk, TID) {
     rt 		<<-  new(RTrie)
     decayRate			<<-  decayRate
     minsup	<<- minsup
     Dk     <<- Dk
     TID    <<- TID

     .self
   }
   
  ),
)

update.DST_EstDec = function(dst, dsd, iterations=1) {

    for(i in 1:iterations){
      
      #print(paste0("iteration: ", i))
      
      Tk <- get_points(dsd)
      #print(Tk)
      dst$RObj$TID <- dst$RObj$TID + 1L;
      
      #Ck(e) is the current count of an itemset e which is the number of trans
      #that contain the itemset among the k transactions
      #Sk(e) = current support of itemset e = Ck(e) / |D|k
      #decay-base  b
      #decay-base-life  h
      #decay = b^-(1/h) (b>1, h>=1, b^-1 <= d < 1)
      
      #updates all sets
      dst$RObj$rt$updateAllSets(Tk[[1]], dst$RObj$TID, dst$RObj$decayRate, dst$RObj$minsup, dst$RObj$Dk)
      
      #parameter updating phase
      dst$RObj$Dk <- (dst$RObj$Dk * dst$RObj$decayRate) + 1.0 #FIXME

    }
    
    #print(trans)
    #counting update
    
    
    #Delayed-insertion phase
    #TKf <- ItemFiltering(Tk);
    
    #for (each itemset e in  Tkf and not in ML) {
    #    if(length(e) == 1){
    #      rt$addSet(e, err = 0, tid = k)
    #}
    #    else {
    #Estimate Cmax(e) and Cmin(e)
    #      if(Cmax(e) > Cupper(e))
    #        Cmax(e) <- Cupper(e)
    #      if(Cmax(e)/dk  >= Sins)
    #        rt$addSet(e, err=Cmax(e)-Cmin(e), tid=k)
    #    }
    #}
    
    #Frequent itemset selection phase
    #Lk <- NULL
    
    #  for(each itemset e in ML) {
    #    cnt <- cnt*d^(k-MRtid); err = err * d^(k-MRtid); MRtid = k;
    #    if (cnt/|D|k >= Smin)
    #      Lk = Lk U e
    #  }
    
}


#returns frequent sets
get_patterns.DST_EstDec = function(dst, ...) {
    patterns <- dst$RObj$rt$getFrequentItemsets()
    counts <- patterns[[length(patterns)]]
    patterns <- patterns[-length(patterns)]
    
    for (i in 1:length(patterns)) {
      attr(patterns[[i]],'count') <- counts[i]
    }
    
    patterns
}


