#estDec

#input: datastream D
#output: A complete set of recent freq itemsets Lk
datastream <- DSD_Transactions_Random(c(integer))

estDec<- function(datastream, iterations) {
  #datastream <- #continous data stream
  d <- 0.2 #given decay rate
  rt <- new(RTrie) #monitoring lattice
  minsup <- 0.3 #FIXME change this
  Dk <- 0.0
  TID <-0  # = k
  
  for(i in 1:iterations){
    
    print(paste0("iteration: ", i))
    
    Tk <- get_points.DSD_Transactions_Random(datastream)
    print(Tk)
    TID <- TID + 1
    
    #Ck(e) is the current count of an itemset e which is the number of trans
    #that contain the itemset among the k transactions
    #Sk(e) = current support of itemset e = Ck(e) / |D|k
    #decay-base  b
    #decay-base-life  h
    #decay = b^-(1/h) (b>1, h>=1, b^-1 <= d < 1)
    estDecUpdate(rt, Tk[[1]], d, Dk, minsup, TID)
    
    #parameter updating phase
    Dk <- Dk * d + 1.0  #FIXME
  }
  
  rt$printAll()
}

#function for a single transaction
estDecUpdate<- function(rt, trans, decayRate, Dk, minsup, k) {
  #print(trans)
  #counting update
  rt$updateAllSets(trans, k, decayRate, minsup, Dk)
  
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
  Lk <- NULL

#  for(each itemset e in ML) {
#    cnt <- cnt*d^(k-MRtid); err = err * d^(k-MRtid); MRtid = k;
#    if (cnt/|D|k >= Smin)
#      Lk = Lk U e
#  }
  
}


