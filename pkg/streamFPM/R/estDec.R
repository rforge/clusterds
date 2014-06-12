#estDec

#input: datastream D
#output: A complete set of recent freq itemsets Lk

#datastream <- #continous data stream
d <- 0.2 #given decay rate
#rt <- new(RTrie) #monitoring lattice

#Tk = new transaction (generated on the kth turn)
#TID = transaction ID number
#|D|k total transactions to date - decay
#Ck(e) is the current count of an itemset e which is the number of trans
#that contain the itemset among the k transactions
#Sk(e) = current support of itemset e = Ck(e) / |D|k
#k = kth transaction

#decay-base  b
#decay-base-life  h
#decay = b^-(1/h) (b>1, h>=1, b^-1 <= d < 1)

#       previous |D|k
#|D|k = |D|k-1 * d + 1

#function for a single transaction
estDec<- function(trie, trans, decayRate) {
  
  #parameter updating phase
  Dk <- Dk * decayRate + 1
  
  #counting update
#  rt$updateAllSets(trans, k, d, minsup, Dk)

  #Delayed-insertion phase
#TKf <- ItemFiltering(Tk);

#for (each itemset e in  Tkf and not in ML) {
#    if(length(e) == 1){
#      rt$addSet(e, err = 0, tid = k)
}
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
  
#}


