# StreamKM options:
# IntOption("sizeCoreset", 's', "Size of the coreset.", 10000);
# IntOption("numClusters", 'k', "Number of clusters to compute.", 5);
# IntOption("width", 'w', "Size of Window for training learner.", 100000, 0, Integer.MAX_VALUE);
# IntOption("randomSeed", 'r', "Seed for random behaviour of the classifier.", 1);

DSC_StreamKM <- function(sizeCoreset=100, k=5, 
  width=1000, randomSeed=NULL) {
  
    if(is.null(randomSeed)) randomSeed <- as.integer(
      runif(1L, 0, .Machine$integer.max))
    else randomSeed <- as.integer(randomSeed)
  
  paramList <- list(s=as.integer(sizeCoreset),
    k=as.integer(k),
    w=as.integer(width),
    r=randomSeed)
  
  # converting the param list to a cli string to use in java
  cliParams <- convert_params(paramList)
  
  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/streamkm/StreamKM")
  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(clusterer, "V", "prepareForUse")
  
  # initializing the R object
  structure(
    list(
      description = "StreamKM",
      options = cliParams,
      javaObj = clusterer
    ), 
    class = c("DSC_StreamKM","DSC_Micro","DSC_MOA","DSC")
  )
}
