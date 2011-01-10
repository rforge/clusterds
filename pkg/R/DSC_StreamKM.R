# StreamKM options:
# -s sizeCoreset, size of the coreset
# -k numClusters
# -w widthOption
# -r randomSeedOption
DSC_StreamKM <- function(sizeCoreset=100, numClusters=5, width=1000, randomSeed=1) {

  # TODO: need some error checking on params, need to verify
  paramList <- list(s=sizeCoreset,
                    k=numClusters,
                    w=width,
                    r=randomSeed)

  # converting the param list to a cli string to use in java
  cliParams <- convertParams(paramList)
  
  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/streamkm/StreamKM")
  .jcall(clusterer, "V", "prepareForUse")

  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)

  # initializing the R object
  l <- list(description = "StreamKM",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- "ClusterDS"
  l  
}
