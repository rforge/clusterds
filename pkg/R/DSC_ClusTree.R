# ClusTree options:
# -t timeWindow (default: 1000)
# -h the maximal height of the tree (default: 8)
DSC_ClusTree <- function(timeWindow=1000, maxHeight=8) {

  # TODO: need some error checking on params, need to verify
  paramList <- list(t=timeWindow,
                    h=maxHeight)

  # converting the param list to a cli string to use in java
  cliParams <- convertParams(paramList)
  
  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/clustree/ClusTree")
  .jcall(clusterer, "V", "prepareForUse")

  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)

  # initializing the R object
  l <- list(description = "ClusTree",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- "ClusterDS"
  l  
}
