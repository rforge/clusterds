# ClusTree options:
# -t timeWindow (default: 1000)
# -h the maximal height of the tree (default: 8)
DSC_ClusTree <- function(timeWindow=1000, maxHeight=8) {

  # error checking
  if (maxHeight < 0) {
    stop("invalid maxHeight")
  }

  if (timeWindow < 0) {
    stop("invalid timeWindow")
  }

  paramList <- list(t=timeWindow,
                    h=maxHeight)

  # converting the param list to a cli string to use in java
  cliParams <- convertParams(paramList)
  
  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/clustree/ClusTree")
  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(clusterer, "V", "prepareForUse")

  # initializing the R object
  l <- list(description = "ClusTree",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- c("DSC", "DSC_MOA", "DSC_ClusTree")
  l  
}
