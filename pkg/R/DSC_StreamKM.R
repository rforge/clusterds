# StreamKM options:
# -s sizeCoreset, size of the coreset
# -k number of clusters
# -w widthOption
# -r randomSeedOption
DSC_StreamKM <- function(sizeCoreset=100, k=5, width=1000, randomSeed=1) {

  if (sizeCoreset < 0)
    stop("invalid sizeCoreset")
  if (k < 0)
    stop("invalid k")
  if (width < 0)
    stop("invalid width")

  paramList <- list(s=sizeCoreset,
                    k=k,
                    w=width,
                    r=randomSeed)

  # converting the param list to a cli string to use in java
  cliParams <- convert_params(paramList)
  
  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/streamkm/StreamKM")
  .jcall(clusterer, "V", "prepareForUse")

  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)

  # initializing the R object
  l <- list(description = "StreamKM",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- c("DSC_StreamKM","DSC_MOA","DSC")
  l  
}
