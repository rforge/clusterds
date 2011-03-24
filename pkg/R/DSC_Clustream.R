# clustream options:
# -t timeWindow (default: 1000)
# Range of the window.
# -k maxNumKernels (default: 100)
# Maximum number of micro kernels to use.
# -M evaluateMicroClustering
# Evaluate the underlying microclustering instead of the macro clustering
DSC_Clustream <- function(timeWindow=1000, maxNumKernels=100) {
  if (timeWindow < 0)
    stop("invalid timeWindow, must be > 0")

  if (maxNumKernels < 0)
    stop("invalid maxNumKernels, must be > 0")

  paramList <- list(t = timeWindow,
                    k = maxNumKernels)

  # converting the param list to a cli string to use in java
  cliParams <- convert_params(paramList)

  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/clustream/Clustream")
  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(clusterer, "V", "prepareForUse")

  # initializing the R object
  l <- list(description = "Clustream",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- c("DSC", "DSC_MOA", "DSC_Clustream")
  l
}
