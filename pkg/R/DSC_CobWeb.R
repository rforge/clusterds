# cobweb options:
# -a acuity (default: 1.0)
# Acuity (minimum standard deviation)
# -c cutoff (default: 0.002)
# Cutoff (minimum category utility)
# -r randomSeed (default: 1)
# Seed for random noise.
DSC_CobWeb <- function(acuity=1.0, cutoff=0.002, randomSeed=1) {

  if (acuity < 0)
    stop("invalid acuity")
  if (cutoff < 0)
    stop("invalid cutoff")

  paramList <- list(a=acuity,
                    c=cutoff,
                    r=randomSeed)

  # converting the param list to a cli string to use in java
  cliParams <- convertParams(paramList)
  
  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/CobWeb")
  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(clusterer, "V", "prepareForUse")

  # initializing the R object
  l <- list(description = "CobWeb",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- c("DSC", "DSC_MOA", "DSC_CobWeb")
  l
}
