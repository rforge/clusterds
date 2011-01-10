# cobweb options:
# -a acuity (default: 1.0)
# Acuity (minimum standard deviation)
# -c cutoff (default: 0.002)
# Cutoff (minimum category utility)
# -r randomSeed (default: 1)
# Seed for random noise.
DSC_CobWeb <- function(acuity=1.0, cutoff=0.002, randomSeed=1) {

  # TODO: need some error checking on params, need to verify
  paramList <- list(a=acuity,
                    c=cutoff,
                    r=randomSeed)

  # converting the param list to a cli string to use in java
  cliParams <- convertParams(paramList)
  
  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/CobWeb")
  .jcall(clusterer, "V", "prepareForUse")

  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)

  # initializing the R object
  l <- list(description = "CobWeb",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- "ClusterDS"
  l
}
