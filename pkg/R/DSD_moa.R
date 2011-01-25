# RandomRBFGeneratorEvents options:
# -m modelRandomSeed
# -i instanceRandomSeed
# -K numCluster
# -k numClusterRange
# -R kernelRadius
# -r kernelRadiusRange
# -d densityRange
# -V speed
# -v speedRange
# -N noiseLevel
# -E eventFrequency
# -M eventMergeWeight
# -P eventSplitWeight
# because there are so many parameters, let's only use a few key ones...
DSD_moa <- function(modelSeed=1, instanceSeed=1, numCluster=4L, avgRadius=0, density=0) {
  #TODO: need error checking on the params

  # we leave the other parameters as defaults
  paramList <- list(m=modelSeed,
                    i=instanceSeed,
                    K=numCluster,
                    k=3L,
                    R=avgRadius,
                    r=0.0,
                    d=density,
                    V=100L,
                    v=0L,
                    N=0.1,
                    E=15000L,
                    M=0.5,
                    P=0.5)

  # converting the param list to a cli string to use in java
  cliParams <- convertParams(paramList)

  # initializing the clusterer
  strm <- .jnew("moa/streams/clustering/RandomRBFGeneratorEvents")
  .jcall(strm, "V", "prepareForUse") #TODO: does this need to be after the options have been set??
  options <- .jcall(strm, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)

  l <- list(Description = "RandomRBFGeneratorEvents",
            cliParams = cliParams,
            javaObj = strm)

  class(l) <- c("DSD","DSD_moa")
  l
}

# we only create 1 data point at a time for Java instances
getPoints.DSD_moa <- function(x, n=1, ...) {
  if (n == 1) {
    inst <- .jcall(x$javaObj, "Lweka/core/Instance;", "nextInstance")
  } else if (n > 1) {    
    stop("getPoints.DSD_moa must have n = 1")
  } else {
    stop("invalid n")
  }

}
