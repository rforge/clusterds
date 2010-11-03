cluster <- function(clusterer=ClusterDS()) {

  panel <- .jnew("moa/gui/clustertab/ClusteringAlgoPanel")
  strm <- .jcall(panel, "Lmoa/streams/clustering/ClusteringStream;", "getStream")
  .jcall(strm, "V", "prepareForUse")
   
  #TODO: be able to expand the number of instances we loop through, and give
  #      the user the ability to pause or resume the datastream

  #looping through the stream, feeding the new datapoints into 
  #the algorithm
  for (i in 1:5000) {
    inst <- .jnew("weka/core/Instance")
    inst <- .jcall(strm, "Lweka/core/Instance;", "nextInstance")
    .jcall(clusterer$javaObj, "V", "trainOnInstanceImpl", inst)
  }
}

convertParams <- function(paramList=list()) {
  length <- length(paramList)
  if (length == 0)
    stop("invalid param list")

  cliParams <- ""

  for (i in 1:length) {
    cliParams <- paste(cliParams, "-", names(paramList[i]), " ", paramList[[i]], " ", sep="")
  }

  # removing the trailing space
  cliParams <- substr(cliParams, 1, nchar(cliParams)-1)
}

# denstream options:
# -e epsilon 	0.01 (defines the epsilon neighborhood, range: 0 to 1)
# -p minPoints 	10 (min. num. points a cluster must have)
# -l lambda	0.006 (range: 0 to 1)
# -b beta		0.001 (range: 0 to 1)
# -m mu		1 (range: 0 to max(double))
# -i initPoints	10000 (number of points to use for initialization)
# -M		false (evaluate micro clustering flag)
DenStream <- function(epsilon=0.1, minPoints=10, lambda=0.006, beta=0.001, mu=1, initPoints=10000) {
  if (epsilon <= 0 || epsilon >= 1)
    stop("invalid epsilon, range: 0 to 1 exclusive")

  if (minPoints < 10)
    stop("invalid minPoints, must be > 0")

  if (lambda <= 0 || lambda >= 1)
    stop("invalid lambda, range: 0 to 1 exclusive")

  if (beta <= 0 || beta >= 1)
    stop("invalid beta, range: 0 to 1 exclusive")

  if (mu <= 0)
    stop("invalid mu, must be > 0")

  if (initPoints < 0)
    stop("invalid initPoints, must be > 0")

  paramList <- list(e = epsilon,
                    p = minPoints,
                    l = lambda,
                    b = beta,
                    m = mu,
                    i = initPoints)

  # converting the param list to a cli string to use in java
  cliParams <- convertParams(paramList)

  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/denstream/DenStream")
  .jcall(clusterer, "V", "prepareForUse")

  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)

  # testing to ensure the options are set correctly
  # setParams <- .jcall(options, "Ljava/lang/String;", "getAsCLIString")
  # print(setParams)

  # initializing the R object
  l <- list(description = "DenStream",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- "ClusterDS"
  l
}

# clustream options:
# -t timeWindow (default: 1000)
# Range of the window.
# -k maxNumKernels (default: 100)
# Maximum number of micro kernels to use.
# -M evaluateMicroClustering
# Evaluate the underlying microclustering instead of the macro clustering
Clustream <- function(timeWindow=1000, maxNumKernels=100) {
  if (timeWindow < 0)
    stop("invalid timeWindow, must be > 0")

  if (maxNumKernels < 0)
    stop("invalid maxNumKernels, must be > 0")

  paramList <- list(t = timeWindow,
                    k = maxNumKernels)

  # converting the param list to a cli string to use in java
  cliParams <- convertParams(paramList)

  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/clustream/Clustream")
  .jcall(clusterer, "V", "prepareForUse")

  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)

  # initializing the R object
  l <- list(description = "Clustream",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- "ClusterDS"
  l
}

# cobweb options:
# -a acuity (default: 1.0)
# Acuity (minimum standard deviation)
# -c cutoff (default: 0.002)
# Cutoff (minimum category utility)
# -r randomSeed (default: 1)
# Seed for random noise.
CobWeb <- function(acuity=1.0, cutoff=0.002, randomSeed=1) {

  #need some error checking on params, need to verify
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

print.ClusterDS <- function(x, ...) {
}

#plot.ClusterDS <- function(x, ...) {
testPlot <- function(x, ...) {

  if (.jcall(x$javaObj, "Z", "implementsMicroClusterer")) {

    # Clustering of microclusters
    mClustering <- .jcall(x$javaObj, "Lmoa/cluster/Clustering;", "getMicroClusteringResult")

    # array of microclusters
    mClusters <- .jcall(mClustering, "Lmoa/core/AutoExpandVector;", "getClustering")

    # length of array
    length <- .jcall(mClusters, "I", "size")
    .jmethods(mClusters)

    # iterating over the array, extracting data to be plotted
    #for (i in 1:length) {
    #  mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", i-1)
    #  print("obtained micro cluster")
    #}

    mClusters
  }
}
