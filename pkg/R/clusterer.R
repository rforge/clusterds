create_stream <- function() {
  panel <- .jnew("moa/gui/clustertab/ClusteringAlgoPanel")
  strm <- .jcall(panel, "Lmoa/streams/clustering/ClusteringStream;", "getStream")
  .jcall(strm, "V", "prepareForUse")

  l <- list(Description = "RandomRBFGeneratorEvents",
            cliParams = "",
            javaObj = strm)

  class(l) <- "DS"
  l
}

# clusterProb is a vector of probabilities for the clusters created
create_rStream <- function(mu=0.5, sd=0.2, numAttr=2, numClusters=1L, clusterProb=1) {
  if (length(mu) != numClusters) {
    stop("mu vector is a different length than the number of clusters")
  }

  if (length(sd) != numClusters) {
    stop("sd vector is a different length than the number of clusters")
  }

  if (length(clusterProb) != numClusters) {
    print("probability vector is a different length than the number of clusters, defaulting to an even split")
    prob <- 1/numClusters
    clusterProb <- as.vector(array(prob, numClusters))
  }

  l <- list(Description = "rStream",
            mu = mu,
            sd = sd,
            numAttr = numAttr,
            numClusters = numClusters,
            clusterProb = clusterProb,
            centroids = as.vector(array(0, numClusters)),
            numPoints = as.vector(array(0, numClusters)))
  class(l) <- "rDS"
  l
}

get_instance <- function(x, ...) UseMethod("get_instance")

get_instance.default <- function(x, ...) {
   stop(gettextf("get_instance not implemented for class '%s'.", class(x)))
}

get_instance.DS <- function(x, ...) {
  inst <- .jcall(x$javaObj, "Lweka/core/Instance;", "nextInstance")
}

# TODO: how do we re-assign the variable after calculating the values??
get_instance.rDS <- function(x, ...) {

  # probabilistic selection of cluster
  if (x$numClusters == 1) {
    numCluster <- 1
  } else {
    roll <- runif(max=1, min=0, n=1)
    val <- x$clusterProb[1]
    for (i in 1:length(x$clusterProb)) {
      if (roll <= val) {
        numCluster <- i
        break;
      }  else {
        val <- val + i
      }
    }
  }

  # generating an instance
  inst <- rnorm(n=x$numAttr, mean=x$mu[numCluster], x$sd[numCluster])
  print(inst)

  # recalculating number of points & centroids
  # TODO: is this the right way to calculate the centroid??
  if (x$numPoints[numCluster] == 0) {
    x$centroids[numCluster] = mean(inst)
  } else {
    x$centroids[numCluster] <- (x$centroids[numCluster] * x$numPoints[numCluster] + mean(inst)) / (x$numPoints[numCluster]+1)
  }
  x$numPoints[numCluster] <- x$numPoints[numCluster]+1

  # casting the inst to a java object
  inst <- .jnew("weka/core/Instance", 1, inst)
}

cluster <- function(clusterer=ClusterDS(), x, numPoints=10000) { 
  if (numPoints < 2)
    stop("numPoints must be > 1")

  # looping through the stream, feeding the new datapoints into 
  # the algorithm
  for (i in 1:numPoints) {
    inst <- get_instance(strm)
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

# ClusTree options:
# -t timeWindow (default: 1000)
# -h the maximal height of the tree (default: 8)
ClusTree <- function(timeWindow=1000, maxHeight=8) {

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

# StreamKM options:
# -s sizeCoreset, size of the coreset
# -k numClusters
# -w widthOption
# -r randomSeedOption
StreamKM <- function(sizeCoreset=100, numClusters=5, width=1000, randomSeed=1) {

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

get_centroids <- function(x, ...) {

  if (.jcall(x$javaObj, "Z", "implementsMicroClusterer")) {

    # Clustering of microclusters
    mClustering <- .jcall(x$javaObj, "Lmoa/cluster/Clustering;", "getMicroClusteringResult")

    # array of microclusters
    mClusters <- .jcall(mClustering, "Lmoa/core/AutoExpandVector;", "getClustering")

    # length of array
    length <- .jcall(mClusters, "I", "size")

    if (length < 2)
      stop("not enough microclusters, please cluster more data")

    # this matrix will hold the data to plot
    m <- matrix(ncol=2, nrow=length)
    colnames(m) <- c("x", "y")

    # iterating over the array, extracting data to be plotted
    for (i in 1:length) {
 
      # will have to cast mCluster as moa/cluster/Cluster
      mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", i-1L)
      mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")

      # array of doubles for each dimension
      center <- .jcall(mCluster, "[D", "getCenter") 
      
      # if the data is 2 dimensional, we don't have to project 
      #if (length(center) == 2) {
        m[i, 1] = center[1]
        m[i, 2] = center[2]
      #}
    }
    
    # returning the matrix 
    m
  }
}

print.DS <- function(x, ...) {
}

print.ClusterDS <- function(x, ...) {
}
