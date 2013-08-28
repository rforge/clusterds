# cobweb options:
# -a acuity (default: 1.0)
# Acuity (minimum standard deviation)
# -c cutoff (default: 0.002)
# Cutoff (minimum category utility)
# -r randomSeed (default: 1)
# Seed for random noise.
DSC_CobWeb <- function(acuity=1.0, cutoff=0.002, randomSeed=NULL) {

    if (acuity < 0)
	stop("invalid acuity")
    if (cutoff < 0)
	stop("invalid cutoff")

    if(is.null(randomSeed)) randomSeed <- as.integer(
	    runif(1L, 0, .Machine$integer.max))

    paramList <- list(a=acuity,
	    c=cutoff,
	    r=randomSeed)

    # converting the param list to a cli string to use in java
    cliParams <- convert_params(paramList)

    # initializing the clusterer
    clusterer <- .jnew("moa/clusterers/CobWeb")
    options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
    .jcall(options, "V", "setViaCLIString", cliParams)
    .jcall(clusterer, "V", "prepareForUse")

    # initializing the R object
    l <- list(description = "CobWeb",
	    options = cliParams,
	    javaObj = clusterer)

    class(l) <- c("DSC_CobWeb","DSC_MOA","DSC")
    l
}

### NOTE: CobWeb does not implement implementsMicroClusterer!
###	    Otherwise we could live without the following code.
get_centers.DSC_CobWeb <- function(x, ...) {

    mClustering <- .jcall(x$javaObj, 
	    "Lmoa/cluster/Clustering;", "getClusteringResult")

    # array of microclusters
    mClusters <- .jcall(mClustering, 
	    "Lmoa/core/AutoExpandVector;", "getClustering")

    # length of array
    length <- .jcall(mClusters, "I", "size")

    # prepping before the loop
    mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", 0L)
    mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")
    center <- .jcall(mCluster, "[D", "getCenter")

    m <- matrix(ncol=length(center), nrow=length)
    m[1,] <- center

    # iterating over the array, extracting data to be plotted
    # the first point has already been used, so start from 2
    if(length>1) for (i in 2:length) {

	# will have to cast mCluster as moa/cluster/Cluster
	mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", i-1L)
	mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")
	center <- .jcall(mCluster, "[D", "getCenter") 
	m[i,] <- center
    }

    # returning the matrix 
    m
}

