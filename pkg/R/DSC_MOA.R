## MOA specific stuff
convert_params <- function(paramList=list()) {
    length <- length(paramList)
    if (length == 0)
	stop("invalid param list")

    cliParams <- ""

    for (i in 1:length) {
	cliParams <- paste(cliParams, "-", names(paramList[i]), 
		" ", paramList[[i]], " ", sep="")
    }

    # removing the trailing space
    cliParams <- substr(cliParams, 1, nchar(cliParams)-1)
}

get_centers.DSC_MOA <- function(x, ...) {

    if (.jcall(x$javaObj, "Z", "implementsMicroClusterer")) {

	# Clustering of microclusters
	mClustering <- .jcall(x$javaObj, "Lmoa/cluster/Clustering;", "getMicroClusteringResult")

	# array of microclusters
	mClusters <- .jcall(mClustering, "Lmoa/core/AutoExpandVector;", "getClustering")

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
    }else{
	warning("Clusterer ",x$description, 
		" does not implement microclusters!")
	invisible(NULL)
    }
}

