## MOA specific stuff
convert_params <- function(paramList=list()) {
    length <- length(paramList)
    if (length == 0)
	stop("invalid param list")

    cliParams <- ""

    for (i in 1:length) {
	if(is.logical(paramList[[i]])) {    
	    if(paramList[[i]]) cliParams <- paste(cliParams, "-", 
		    names(paramList[i]), " ", sep="")
	} else {
	    cliParams <- paste(cliParams, "-", names(paramList[i]), 
		    " ", paramList[[i]], " ", sep="")
	}
    }

    # removing the trailing space
    cliParams <- substr(cliParams, 1, nchar(cliParams)-1)
}

get_centers.DSC_MOA <- function(x, ...) {

    if (.jcall(x$javaObj, "Z", "implementsMicroClusterer")) {
	mClustering <- .jcall(x$javaObj, 
		"Lmoa/cluster/Clustering;", "getMicroClusteringResult")
    }else{
	mClustering <- .jcall(x$javaObj, 
		"Lmoa/cluster/Clustering;", "getClusteringResult")
    }

    # array of microclusters
    mClusters <- .jcall(mClustering, 
	    "Lmoa/core/AutoExpandVector;", "getClustering")

    
    # length of array
    length <- .jcall(mClusters, "I", "size")

    # empty clustering?
    if(length<1) {
	#warning("Clustering has no clusters!")
	return(data.frame())
    }

    m <- data.frame()

    # iterating over the array, extracting data to be plotted
    # the first point has already been used, so start from 2
    for (i in 1:length) {

	# will have to cast mCluster as moa/cluster/Cluster
	mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", i-1L)
	mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")
	center <- .jcall(mCluster, "[D", "getCenter") 
	weight <- .jcall(mCluster, "D", "getWeight") 
	m <- rbind(m, center)
    }

    colnames(m) <- paste("X", 1:ncol(m), sep="")
    
    m$weight <- weight
    
    # returning the matrix 
    m
}

