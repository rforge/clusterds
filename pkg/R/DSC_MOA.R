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

get_microclusters.DSC_MOA <- function(x, ...) {

    error <- tryCatch( {
    
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
	}
	
	,error=function(err){
	    return(1)
	})

    
    # length of array
    if(error != 1) {
    	length <- .jcall(mClusters, "I", "size")
    } else {length <- 0}

    # empty clustering?
    if(length<1) {
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
    
    
    # returning the matrix 
    m
}

get_microweights.DSC_MOA <- function(x) {

    if (.jcall(x$javaObj, "Z", "implementsMicroClusterer")) {
	mClustering <- .jcall(x$javaObj, 
		"Lmoa/cluster/Clustering;", "getMicroClusteringResult")
    }else{
	mClustering <- .jcall(x$javaObj, 
		"Lmoa/cluster/Clustering;", "getClusteringResult")
    }

    # array of microclusters
  	error <- tryCatch(mClusters <- .jcall(mClustering, 
	    "Lmoa/core/AutoExpandVector;", "getClustering"),error=function(err){
	    	return(1)
	    })

    
    # length of array
    if(error != 1) {
    	length <- .jcall(mClusters, "I", "size")
    } else {length <- 0}

    # empty clustering?
    if(length<1) {
	return(numeric())
    }

    m <- data.frame()

    # iterating over the array, extracting data to be plotted
    # the first point has already been used, so start from 2
    for (i in 1:length) {

	# will have to cast mCluster as moa/cluster/Cluster
	mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", i-1L)
	mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")
	weight <- .jcall(mCluster, "D", "getWeight") 
	m <- rbind(m, weight)
    }

    colnames(m) <- paste("X", 1:ncol(m), sep="")
    
    unlist(m)
}

get_copy.DSC_MOA <- function(x) {
	#TODO
	stop("Copy not yet implemented for MOA")
    }
