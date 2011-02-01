cluster <- function(clusterer, x, n=10000) { 
    if (n < 2)
	stop("numPoints must be > 1")

    # looping through the stream, feeding the new datapoints into 
    # the algorithm
    for (i in 1:n) {
	inst <- getPoints(x)

	# is the clusterer in java?
	if(clusterer@javaObj != NULL) {
	    # java clusterer

	    # casting the inst to a java object
	    if (!is(x, "DSD_moa")) {
		inst <- .jnew("weka/core/Instance", 1, inst)
	    }

	    .jcall(clusterer$javaObj, "V", "trainOnInstanceImpl", inst)
	}else{
	    # R clusterer
	    # FIXME: this needs more thinking! Maybe we should use a environment
	    # to simulate pass by reference.
	    clusterer <- clusterer$clusterFun(clusterer, inst)
	}
    }

    ### so cl <- cluster(cl, ...) also works
    invisible(clusterer)

}
