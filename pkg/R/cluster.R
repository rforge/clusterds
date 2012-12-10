## wrapper for cluster functions

cluster <- function(dsc, dsd, n=1, verbose=FALSE, ...) { 
    if (n < 1)
	stop("numPoints must be >= 1")

    # looping through the stream, feeding the new datapoints into 
    # the algorithm
    .cluster(dsc, dsd, n, verbose, ...)
    
    # so cl <- cluster(cl, ...) also works
    invisible(dsc)
}

### Workers
.cluster <- function(dsc, x, n, verbose=FALSE, ...) UseMethod(".cluster")

.cluster.DSC_MOA <- function(dsc, dsd, n, verbose=FALSE, ...) {
    ## data has to be all doubles for MOA clusterers!
    for (i in 1:n) {

	if(verbose && !i%%100) cat("Processed", i, "points\n")

	d <- get_points(dsd, 1)
    	x <- .jcast(
		    .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(d))),
		    "weka/core/Instance"
		    )
    
    	.jcall(dsc$javaObj, "V", "trainOnInstanceImpl", x)
    }	
}

.cluster.DSC_R <- function(dsc, dsd, n, verbose=FALSE, ...) {
    ### dsc contains an RObj which is  a reference object with a cluster method
    for (i in 1:n) {

	if(verbose && !i%%100) cat("Processed", i, "points\n")
    	
	d <- get_points(dsd,1)
    	
    	dsc$RObj$cluster(d, ...)
    }
}

.cluster.DSC_Macro <- function(dsc, dsd, n, ...) {
    d <- get_points(dsd,n=n)
    dsc$RObj$cluster(d, ...)
}


