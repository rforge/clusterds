## wrapper for cluster and recluster functions

cluster <- function(dsc, dsd, n=1, ...) { 
    if (n < 1)
	stop("numPoints must be >= 1")

    # looping through the stream, feeding the new datapoints into 
    # the algorithm
    .cluster(dsc, dsd, n, ...)
    
    # so cl <- cluster(cl, ...) also works
    invisible(dsc)
}

recluster <- function(macro, dsc, ...) {
    if(!is(macro, "DSC_Macro")) stop("macro is not od class DSC_marco")
    
    x <- get_centers(dsc)
    weight <- get_weights(dsc)
    macro$RObj$cluster(x, weight=weight, ...)
}


### Workers
.cluster <- function(dsc, x, n, ...) UseMethod(".cluster")

.cluster.DSC_MOA <- function(dsc, dsd, n, ...) {
    ## data has to be all doubles for MOA clusterers!
    for (i in 1:n) {
		x <- get_points(dsd)
    	x <- .jcast(
		    .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(x))),
		    "weka/core/Instance"
		    )
    
    	.jcall(dsc$javaObj, "V", "trainOnInstanceImpl", x)
    }
}

.cluster.DSC_R <- function(dsc, dsd, n, ...) {
    ### dsc contains an RObj which is  a reference object with a cluster method
    for (i in 1:n) {
    	x <- get_points(dsd)
    	dsc$RObj$cluster(x, ...)
    }
}

### FIXME: macro clusterers get all the data and can only be used once!!!
### FIXME: we should warn that the old clustering is completely list!
.cluster.DSC_Macro <- function(dsc, dsd, n, ...) {
    x <- get_points(dsd,n=n)
    dsc$RObj$cluster(x, ...)
}


