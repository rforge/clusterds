## wrapper fo cluster functions

cluster <- function(dsc, dsd, n=1, ...) { 
    if (n < 1)
	stop("numPoints must be >= 1")

    # looping through the stream, feeding the new datapoints into 
    # the algorithm
    .cluster(dsc, dsd, n, ...)
    
    # so cl <- cluster(cl, ...) also works
    invisible(dsc)
}

.cluster <- function(dsc, x, n, ...) UseMethod(".cluster")

.cluster.DSC_Sample <- function(dsc, dsd, n, ...) {
   	x <- get_points(dsd,n=n)
	dsc$RObj$cluster(x)
}

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

.cluster.DSC_Macro <- function(dsc, dsd, n, ...) {
   	x <- get_points(dsd,n=n)
	dsc$RObj$cluster(x, ...)
}

.cluster.DSC_R <- function(dsc, dsd, n, ...) {
    ### dsc contains an RObj which is  a reference object with a cluster method
    for (i in 1:n) {
    	 x <- get_points(dsd)
    	dsc$RObj$cluster(x, ...)
    }
}

