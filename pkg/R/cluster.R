## wrapper fo cluster functions

cluster <- function(dsc, dsd, n=1) { 
    if (n < 1)
	stop("numPoints must be >= 1")

    # looping through the stream, feeding the new datapoints into 
    # the algorithm
    for (i in 1:n) .cluster(dsc, get_points(dsd))
    
    # so cl <- cluster(cl, ...) also works
    invisible(dsc)
}

.cluster <- function(dsc, x) UseMethod(".cluster")

.cluster.DSC_MOA <- function(dsc, x) {
    ## data has to be all doubles for MOA clusterers!
    x <- .jcast(
	    .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(x))),
	    "weka/core/Instance"
	    )
    
    .jcall(dsc$javaObj, "V", "trainOnInstanceImpl", x)
}

.cluster.DSC_R <- function(dsc, x) {
    ### dsc contains an RObj which is  a reference object with a cluster method
    dsc$RObj$cluster(x)
}

