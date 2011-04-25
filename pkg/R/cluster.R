## wrapper fo cluster functions

cluster <- function(dsc, dsd, n=1) { 
    if (n < 1)
	stop("numPoints must be >= 1")

    # looping through the stream, feeding the new datapoints into 
    # the algorithm
    for (i in 1:n) {
	.cluster(dsc, get_points(dsd))
    }
    
    # so cl <- cluster(cl, ...) also works
    invisible(dsc)
}

.cluster <- function(dsc, inst) UseMethod(".cluster")

.cluster.DSC_MOA <- function(dsc, inst) {
    ## data has to be all doubles for MOA clusterers!
    inst <- .jnew("weka/core/Instance", 1.0, as.double(inst))
    .jcall(dsc$javaObj, "V", "trainOnInstanceImpl", inst)
}

.cluster.DSC_R <- function(dsc, inst) {
    dsc <- dsc$clusterFun(dsc, inst)
}

