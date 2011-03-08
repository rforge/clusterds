## wrapper fo cluster functions

cluster <- function(dsc, dsd, n=10000) { 
  if (n < 2)
    stop("numPoints must be > 1")

  # looping through the stream, feeding the new datapoints into 
  # the algorithm
  for (i in 1:n) {
    inst <- getPoints(dsd)
    .cluster(dsc, inst)
	
    # so cl <- cluster(cl, ...) also works
    invisible(dsc)
  }
}

.cluster <- function(dsc, inst) UseMethod(".cluster")

.cluster.DSC_MOA <- function(dsc, inst) {
	inst <- .jnew("weka/core/Instance", 1, inst)
    .jcall(dsc$javaObj, "V", "trainOnInstanceImpl", inst)
	invisible(dsc)
}

.cluster.DSC_R <- function(dsc, inst) {
	dsc <- dsc$clusterFun(dsc, inst)
	invisible(dsc)
}

