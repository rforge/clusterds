cluster <- function(dsc, dsd, n=10000) { 
  if (n < 2)
    stop("numPoints must be > 1")

  # looping through the stream, feeding the new datapoints into 
  # the algorithm
  for (i in 1:n) {
    inst <- getPoints(dsd)
    if(dsc$javaObj != NULL) {

      # casting the inst to a java object
      if (!is(dsd, "DSD_moa")) {
        inst <- .jnew("weka/core/Instance", 1, inst)
      }

      .jcall(dsc$javaObj, "V", "trainOnInstanceImpl", inst)

    } else {

      # R clusterer
      # FIXME: this needs more thinking! Maybe we should use a environment
      # to simulate pass by reference.
      clusterer <- dsc$clusterFun(dsc, inst)
    }
  }

    ### so cl <- cluster(cl, ...) also works
    invisible(dsc)
}
