cluster <- function(clusterer, x, n=10000) { 
  if (n < 2)
    stop("numPoints must be > 1")

  # looping through the stream, feeding the new datapoints into 
  # the algorithm
  for (i in 1:n) {
    inst <- getPoints(x)
    
    #TODO: how do we check if either of the classes == javaDS ??
    # casting the inst to a java object
    if (!is(x, "DSD_moa")) {
      inst <- .jnew("weka/core/Instance", 1, inst)
    }

    .jcall(clusterer$javaObj, "V", "trainOnInstanceImpl", inst)
  }

  ### so cl <- cluster(cl, ...) also works
  invisible(clusterer)

}
