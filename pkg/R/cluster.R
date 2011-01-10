cluster <- function(clusterer, x, numPoints=10000) { 
  if (numPoints < 2)
    stop("numPoints must be > 1")

  # looping through the stream, feeding the new datapoints into 
  # the algorithm
  for (i in 1:numPoints) {
    inst <- get_instance(x)
    
    # casting the inst to a java object
    if (class(x) != "javaDS") {
      inst <- .jnew("weka/core/Instance", 1, inst)
    }

    .jcall(clusterer$javaObj, "V", "trainOnInstanceImpl", inst)
  }
}
