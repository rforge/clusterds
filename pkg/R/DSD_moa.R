DSD_moa <- function() {
  panel <- .jnew("moa/gui/clustertab/ClusteringAlgoPanel")
  strm <- .jcall(panel, "Lmoa/streams/clustering/ClusteringStream;", "getStream")
  .jcall(strm, "V", "prepareForUse")

  l <- list(Description = "RandomRBFGeneratorEvents",
            cliParams = "",
            javaObj = strm)

  class(l) <- "javaDS"
  l
}

#TODO: need to extend this to handle when numPoints > 1
get_instance.moa <- function(x, numPoints=1, ...) {
  inst <- .jcall(x$javaObj, "Lweka/core/Instance;", "nextInstance")
}
