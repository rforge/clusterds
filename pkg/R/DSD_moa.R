DSD_moa <- function() {

  #TODO: shouldn't have to create the algo panel
  panel <- .jnew("moa/gui/clustertab/ClusteringAlgoPanel")
  strm <- .jcall(panel, "Lmoa/streams/clustering/ClusteringStream;", "getStream")
  .jcall(strm, "V", "prepareForUse")

  l <- list(Description = "RandomRBFGeneratorEvents",
            cliParams = "",
            javaObj = strm)

  class(l) <- c("DSD","DSD_moa")
  l
}

#TODO: need to extend this to handle when numPoints > 1
getPoints.moa <- function(x, numPoints=1, ...) {
  inst <- .jcall(x$javaObj, "Lweka/core/Instance;", "nextInstance")
}
