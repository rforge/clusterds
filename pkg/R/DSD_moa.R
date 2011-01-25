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

# we only create 1 data point at a time for Java instances
getPoints.DSD_moa <- function(x, n=1, ...) {
  if (n == 1) {
    inst <- .jcall(x$javaObj, "Lweka/core/Instance;", "nextInstance")
  } else if (n > 1) {    
    stop("getPoints.DSD_moa must have n = 1")
  } else {
    stop("invalid n")
  }

}
