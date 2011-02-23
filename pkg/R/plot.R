plot.DSC <- function(x, ...) {
  plot(getCenters(x), ...)
}

getCenters <- function(x, ...) {

  if (.jcall(x$javaObj, "Z", "implementsMicroClusterer")) {

    # Clustering of microclusters
    mClustering <- .jcall(x$javaObj, "Lmoa/cluster/Clustering;", "getMicroClusteringResult")

    # array of microclusters
    mClusters <- .jcall(mClustering, "Lmoa/core/AutoExpandVector;", "getClustering")

    # length of array
    length <- .jcall(mClusters, "I", "size")

    if (length < 2)
      stop("not enough microclusters, please cluster more data")

    # this matrix will hold the data to plot
    m <- matrix(ncol=2, nrow=length)
    colnames(m) <- c("x", "y")

    # iterating over the array, extracting data to be plotted
    for (i in 1:length) {
 
      # will have to cast mCluster as moa/cluster/Cluster
      mCluster <- .jcall(mClusters, "Ljava/lang/Object;", "get", i-1L)
      mCluster <- .jcast(mCluster, "Lmoa/cluster/Cluster")

      # array of doubles for each dimension
      center <- .jcall(mCluster, "[D", "getCenter") 
      
      #TODO: projection
      #print(c("debugging: dimensions = ", length(center)))
      # if the data is 2 dimensional, we don't have to project 
      #if (length(center) == 2) {
        m[i, 1] = center[1]
        m[i, 2] = center[2]
      #}
    }
    
    # returning the matrix 
    m
  }
}
