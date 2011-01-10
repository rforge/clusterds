# clusterProb is a vector of probabilities for the clusters created
DSD_Static <- function(mu=c(0.5,0.5), sd=c(0.2,0.2), clusterProb=1) {
  
  mu <- as.matrix(mu)
  sd <- as.matrix(sd)

  if (ncol(mu) != ncol(sd) ||
      nrow(mu) != nrow(sd)) {
    stop("mu matrix is a different size than sd matrix")
  }

  # if the cluster probability differs in length, we default to an equal probability to all clusters
  if (length(clusterProb) != ncol(mu)) {
    prob <- 1/ncol(mu)
    clusterProb <- as.vector(array(prob, ncol(mu)))
  }

  l <- list(Description = "Static R Data Stream",
            mu = mu,
            sd = sd,
            clusterProb = clusterProb)
  class(l) <- "staticDS"
  l
}

get_instance.Static <- function(x, numPoints=1, ...) {
  # selecting a cluster, and generating the instances
  for (i in 1:numPoints) {
    numCluster <- sample(x=c(1:ncol(x$mu)), size=numPoints, replace=TRUE, prob=x$clusterProb)
    inst <- rbind(inst, rnorm(n=ncol(mu), mean=x$mu[numCluster,], x$sd[numCluster,]))
  }

  inst
}
