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
  class(l) <- c("DSD","DSD_Static")
  l
}

getPoints.DSD_Static <- function(x, n=1, ...) {

  # if n == 1, we just create 1 instance
  if (n == 1) {
      numCluster <- sample(x=c(1:ncol(x$mu)), size=n, replace=TRUE, prob=x$clusterProb)
      inst <- rnorm(n=nrow(x$mu), mean=x$mu[numCluster,], x$sd[numCluster,])
  
  # otherwise, we loop and create a number of instances
  } else if (n > 1) {
      numCluster <- sample(x=c(1:ncol(x$mu)), size=n, replace=TRUE, prob=x$clusterProb)
      inst <- rnorm(n=nrow(x$mu), mean=x$mu[numCluster,], x$sd[numCluster,])

    #TODO: is there a better way to do this than looping? (using vectorization)
    for (i in 2:n) {
      numCluster <- sample(x=c(1:ncol(x$mu)), size=n, replace=TRUE, prob=x$clusterProb)
      inst <- rbind(inst, rnorm(n=ncol(x$mu), mean=x$mu[numCluster,], x$sd[numCluster,]))
    }
  } else {
    stop("invalid n")
  }

  inst
}
