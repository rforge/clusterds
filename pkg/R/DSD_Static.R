# clusterProb is a vector of probabilities for the clusters created
DSD_Static <- function(n=1, dims=2, ...) {

  #TODO: need to look into how to handle the list
  # optional params
  # mu=c(0.5,0.5), sd=c(0.2,0.2), clusterProb=1
  
#  mu <- as.matrix(mu)
#  sd <- as.matrix(sd)

#  if (ncol(mu) != ncol(sd) ||
#      nrow(mu) != nrow(sd)) {
#    stop("mu matrix is a different size than sd matrix")
#  }

  # if clusterProb isn't defined, we give all the clusters equal probability
  if (clusterProb == NULL) {
    prob <- 1/n
    clusterProb <- as.vector(array(prob, n))
  }

  #TODO: need to randomly generate mu values
  if (mu == NULL) {
    mu <- matrix(ncol=dims, nrow=n)
  }

  #TODO: need to randomly generate sd values
  if (sd == NULL) {
    sd <- matrix(ncol=dims, nrow=n)
  }

  if (n == 1) {

  } else if (n > 1) {
    
  } else {
    stop("invalid n")
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
      inst <- rnorm(n=ncol(x$mu), mean=x$mu[numCluster[[1]],], x$sd[numCluster[[1]],])
  
  # otherwise, we loop and create a number of instances
  } else if (n > 1) {
      numCluster <- sample(x=c(1:ncol(x$mu)), size=n, replace=TRUE, prob=x$clusterProb)
      inst <- rnorm(n=ncol(x$mu), mean=x$mu[numCluster[[1]],], x$sd[numCluster[[1]],])

    #TODO: is there a better way to do this than looping? (using vectorization)
    for (i in 2:n) {
      inst <- rbind(inst, rnorm(n=ncol(x$mu), mean=x$mu[numCluster[[i]],], x$sd[numCluster[[i]],]))
    }

  # error out
  } else {
    stop("invalid n")
  }

  inst
}
