# TODO:
# Additional params
#	- rangeVar (for genPositiveDefMat)
#	- min/max on runif
#
DSD_Static <- function(k=2, d=2, mu, sigma, p) { 
  
  # if p isn't defined, we give all the clusters equal probability
  if (missing(p)) {
    p <- rep(1/k, k)
  }

  # for each d, random value between 0 and 1
  # we create a matrix of d columns and k rows
  if (missing(mu)) {
	mu <- matrix(rep(runif(d, min=0, max=1), k), ncol=d)
  } else {
	mu <- as.matrix(mu)
  }

  # covariance matrix
  if (missing(sigma)) {
    sigma <- replicate(k,genPositiveDefMat(
	                        "unifcorrmat", 
	                        rangeVar=c(0.01,0.1), 
							dim=d)$Sigma,
					   simplify=F)
  }
  
  # error checking
  if (length(p) != k)
    stop("size of probability vector, p, must equal k")
	
  if (d < 0)
    stop("invalid number of dimensions")
	
  if (ncol(mu) != d || nrow(mu) != k)
    stop("invalid size of mu matrix")
	
  ## TODO: error checking on sigma
  # list of length k
  # d x d matrix in the list

  l <- list(Description = "Static R Data Stream",
			k = k,
			d = d,
            mu = mu,
			sigma = sigma,
            p = p)
  class(l) <- c("DSD","DSD_Static")
  l
}

getPoints.DSD_Static <- function(x, n=1, ...) {

    clusterOrder <- sample(x=c(1:x$k), 
                           size=n, 
						   replace=TRUE, 
						   prob=x$p)
						 
	data <- t(sapply(clusterOrder, FUN = function(i)
				mvrnorm(1, mu=x$mu[i,], Sigma=x$sigma[[i]])))			
	data
}
