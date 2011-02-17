# clusterProb is a vector of probabilities for the clusters created
#TODO: identify other params to alter the covariance matrix, and the min/max on runif etc
DSD_Static <- function(k=1, d=2, mu, sigma, p ) { 
  
  # if clusterProb isn't defined, we give all the clusters equal probability
  if (missing(p)) {
    p <- rep(1/k, k)
  } else {
  
    #TODO: check on the size of the p vector
  }

  ## for each d, random value between 0 and 1
  ## we create a matrix of d columns and k rows
  if (missing(mu)) {
	mu <- matrix(rep(runif(d, min=0, max=1), k), ncol=d)
  } else {
	mu <- as.matrix(mu)
	
	#TODO: check on the num rows & cols
  }

  # covariance matrix
  #TODO: rangeVar should be an argument 
  if (missing(sigma)) {
    sigma <- replicate(k,genPositiveDefMat("unifcorrmat", 
	                        rangeVar=c(0.01,0.1), 
							dim=d)$Sigma,
					   simplify=F)
  }

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
