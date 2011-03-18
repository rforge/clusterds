# TODO:
# Additional params
#	- rangeVar (for genPositiveDefMat)
#	- min/max on runif
#
DSD_Static <- function(k=2, d=2, mu, sigma, p, noise = 0) { 

    # if p isn't defined, we give all the clusters equal probability
    if (missing(p)) {
	p <- rep(1/k, k)
    }

    # for each d, random value between 0 and 1
    # we create a matrix of d columns and k rows
    if (missing(mu)) {
	mu <- matrix(runif(d*k, min=0, max=1), ncol=d)
    } else {
	mu <- as.matrix(mu)
    }

    # covariance matrix
    if (missing(sigma)) {
	sigma <- replicate(k,genPositiveDefMat(
			"unifcorrmat", 
			rangeVar=c(0.001,0.01), 
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


    l <- list(description = "Static R Data Stream",
	    k = k,
	    d = d,
	    mu = mu,
	    sigma = sigma,
	    p = p,
	    noise = noise)
    class(l) <- c("DSD","DSD_Static")
    l
}

getPoints.DSD_Static <- function(x, n=1, assignment = FALSE, ...) {

    clusterOrder <- sample(x=c(1:x$k), 
	    size=n, 
	    replace=TRUE, 
	    prob=x$p)

    data <- t(sapply(clusterOrder, FUN = function(i)
		    mvrnorm(1, mu=x$mu[i,], Sigma=x$sigma[[i]])))			
    ## Replace some points by random noise
    ## FIXME: [0,1]^d might not be a good choice. Some clusters can have
    ## points outside this range!
    if(x$noise) {
	repl <- runif(n)<x$noise 
	data[repl,] <- replicate(x$d, runif(sum(repl)))
	clusterOrder[repl] <- NA
    }

    if(assignment) attr(data, "assignment") <- clusterOrder
    data
}
