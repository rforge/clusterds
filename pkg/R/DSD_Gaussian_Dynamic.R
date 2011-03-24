##TODO: currently this is just a copy from the static DS
##      will need additional work to add movement to the clusters

DSD_Gaussian_Dynamic <- function(k=2, d=2, mu, sigma, p) {

    l <- DSD_Gaussian_Static(k,d,mu,sigma,p)
    class(l) <- c("DSD","DSD_R","DSD_Gaussian_Dynamic")
    l
}

get_points.DSD_Gaussian_Dynamic <- function(x, n=1, assignment=FALSE, ...) {

    clusterOrder <- sample(x=c(1:x$k), 
	    size=n, 
	    replace=TRUE, 
	    prob=x$p)

    data <- t(sapply(clusterOrder, FUN = function(i)
		    mvrnorm(1, mu=x$mu[i,], Sigma=x$sigma[[i]])))
			
    ## TODO: Add noise
    if(x$noise) stop("Noise not implemented yet!")
    
    if(assignment) attr(data, "assignment") <- clusterOrder
    data
}
