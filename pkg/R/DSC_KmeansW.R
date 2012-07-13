kmeansW <- setRefClass("kmeansW", 
	fields = list(
		x	    = "ANY",
		centers	    = "ANY",
		iter.max    = "numeric",
		nstart	    = "numeric",
		clusters    = "ANY",
		details	    = "ANY",
		clusterCenters = "ANY"
	), 

	methods = list(
		initialize = function(
			iter.max    = 10,
			nstart	    = 1
			) {
		    
		    iter.max	<<- iter.max 
		    nstart	<<- nstart
		    
		    .self
		}
	)
)

kmeansW$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
	    x <<- x
	    if(nrow(x)>centers) {
			kmeansW<-kmeansW(x=x, weight=weight, centers=centers, 
	    	iter.max = iter.max, nstart = nstart)
		
			clusters <<- kmeansW$cluster
			clusterCenters <<- kmeansW$centers
			details <<- kmeansW
		} else
			clusters <<- 1:nrow(x)
	}
)

### creator    
DSC_KmeansW <- function(k, iter.max = 10, nstart = 1) {

    kmeansW <- new("kmeansW", 
	    iter.max = iter.max, nstart = nstart)

		kmeansW$centers <- k

    l <- list(description = "Weighted k-Means",
	    RObj = kmeansW)

    class(l) <- c("DSC_KmeansW","DSC_Macro","DSC")
    l
}

### get centers
get_centers.DSC_KmeansW <- function(x, ...) x$RObj$clusterCenters
