kmeansW <- setRefClass("kmeansW", 
	fields = list(
		data	    = "data.frame",
		centers	    = "numeric",
		iter.max    = "numeric",
		nstart	    = "numeric",
		assignment    = "numeric",
		details	    = "ANY",
		clusterCenters = "data.frame",
		weights = "numeric"
	), 

	methods = list(
		initialize = function(
			iter.max    = 10,
			nstart	    = 1
			) {
		    
		    data <<- data.frame()
		    centers <<- numeric()
		    assignment <<- numeric()
		    weights <<- numeric()
		    clusterCenters <<- data.frame()
		    iter.max	<<- iter.max 
		    nstart	<<- nstart
		    
		    .self
		}
	)
)

kmeansW$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
		if(length(data)>0) {warning("KmeansW: Previous data is being overridden")}
		weights <<- weight
	    data <<- x
	    if(nrow(x)>centers) {
			kmeansW<-kmeansW(x=data, weight=weight, centers=centers, 
	    	iter.max = iter.max, nstart = nstart)
		
			assignment <<- kmeansW$cluster
			clusterCenters <<- data.frame(kmeansW$centers)
			details <<- kmeansW
		} else
			assignment <<- 1:nrow(data)
	}
)

### creator    
DSC_KmeansW <- function(k, iter.max = 10, nstart = 1) {

    kmeansW <- new("kmeansW", 
	    iter.max = iter.max, nstart = nstart)

		kmeansW$centers <- k

    l <- list(description = "Weighted k-Means",
	    RObj = kmeansW)

    class(l) <- c("DSC_KmeansW","DSC_Kmeans","DSC_R","DSC_Macro","DSC")
    l
}

