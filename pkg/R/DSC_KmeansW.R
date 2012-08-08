kmeansW <- setRefClass("kmeansW", 
	fields = list(
		data	    = "data.frame",
		centers	    = "ANY",
		iter.max    = "numeric",
		nstart	    = "numeric",
		clusters    = "ANY",
		details	    = "ANY",
		clusterCenters = "ANY",
		weights = "ANY"
	), 

	methods = list(
		initialize = function(
			iter.max    = 10,
			nstart	    = 1
			) {
		    
		    data <<- data.frame()
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
		
			clusters <<- kmeansW$cluster
			clusterCenters <<- kmeansW$centers
			details <<- kmeansW
		} else
			clusters <<- 1:nrow(data)
	}
)

get_microclusters.DSC_KmeansW <- function(x, ...) x$RObj$data

get_assignment.DSC_KmeansW <- function(dsc,points)  {
	d <- points
	c <- get_microclusters(dsc)
	dist <- dist(d,c)
	#Find the minimum distance and save the class
	predict <- apply(dist, 1, which.min)
	predict <- unlist(lapply(predict, function(y) dsc$RObj$clusters[y]))
	predict[is.null(predict)] <- 1
	predict[is.na(predict)] <- 1
	
	predict	
}

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

get_weights.DSC_KmeansW <- function(x, scale=NULL) {
		
	nclusters <- unique(x$RObj$clusters)
	m <- unlist(lapply(nclusters,function(clusters){sum(x$RObj$weights[which(x$RObj$clusters==clusters)])}))
	
	if(!is.null(scale)) m <- map(m, scale)
	
	m
}

