kmeans <- setRefClass("kmeans", 
	fields = list(
		data     = "data.frame",
		centers   = "ANY",
		iter.max	    = "numeric",
		nstart   = "numeric",
		algorithm   = "character",
		clusters = "ANY",
		details = "ANY",
		clusterCenters = "ANY",
		weights = "ANY"
	), 

	methods = list(
		initialize = function(
			iter.max    = 10,
			nstart	    = 1,
			algorithm   = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")
			) {
		    
		    iter.max	<<- iter.max 
		    nstart	<<- nstart
		    algorithm   <<- algorithm
		    data <<- data.frame()
		    
		    .self
		}

	),
)

kmeans$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
		if(length(data)>0) {warning("Kmeans: Previous data is being overridden")}
		weights <<- weight
	    data <<- x
	    if(nrow(data)>centers) {
			kmeans<-kmeans(x=data, centers=centers, 
	    	iter.max = iter.max, nstart = nstart,
	    	algorithm = algorithm)
		
			clusters <<- kmeans$cluster
			clusterCenters <<- kmeans$centers
			details <<- kmeans
		} else
			clusters <<- 1:nrow(data)
	}
)

get_microclusters.DSC_Kmeans <- function(x, ...) x$RObj$data

get_assignment.DSC_Kmeans <- function(dsc,points)  {
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
DSC_Kmeans <- function(k, iter.max = 10, nstart = 1,
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                     "MacQueen")) {

    kmeans <- kmeans$new( 
	    iter.max = iter.max, nstart = nstart,
	    algorithm = algorithm)

		kmeans$centers <- k

    l <- list(description = "k-Means",
	    RObj = kmeans)

    class(l) <- c("DSC_Kmeans","DSC_Macro","DSC")
    l
}

### get centers
get_centers.DSC_Kmeans <- function(x, ...) x$RObj$clusterCenters

get_weights.DSC_Kmeans <- function(x, scale=NULL) {
		
	nclusters <- unique(x$RObj$clusters)
	m <- unlist(lapply(nclusters,function(clusters){sum(x$RObj$weights[which(x$RObj$clusters==clusters)])}))
	
	if(!is.null(scale)) m <- map(m, scale)
	
	m
}

