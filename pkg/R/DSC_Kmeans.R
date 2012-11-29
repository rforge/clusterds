kmeans <- setRefClass("kmeans", 
	fields = list(
		data     = "data.frame",
		centers   = "numeric",
		iter.max	    = "numeric",
		nstart   = "numeric",
		algorithm   = "character",
		assignment = "numeric",
		details = "ANY",
		clusterCenters = "data.frame",
		weights = "numeric"
	), 

	methods = list(
		initialize = function(
			iter.max    = 10,
			nstart	    = 1,
			algorithm   = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")
			) {
		    
		    iter.max	<<- iter.max 
		    centers	<<- numeric() 
		    assignment	<<- numeric() 
		    weights	<<- numeric() 
		    clusterCenters <<- data.frame()
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
		
			assignment <<- kmeans$cluster
			clusterCenters <<- data.frame(kmeans$centers)
			details <<- kmeans
		} else
			assignment <<- 1:nrow(data)
	}
)

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

    class(l) <- c("DSC_Kmeans","DSC_Macro","DSC_R","DSC")
    l
}

### get centers
get_macroclusters.DSC_Kmeans <- function(x, ...) {
	if(length(x$RObj$clusterCenters) == 0) warning(paste(class(x)[1],": There are no clusters",sep=""))
	x$RObj$clusterCenters
} 
