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
		weights = "numeric",
		clusterWeights = "numeric"
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
		    clusterWeights <<- numeric() 
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
	
		    clusterWeights <<- sapply(1:centers, FUN =
			    function(i) sum(assignment==i))

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

get_macroclusters.DSC_Kmeans <- function(x) x$RObj$clusterCenters
get_macroweights.DSC_Kmeans <- function(x) x$RObj$clusterWeights

get_microclusters.DSC_Kmeans <- function(x) x$RObj$data
get_microweights.DSC_Kmeans <- function(x) x$RObj$weights

microToMacro.DSC_Kmeans <- function(x, micro) x$RObj$assignment[micro]

