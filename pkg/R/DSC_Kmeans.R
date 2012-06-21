kmeans <- setRefClass("kmeans", 
	fields = list(
		x     = "ANY",
		centers   = "ANY",
		iter.max	    = "numeric",
		nstart   = "numeric",
		algorithm   = "character",
		clusters = "ANY",
		details = "ANY"
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
		    
		    
		    .self
		}

	),
)

kmeans$methods(cluster = function(x, ...) {
	    x <<- x
	    if(nrow(x)>centers) {
			kmeans<-kmeans(x=x, centers=centers, 
	    	iter.max = iter.max, nstart = nstart,
	    	algorithm = algorithm)
		
			clusters <<- kmeans$cluster
			details <<- kmeans
		} else
			clusters <<- 1:nrow(x)
	}
)

### creator    
DSC_Kmeans <- function(centers, iter.max = 10, nstart = 1,
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                     "MacQueen")) {

    kmeans <- kmeans$new( 
	    iter.max = iter.max, nstart = nstart,
	    algorithm = algorithm)

		kmeans$centers <- centers

    l <- list(description = "kmeans",
	    RObj = kmeans)

    class(l) <- c("DSC_Kmeans","DSC_Macro","DSC")
    l
}

### get centers
get_centers.DSC_Kmeans <- function(x, ...) x$RObj$x

nclusters.DSC_Kmeans <- function(x) length(unique(x$RObj$clusters))

get_assignment.DSC_Kmeans <- function(x) x$RObj$clusters
