kmeans <- setRefClass("kmeans", 
	fields = list(
		x     = "matrix",
		centers   = "ANY",
		iter.max	    = "numeric",
		nstart   = "numeric",
		algorithm   = "character",
		clusters = "matrix",
		details = "ANY",
		time = "numeric"
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
	    if(nrow(x)>=centers) {
			kmeans<-kmeans(x=x, centers=centers, 
	    	iter.max = iter.max, nstart = nstart,
	    	algorithm = algorithm)
		
			clusters <<- kmeans$centers
			details <<- kmeans
		} else
			clusters <<- x
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
get_centers.DSC_Kmeans <- function(x, ...) x$RObj$clusters

nclusters.DSC_Kmeans <- function(x) nrow(get_centers(x))

get_assignment.DSC_Kmeans <- function(x) 1:nrow(get_centers(x))
