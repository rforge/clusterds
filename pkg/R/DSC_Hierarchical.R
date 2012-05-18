hierarchical <- setRefClass("hierarchical", 
	fields = list(
		data = "matrix",
		d     = "matrix",
		method   = "character",
		members	    = "ANY",
		k = "numeric",
		assignment = "numeric",
		details = "ANY",
		time = "numeric"
	), 

	methods = list(
		initialize = function(
			method	    = "complete",
			members   = NULL
			) {
		    
		    method	<<- method 
		    members	<<- members
		    
		    
		    .self
		}

	),
)

hierarchical $methods(cluster = function(x, ...) {
	    data <<- x
		hierarchical <-hclust(d=dist(x), method = method, members= members)
		memb <- cutree(hierarchical, k = k)

		assignment <<- memb
		details <<- hierarchical
	}
)

### creator    
DSC_Hierarchical <- function(k, method = "complete", members = NULL) {

    hierarchical <- hierarchical $new( 
	    method = method, members = members)

	hierarchical$k <- k

    l <- list(description = "hierarchical",
	    RObj = hierarchical)

    class(l) <- c("DSC_Hierarchical","DSC_Macro","DSC")
    l
}

### get centers
get_centers.DSC_Hierarchical <- function(x, ...) x$RObj$data

nclusters.DSC_Hierarchical <- function(x)  length(unique(x$RObj$assignment))

get_assignment.DSC_Hierarchical <- function(x, ...) x$RObj$assignment
