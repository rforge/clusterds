hierarchical <- setRefClass("hierarchical", 
	fields = list(
		data = "ANY",
		d     = "matrix",
		method   = "character",
		members	    = "ANY",
		k = "numeric",
		assignment = "numeric",
		details = "ANY"
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
	    if(nrow(data)>=2) {
			hierarchical <-hclust(d=dist(x), method = method, members= members)
			if(k < length(unlist(hierarchical['height'])))
				memb <- cutree(hierarchical, k = k)
			else
				memb <- 1
			assignment <<- memb
			details <<- hierarchical
		}
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
