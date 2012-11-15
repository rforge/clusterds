hierarchical <- setRefClass("hierarchical", 
	fields = list(
		data = "data.frame",
		d     = "matrix",
		method   = "character",
		members	    = "ANY",
		k = "numeric",
		assignment = "numeric",
		details = "ANY",
		weights = "numeric"
	), 

	methods = list(
		initialize = function(
			method	    = "complete",
			members   = NULL
			) {
		    
		    data <<- data.frame()
		    weights <<- numeric()
		    method	<<- method 
		    members	<<- members
		    
		    
		    .self
		}

	),
)

hierarchical $methods(cluster = function(x,  weight = rep(1,nrow(x)), ...) {
		if(length(data)>0) {warning("Hierarchical: Previous data is being overridden")}
		weights <<- weight
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

    l <- list(description = paste("Hierarchical -", method),
	    RObj = hierarchical)

    class(l) <- c("DSC_Hierarchical","DSC_R","DSC_Macro","DSC")
    l
}

