hierarchical <- setRefClass("hierarchical", 
	fields = list(
		data	= "data.frame",
		dataWeights = "numeric",
		d	= "matrix",
		method  = "character",
		members	= "ANY",
		k	= "numeric",
		assignment = "numeric",
		details = "ANY",
		centers	= "data.frame",
		weights = "numeric"
	), 

	methods = list(
		initialize = function(
			method	= "complete",
			members = NULL
			) {
		    
		    data	<<- data.frame()
		    dataWeights	<<- numeric()
		    weights	<<- numeric()
		    centers	<<- data.frame()
		    method	<<- method 
		    members	<<- members
		    
		    .self
		}

	),
)

hierarchical $methods(cluster = function(x,  weight = rep(1,nrow(x)), ...) {
	    if(length(data)>0) {
		warning("Hierarchical: Previous data is being overridden")
	    }
	    
	    dataWeights <<- weight
	    data <<- x
	    
	    if(nrow(data)>=2) {
		hierarchical <-hclust(d=dist(x), method = method, 
			members= members)
		
		if(k < length(unlist(hierarchical['height'])))
		    memb <- cutree(hierarchical, k = k)
		else
		    memb <- 1
		
		assignment <<- memb
		details <<- hierarchical
	    
		centers <<- as.data.frame(t(sapply(1:k, FUN=
			function(i) colMeans(data[assignment==i,]))))
		weights <<- sapply(1:k, FUN =
			function(i) sum(dataWeights[assignment==i], na.rm=TRUE))

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

    class(l) <- c("DSC_Hierarchical","DSC_Macro","DSC_R","DSC")
    l
}

get_macroclusters.DSC_Hierarchical <- function(x) x$RObj$centers
get_macroweights.DSC_Hierarchical <- function(x) x$RObj$weights

