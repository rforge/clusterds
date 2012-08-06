

static <- setRefClass("static", 
	fields = list(
		centers = "data.frame",
		microclusters = "data.frame",
		weights = "numeric"
	), 

	methods = list(
		initialize = function(
			centers	    = data.frame(),
			microclusters	    = data.frame(),
			weights   = numeric()
			) {
		    
		    centers	<<- centers 
		    microclusters	<<- microclusters
		    weights   <<- weights
		    
		    .self
		}

	),
)

static$methods(cluster = function(newdata, verbose = FALSE) {
			stop("You cannot cluster DSC_Static")
	    }
)

DSC_Static <- function(centers=NULL,microclusters=NULL,weights=NULL) {

    static <- static$new(centers=centers,microclusters=microclusters,weights=weights)

    l <- list(description = "Static",
	    RObj = static)

    class(l) <- c("DSC_Static","DSC_R","DSC")
    l
    l
}

### get centers
get_centers.DSC_Static <- function(x, ...) x$RObj$centers

get_microclusters.DSC_Static <- function(x, ...) x$RObj$microclusters

get_weights.DSC_Static <- function(x, scale = NULL)  {
    weight <- x$RObj$weights

    if(!is.null(scale)) weight <- map(weight, scale)
    
    weight
}
