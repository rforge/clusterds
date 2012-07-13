Sample <- setRefClass("Sample", 
	fields = list(
		x     = "ANY",
		size     = "numeric",

		centers	    = "data.frame"
	), 

	methods = list(
		initialize = function(
			size	    = 100
			) {
		    
		    size	<<- size 
		    
		    .self
		}

	),
)

### FIXME: We need reservoir sampling here!
Sample $methods(cluster = function(x, ...) {
	     x <<- x
	     centers <<- x[sample(nrow(x), size=size), ]
	     }
)

   
DSC_Sample <- function(size = 100) {

    sample <- Sample$new( size = size)


    l <- list(description = "Sample",
	    RObj = sample)

    class(l) <- c("DSC_Sample","DSC_R","DSC")
    l
}



### get centers
get_centers.DSC_Sample <- function(x, ...) x$RObj$centers
