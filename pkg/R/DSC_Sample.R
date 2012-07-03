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


Sample$methods(cluster = function(newdata, verbose = FALSE) {
	     x <<- x
	     centers <<- x[sample(nrow(x), size=size), ]
}
)

   
DSC_Sample <- function(size = 100) {

    sample <- sample$new( size = size)


    l <- list(description = "sample",
	    RObj = sample)

    class(l) <- c("DSC_Sample","DSC_R","DSC")
    l
}



### get centers
get_centers.DSC_Kmeans <- function(x, ...) x$RObj$centers