Sample <- setRefClass("Sample", 
	fields = list(
		k		= "integer",
		stream_size	= "integer",

		centers		= "data.frame"
	), 

	methods = list(
		initialize = function(
			k	= 100L
			) {
		    
		    k		<<- k
		    stream_size	<<- 0L 
		    
		    .self
		}

	),
)

### Reservoir sampling: all values in the stream have the same prob. to
### be sampled
Sample $methods(cluster = function(x, ...) {
	    
	    ### fill with first values
	    if(nrow(centers) < k) {
		centers <<- rbind(centers,x)

	    }else{ ### replace values with decreasing probabilities
		r <- as.integer(runif(1, min=1, max=stream_size+1))
		if(r < k) centers[r, ] <<- x
	    }	 

	    stream_size <<- stream_size + 1L
	}
	)

   
DSC_Sample <- function(k = 100) {

    sample <- Sample$new(k = as.integer(k))


    l <- list(description = "Sample",
	    RObj = sample)

    class(l) <- c("DSC_Sample","DSC_Micro","DSC_R","DSC")
    l
}



### get centers
get_microclusters.DSC_Sample <- function(x, ...) {
    x$RObj$centers
}

get_microweights.DSC_Sample <- function(x, ...) {
    rep(1, nclusters(x))
}
