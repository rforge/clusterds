Sample <- setRefClass("Sample", 
	fields = list(
		data	    = "data.frame",
		size	    = "numeric",
		stream_size = "numeric",

		centers	    = "data.frame"
	), 

	methods = list(
		initialize = function(
			size	    = 100
			) {
		    
		    data <<- data.frame()
		    size	<<- size 
		    stream_size	<<- 0 
		    
		    .self
		}

	),
)

### Reservoir sampling: all values in the stream have the same prob. to
### be sampled
Sample $methods(cluster = function(x, ...) {
	    data <<- rbind(data,x)

	    ### fill with first values
	    if(nrow(centers) < size) {
		centers <<- rbind(centers,x)

	    }else{ ### replace values with decreasing probabilities
		r <- as.integer(runif(1, min=1, max=stream_size+1))
		if(r < size) centers[r, ] <<- x
	    }	 

	    stream_size <<- stream_size+1
	}
	)

   
DSC_Sample <- function(size = 100) {

    sample <- Sample$new( size = size)


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
