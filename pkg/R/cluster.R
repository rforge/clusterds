## wrapper for cluster and recluster functions

cluster <- function(dsc, dsd, n=1, plot=FALSE, microclusters=FALSE, horizon=500, interval=100, sleep= 0.5, ...) { 
    if (n < 1)
	stop("numPoints must be >= 1")

    # looping through the stream, feeding the new datapoints into 
    # the algorithm
    
    i <- 0
    
    if(plot)
    	points <- data.frame()
    
    if(plot)
    	col <- gray.colors(horizon, start = 1, end = .7, gamma = 2.2)
    
    while (i<n) {
    	new_points <- .cluster(dsc, dsd, n)
    	i <- i + nrow(new_points)
    	
    	if(plot) {
    		points <- rbind(points, new_points)
    		
    		if(nrow(points) > horizon) {
    			points <- points[(nrow(points)-horizon +1):nrow(points),]
    		}
   	
   	 		if(i %% interval ==0) {
   	 			plot(points,col=col[horizon-nrow(points)+1: horizon],...)
   	 			if(nrow(get_centers(dsc))>0)
    				points(get_centers(dsc),col=2,pch=16)
    			if(microclusters)
   	 				if(nrow(get_microclusters(dsc))>0)
    					points(get_microclusters(dsc))
    			Sys.sleep(sleep)
    		}
    	}
    }
    
    # so cl <- cluster(cl, ...) also works
    invisible(dsc)
}

recluster <- function(macro, dsc, ...) {
    if(!is(macro, "DSC_Macro")) stop("macro is not of class DSC_marco")
    
    x <- get_centers(dsc)
    weight <- get_weights(dsc)
    macro$RObj$cluster(x, weight=weight, ...)
}


### Workers
.cluster <- function(dsc, x, n, ...) UseMethod(".cluster")

.cluster.DSC_MOA <- function(dsc, dsd, n, ...) {
    ## data has to be all doubles for MOA clusterers!
    	d <- get_points(dsd)
		x <- d
    	x <- .jcast(
		    .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(x))),
		    "weka/core/Instance"
		    )
    
    	.jcall(dsc$javaObj, "V", "trainOnInstanceImpl", x)
    
    	d
    	
}

.cluster.DSC_R <- function(dsc, dsd, n, ...) {
    ### dsc contains an RObj which is  a reference object with a cluster method
    	d <- get_points(dsd)
    	
    	dsc$RObj$cluster(d, ...)
    	
    	d
}

### FIXME: macro clusterers get all the data and can only be used once!!!
### FIXME: we should warn that the old clustering is completely list!
.cluster.DSC_Macro <- function(dsc, dsd, n, ...) {
    d <- get_points(dsd,n=n)
    dsc$RObj$cluster(d, ...)
    
    d
}


