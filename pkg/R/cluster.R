## wrapper for cluster and recluster functions

cluster <- function(dsc, dsd, n=1, ...) { 
    if (n < 1)
	stop("numPoints must be >= 1")

    # looping through the stream, feeding the new datapoints into 
    # the algorithm
    .cluster(dsc, dsd, n, ...)
    
    # so cl <- cluster(cl, ...) also works
    invisible(dsc)
}

recluster <- function(macro, dsc, ...) {
    if(!is(macro, "DSC_Macro")) stop("macro is not of class DSC_marco")
    
    x <- as.data.frame(get_centers(dsc))
    weight <- get_weights(dsc,scale=NULL)
    macro$RObj$cluster(x, weight=weight, ...)
}

cluster_animation <- function(dsc,dsd,n=1,interval=.1, horizon=500, pointInterval=100, weights=FALSE, scale=c(1,10), outdir=NULL,...) {
	if(is.null(outdir)) {
		cluster.ani(dsc, dsd, n, pointInterval, horizon, weights, scale,save=FALSE,interval=interval,...)
	} else {
		saveMovie(cluster.ani(dsc, dsd, n, pointInterval, horizon, weights, scale,save=TRUE,...), interval = interval, outdir = outdir)
	}
}

plot_animation <- function(dsd,n=1,interval=.1, horizon=500, pointInterval=100, outdir=NULL,...) {
	if(is.null(outdir)) {
		cluster.ani(, dsd, n, pointInterval, horizon,save=FALSE,interval=interval,...)
	} else {
		saveMovie(cluster.ani(, dsd, n, pointInterval, horizon,save=TRUE,...), interval = interval, outdir = outdir)
	}
}

cluster.ani <- function(dsc=NULL, dsd, n, pointInterval=100, horizon=5*pointInterval, weights=FALSE, scale=c(1,10),save=TRUE,interval=.1,...) {
	points <- data.frame()
	col <- gray.colors(horizon, start = 1, end = .7, gamma = 2.2)
	i <- 1
	j <- 0
	
	while ( j <= n) {
		d <- get_points(dsd)
		if(!is.null(dsc)) {
			.cluster(dsc, DSD_Wrapper(d,0),1)}
		j <- j + nrow(d)
		
		points <- rbind(points, d)
		if(nrow(points) > horizon) { points <- points[(nrow(points)-horizon +1):nrow(points),] }
		
		if(j %% pointInterval == 0) {
			plot(points,col=col[horizon-nrow(points)+1: horizon],...)
			if(!is.null(dsc) && length(get_centers(dsc))>0)
				if(weights)
					points(get_centers(dsc),col=2,pch=10,cex=get_weights(dsc,scale))
				else
					points(get_centers(dsc),col=2,pch=10)
			if(save)
				ani.pause()
			else
				Sys.sleep(interval)
			i <- i + 1
		}
	}
}



### Workers
.cluster <- function(dsc, x, n, ...) UseMethod(".cluster")

.cluster.DSC_MOA <- function(dsc, dsd, n, ...) {
    ## data has to be all doubles for MOA clusterers!
    for (i in 1:n) {

    	d <- get_points(dsd)
		x <- d
    	x <- .jcast(
		    .jnew("weka/core/DenseInstance", 1.0, .jarray(as.double(x))),
		    "weka/core/Instance"
		    )
    
    	.jcall(dsc$javaObj, "V", "trainOnInstanceImpl", x)
    }	
}

.cluster.DSC_R <- function(dsc, dsd, n, ...) {
    ### dsc contains an RObj which is  a reference object with a cluster method
    for (i in 1:n) {

    	d <- get_points(dsd)
    	
    	dsc$RObj$cluster(d, ...)
    }
}

### FIXME: macro clusterers get all the data and can only be used once!!!
### FIXME: we should warn that the old clustering is completely list!
.cluster.DSC_Macro <- function(dsc, dsd, n, ...) {
    d <- get_points(dsd,n=n)
    dsc$RObj$cluster(d, ...)
}


