# DSClusterer - DataStreamClusterer interface
# all DSC classes have these methods
# and an additional function to create the DSC

get_centers.default <- function(x, ...) {
   stop(gettextf("get_centers not implemented for class '%s'.", class(x)))
}

get_centers <- function(x, ...) UseMethod("get_centers")

nclusters <- function(x) nrow(get_centers(x))

print.DSC <- function(x, ...) {
    cat(paste('DSC - Data Stream Clusterer:', x$description, '\n'))
    cat(paste('Number of (micro) clusters:', nclusters(x), '\n'))
}

plot.DSC <- function(x, dsd = NULL, n = 1000, macro=TRUE, main = "Micro clusters", ..., method="pairs") {
    ## method can be pairs, plot or pc (projection with PCA)
    centers <- get_centers(x)
    if(!is.null(dsd)) {
    	d <- get_points(dsd, n, assignment = TRUE)
    	plot(d, col="gray", pch=attr(d, "assignment"))
		if(macro && !is.null(x$assignment))
    		points(centers, col=x$assignment+1, pch=15)
    	else
    		points(centers, col="red", pch=3)
    }
    else if(ncol(centers)>2 && method=="pairs") {
    	if(macro && !is.null(x$assignment))
    		pairs(centers, col=x$assignment+1, main=main, ...)
    	else
    		pairs(centers, main=main, ...)
    }
    else if(ncol(centers)>2 && method=="pc") {
	## we assume Euclidean here
	p <- prcomp(centers)
	if(macro && !is.null(x$assignment))
		plot(p$x, main=main, col=x$assignment+1, ...)
	else
		plot(p$x, main=main, ...)
    
    } else {
    	if(macro && !is.null(x$assignment))
    		plot(centers, col=x$assignment+1, main=main, ...)
    	else
    		plot(centers, main=main, ...)
    }    
}

