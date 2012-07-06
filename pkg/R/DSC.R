# DSClusterer - DataStreamClusterer interface
# all DSC classes have these methods
# and an additional function to create the DSC

get_centers.default <- function(x, ...) {
   stop(gettextf("get_centers not implemented for class '%s'.", class(x)))
}

get_centers <- function(x, ...) UseMethod("get_centers")

get_copy <- function(x) UseMethod("get_copy")

nclusters <- function(x) UseMethod("nclusters")

get_microclusters <- function(x) UseMethod("get_microclusters")

nclusters.DSC <- function(x) nrow(get_centers(x))

get_assignment <- function(dsc,points,n) UseMethod("get_assignment")

get_weights <- function(dsc, ...) UseMethod("get_weights")

get_microclusters.default <- function(x) {
	UseMethod("get_centers")
}

get_assignment.DSC <- function(dsc,points,n) {
	d <- points
	c <- get_centers(dsc)
	dist <- dist(d,c)
	#Find the minimum distance and save the class
	predict <- apply(dist, 1, which.min)
	#predict <- unlist(lapply(predict, function(y) assignment[y]))
	predict[is.null(predict)] <- 1
	predict[is.na(predict)] <- 1
	
	predict	
}

get_copy.DSC_Macro <- function(x) {
	temp <- x
	temp$RObj <- x$RObj$copy(TRUE)
	temp
}

get_weights.DSC <- function(x) {
	rep(1,nclusters(x))
}

get_copy.DSC_R <- function(x) {
	temp <- x
	temp$RObj <- x$RObj$copy(TRUE)
	temp
}

get_copy.DSC_MOA <- function(x) {
	stop("Copy not yet implemented for MOA")
}

print.DSC <- function(x, ...) {
    cat(paste('DSC - Data Stream Clusterer:', x$description, '\n'))
    cat(paste('Number of clusters:', nclusters(x), '\n'))
}

plot.DSC <- function(x, dsd = NULL, n = 1000, color=TRUE, main = "Micro clusters", ..., method="pairs") {
    ## method can be pairs, plot or pc (projection with PCA)
    centers <- get_centers(x)
    if(!is.null(dsd)) {
    	d <- get_points(dsd, n, assignment = TRUE)
    	plot(d, col="gray", pch=attr(d, "assignment"))
    	if(color) points(centers, col=get_assignment(x), pch=15)
    	else points(centers, pch=15)
    }
    else if(ncol(centers)>2 && method=="pairs") {
    	if(color) pairs(centers, col=get_assignment(x), main=main, ...)
    	else pairs(centers, main=main, ...)
    }
    else if(ncol(centers)>2 && method=="pc") {
		## we assume Euclidean here
		p <- prcomp(centers)
		if(color) plot(p$x, main=main, col=get_assignment(x), ...)
		else  plot(p$x, main=main, ...)
    } else {
    	if(color) plot(centers, col=get_assignment(x), main=main, ...)
    	else plot(centers, main=main, ...)
    }    
}

