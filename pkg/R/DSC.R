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
    cat(paste('Number of micro clusters:', nclusters(x), '\n'))
    ### FIXME: print other stats
}

plot.DSC <- function(x, main = "Microclusters", ..., method="pairs") {
    ## method can be pairs, plot or pc (projection with PCA)
    centers <- get_centers(x)
    if(ncol(centers)>2 && method=="pairs") pairs(centers, ...)
    else if(ncol(centers)>2 && method=="pc") {
	## we assume Euclidean here
	p <- prcomp(centers)
	plot(p$x)
    
    } else plot(centers, main=main, ...)
}

