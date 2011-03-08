# DSClusterer - DataStreamClusterer interface
# all DSC classes have these methods
# and an additional function to create the DSC

getCenters <- function(x, ...) UseMethod("getCenters")

nclusters <- function(x) nrow(getCenters(x))

print.DSC <- function(x, ...) {
    cat(paste('DSC - Data Stream Clusterer:', x$description, '\n'))
    cat(paste('Number of clusters:', nclusters(x), '\n'))
    ### FIXME: print other stats
}

plot.DSC <- function(x, ..., method="pairs") {
    ## method can be pairs, plot or pc (projection with PCA)
    centers <- getCenters(x)
    if(ncol(centers)>2 && method=="pairs") pairs(centers, ...)
    else if(ncol(centers)>2 && method=="pc") {
	## we assume Euclidean here
	p <- prcomp(centers)
	plot(p$x)
    
    } else plot(centers, ...)
}

