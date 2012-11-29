### DSC - Data Stream Clusterer interface

### all DSC classes have these interface methods

get_centers <- function(x, ...) UseMethod("get_centers")
get_centers.default <- function(x, ...) {
   stop(gettextf("get_centers not implemented for class '%s'.", class(x)))
}

### get MC weights. In case it is not implemented it returns 1 for each MC
get_weights <- function(x, scale = NULL) UseMethod("get_weights")
get_weights.DSC <- function(x, scale=NULL) {
	m <- rep(1,nclusters(x))
	if(!is.null(scale)) m <- map(m, scale)
	m
}

### make a deep copy of the 
get_copy <- function(x) UseMethod("get_copy")
get_copy.default <- function(x, ...) {
   stop(gettextf("get_copy not implemented for class '%s'.", class(x)))
}

get_microclusters <- function(x) UseMethod("get_microclusters")
get_microclusters.DSC <- function(x) {
	stop(gettextf("get_microclusters not implemented for class '%s'.", class(x)))
}

get_macroclusters <- function(x) UseMethod("get_macroclusters")
get_macroclusters.DSC <- function(x) {
	stop(gettextf("get_macroclusters not implemented for class '%s'.", class(x)))
}

recluster <- function(macro, dsc, ...) UseMethod("recluster")
recluster.DSC <- function(macro, dsc, ...) {
	stop(gettextf("recluster not implemented for class '%s'.", class(x)))
}

### End of interface
#####################################################################3
### derived functions, plot and print
nclusters <- function(x) UseMethod("nclusters")
nclusters.DSC <- function(x) {
	options(warn=-1)
	nclusters <- nrow(get_centers(x))
	options(warn=1)
	
	nclusters
}

get_assignment <- function(dsc,points) UseMethod("get_assignment")
get_assignment.DSC <- function(dsc,points) {
	d <- points
	c <- get_centers(dsc)
	if(length(c)>0) {
		dist <- dist(d,c)
		#Find the minimum distance and save the class
		predict <- apply(dist, 1, which.min)+1
		predict[is.null(predict)] <- 1
		predict[is.na(predict)] <- 1
	} else {
		warning(paste(class(dsc)[1],": There are no clusters",sep=""))
		predict <- rep(1,nrow(d))
	}
	predict	
}

print.DSC <- function(x, ...) {
    cat(paste(class(x)[1], "-", x$description, '\n'))
    cat(paste('Number of clusters:', nclusters(x), '\n'))
}


#plot.DSC will call super question.
plot.DSC <- function(x, dsd = NULL, n = 1000, 
	col_points="gray",  
	col_macro="red", 
	col_micro="black",
	weights=FALSE,
	scale=c(1,10),
	..., 
	method="pairs", microclusters=FALSE) {
    
    ## method can be pairs, plot or pc (projection with PCA)
    centers <- get_centers(x)
    
    if(!is.null(dsd)) {
	d <- get_points(dsd, n, assignment = TRUE)
	names(d) <- names(centers)

	if(ncol(centers)>2 && method=="pairs") {
	    pairs(rbind(d,centers),
		    col=c(rep(col_points,n),rep(col_macro,nrow(centers))), 
		    ...)
	}
	else if(ncol(centers)>2 && method=="pc") {
	    ## we assume Euclidean here
	    p <- prcomp(rbind(d,centers))
	    if(weights)
	    	plot(p$x,
		    col=c(rep(col_points,n),rep(col_macro,nrow(centers))),
		    cex=c(rep(1,n),get_weights(x,scale)),
		    ...)
	    else
		plot(p$x,
		    col=c(rep(col_points,n),rep(col_macro,nrow(centers))),
		    ...)
	} else {
	    if(weights)
		plot(rbind(d,centers),
		    col=c(rep(col_points,n),rep(col_macro,nrow(centers))),
		    cex=c(rep(1,n),get_weights(x,scale)),
		    ...)
	    else
		plot(rbind(d,centers),
		    col=c(rep(col_points,n),rep(col_macro,nrow(centers))),
		    ...)
	}
    } else {
	if(ncol(centers)>2 && method=="pairs") {
	    if(weights)
	   		pairs(centers,col=col_macro,cex=get_weights(x,scale), ...)
	   	else
	   		pairs(centers,col=col_macro, ...)
	}
	else if(ncol(centers)>2 && method=="pc") {
	    ## we assume Euclidean here
	    p <- prcomp(centers)
	    if(weights)
	    	plot(p$x,col=col_macro,cex=get_weights(x,scale),...)
	    else
	    	plot(p$x,col=col_macro,...)
	} else {
	    if(weights)
	    	plot(centers,col=col_macro,cex=get_weights(x,scale),...)
	    else
	    	plot(centers,col=col_macro,...)
	}
    }

    if(!is.null(x) && microclusters && length(get_microclusters(x))>0) {
	if(class(x)[1] == "DSC_tNN") {
	    library(sfsmisc)
	    p <- get_microclusters(x)
	    for(i in 1:nrow(p)){
		lines(ellipsePoints(x$RObj$r, x$RObj$r, 
				loc=as.numeric(p[i,]), n=90),
			col = col_micro, lty=3)
	    }
	    
	    edgelist <- get_edgelist(x)
	    for(i in (1:(length(edgelist)/2))*2-1){
		lines(rbind(p[edgelist[i],],p[edgelist[i+1],]),
			col=col_micro)}
	}
	
	points(get_microclusters(x), col=col_micro)
	
	points(centers,col=col_macro)
    }
}

