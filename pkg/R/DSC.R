### DSC - Data Stream Clusterer interface

### all DSC classes have these interface methods

get_centers <- function(x, type = c("auto", "micro", "macro"), ...) 
    UseMethod("get_centers")
get_centers.default <- function(x, type = c("auto", "micro", "macro"), ...) {
    stop(gettextf("get_centers not implemented for class '%s'.",
		                        paste(class(x), collapse=", ")))
}

### get MC weights. In case it is not implemented it returns 1s
get_weights <- function(x, type=c("auto", "micro", "macro"), scale=NULL, ...) 
    UseMethod("get_weights")
get_weights.default <- function(x, type=c("auto", "micro", "macro"), 
	scale=NULL, ...) {
    m <- rep(1,nclusters(x, type=type))
    if(!is.null(scale)) m <- map(m, range=scale)
    m
}

### End of interface
#####################################################################3

### make a deep copy of the 
get_copy <- function(x) UseMethod("get_copy")
get_copy.default <- function(x, ...) {
    stop(gettextf("get_copy not implemented for class '%s'.",
		    paste(class(x), collapse=", ")))
}

get_microclusters <- function(x) UseMethod("get_microclusters")
get_microclusters.DSC <- function(x) {
    stop(gettextf("No micro-clusters available for class '%s'.",
		    paste(class(x), collapse=", ")))
}

get_macroclusters <- function(x) UseMethod("get_macroclusters")
get_macroclusters.DSC <- function(x) {
    stop(gettextf("No macro-clusters available for class '%s'.",
		    paste(class(x), collapse=", ")))
}

get_microweights <- function(x) UseMethod("get_microweights")
get_microweights.DSC <- function(x) {
    stop(gettextf("No weights for micro-clusters available for class '%s'.",
		    paste(class(x), collapse=", ")))
}

get_macroweights <- function(x) UseMethod("get_macroweights")
get_macroweights.DSC <- function(x) {
    stop(gettextf("No weights for macro-clusters available for class '%s'.",
		    paste(class(x), collapse=", ")))
}


### derived functions, plot and print
nclusters <- function(x, type=c("auto", "micro", "macro"), ...) 
    UseMethod("nclusters")
nclusters.DSC <- function(x, type=c("auto", "micro", "macro"), ...) {
    nrow(get_centers(x, type=type))
}

get_assignment <- function(dsc, points, type=c("auto", "micro", "macro"), ...) 
    UseMethod("get_assignment")
get_assignment.DSC <- function(dsc, points, type=c("auto", "micro", "macro"), 
	...) {
    d <- points
    
    c <- get_centers(dsc, type=type)
    
    if(nrow(c)>0) {
	dist <- dist(d,c)
	#Find the minimum distance and save the class
	#predict <- apply(dist, 1, which.min)+1
	predict <- apply(dist, 1, which.min)
	predict[is.null(predict)] <- 1L
	predict[is.na(predict)] <- 1L
    } else {
	warning("There are no clusters!")
	predict <- rep(1L, nrow(d))
    }
    predict	
}

print.DSC <- function(x, ...) {
    cat(paste(paste(class(x), collapse=", "), "-", x$description, '\n'))
    cat(paste('Number of clusters:', nclusters(x), '\n'))
}


#plot.DSC will call super question.
plot.DSC <- function(x, dsd = NULL, n = 1000, 
	col_points="gray",  
	col_clusters="red", 
	weights=TRUE,
	scale=c(1,5),
	cex =1,
	pch=NULL,
	..., 
	method="pairs", 
	type=c("auto", "micro", "macro")) {

    ## method can be pairs, plot or pc (projection with PCA)
    k <- nclusters(x, type=type)
    centers <- get_centers(x, type=type)
    if(weights) cex_clusters <- get_weights(x, type=type, scale=scale)
    else cex_clusters <- rep(cex, k)
	col <- rep(col_clusters, k)
    mpch <- rep(1, k)

    ### prepend data if given
    if(!is.null(dsd)) {
	d <- get_points(dsd, n, assignment = TRUE)
	names(d) <- names(centers)

	centers <- rbind(d, centers)
	col <- c(rep(col_points,n), col)
	cex_clusters <- c(rep(cex, n), cex_clusters)
	mpch <- c(attr(d, "assignment"), mpch)
    }

    if(!is.null(pch)) mpch <- pch

    ### plot
    if(ncol(centers)>2 && method=="pairs") {
	    pairs(centers, col=col, cex=cex_clusters, pch=mpch, ...)
    }
    else if(ncol(centers)>2 && method=="pc") {
	## we assume Euclidean here
	p <- prcomp(centers)
	    plot(p$x, col=col, cex=cex_clusters, pch=mpch, ...)
    } else { ## plot first 2 dimensions
	    plot(centers, col=col, cex=cex_clusters, pch=mpch, ...)
    }


### add lines for tNN this only works for plot!!!
    if(all(c("DSC_tNN", "DSC_Macro") %in% class(x))
		&& type %in% c("macro", "auto" )
		&& (ncol(centers)<=2 || method=="plot")) {
	p <- get_microclusters(x)
	if(length(p)>0) {
	    
	    library(sfsmisc)
	    
	    for(i in 1:nrow(p)){
		lines(ellipsePoints(x$RObj$r, x$RObj$r, 
				loc=as.numeric(p[i,]), n=90),
			col = "black", lty=3)
	    }

	    edgelist <- get_edgelist(x)
	    for(i in (1:(length(edgelist)/2))*2-1){
		lines(rbind(p[edgelist[i],],p[edgelist[i+1],]),
			col="black")}
	    }

	    #points(p, col=col_clusters)
	    #points(centers,col=col_clusters)
	}
    }

