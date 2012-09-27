# DSDData - DataStreamData interface
# all DSD classes have these functions
# and an additional function to create the DSD

get_points.default <- function(x, n=1, ...) {
   stop(gettextf("get_points not implemented for class '%s'.", class(x)))
}

get_points <- function(x, n=1, ...) UseMethod("get_points")

print.DSD <- function(x, ...) {
    cat(paste('DSD - Data Stream Datasource:', x$description, '\n'))
	cat(paste('With', x$k, 'clusters', 'in', x$d, 'dimensions', '\n'))
}

reset_stream.DSD <- function(dsd) {
    stop(gettextf("reset_stream not implemented for class '%s'.", class(dsd)))
}

reset_stream <- function(dsd) UseMethod("reset_stream")

plot.DSD <- function(dsd = NULL, n = 1000, ..., method="pairs") {
    ## method can be pairs, plot or pc (projection with PCA)
    d <- get_points(dsd, n, assignment = TRUE)
    
	names(d)
    if(ncol(d)>2 && method=="pairs") {
   		pairs(d, ...)
    }
    else if(ncol(d)>2 && method=="pc") {
		## we assume Euclidean here
		p <- prcomp(d)
		plot(p$x,...)
    } else {
		plot(d,col=attr(d,"assignment"),...)
    }
}
