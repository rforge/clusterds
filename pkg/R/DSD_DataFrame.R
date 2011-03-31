DSD_DataFrame <- function(df, k) {

	state <- new.env()
	assign("counter", 1, envir = state)
	
	if (missing(k))
		k <- "unknown"

    # creating the DSD object
    l <- list(description = "Data Frame/Matrix Wrapper Stream",
			  strm = df,
			  state = state,
			  d = ncol(df),
			  k = k)
    class(l) <- c("DSD","DSD_R","DSD_DataFrame")
    l
}

get_points.DSD_DataFrame <- function(x, n=1, loop=FALSE, ...) {
	
	# this means we will go off the edge, so we need to loop
	if (loop && (x$state$counter + n > nrow(x$strm))) {
	remaining <- nrow(x$strm) - x$state$counter
	
	if (remaining > 0) {
	d <- x$strm[x$state$counter:nrow(x$strm),] 
	}
	
	n <- n - remaining - 1
	
		if (n > 0) {
		d <- rbind(d, x$strm[1:n,])
		x$state$counter <- n
		}
		
		else {
		x$state$counter <- 1
		}
	
	}
	
	# we will go of the edge, but no loop, so return as much as possible
	# and warn the user
	else if (x$state$counter + n > nrow(x$strm)) {
	n <- nrow(x$strm) - x$state$counter
	
		if (n > 0) {
		d <- x$strm[x$state$counter:nrow(x$strm),]
		x$state$counter <- nrow(x$strm)
		}
		
		else {
		stop("no more data points in the stream")
		}
		
	print("reached the end of the stream, returned as much as possible")
	}
	
	# otherwise we return like normal
	else {
	d <- x$strm[x$state$counter:(x$state$counter+n-1),]
	x$state$counter <- x$state$counter + n
	}
	
	d
}

reset.default <- function(x) {
   stop(gettextf("reset not implemented for class '%s'.", class(x)))
}

reset <- function(x) UseMethod("reset")

reset.DSD_DataFrame <- function(x) {
	x$state$counter <- 0
}