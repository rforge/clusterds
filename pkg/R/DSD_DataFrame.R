DSD_DataFrame <- function(df, k, loop=FALSE) {
    df <- as.data.frame(df)

    state <- new.env()
    assign("counter", 1, envir = state)

    if (missing(k))
	k <- NA

    # creating the DSD object
    l <- list(description = "Data Frame/Matrix Wrapper Stream",
	    strm = df,
	    state = state,
	    d = ncol(df),
	    k = k,
		loop = loop)
    class(l) <- c("DSD_DataFrame","DSD_R","DSD")
    l
}

get_points.DSD_DataFrame <- function(x, n=1, ...) {

    # this means we will go off the edge, so we need to loop
    if (x$loop && (x$state$counter + n > nrow(x$strm))) {
	
		# prep before loop
		togo <- n
		in_stream <- nrow(x$strm) - x$state$counter + 1
		if (in_stream > 0) {
			d <- x$strm[x$state$counter:nrow(x$strm),] 
		}
		
		# subtracting the amount that was left in the stream
		togo <- togo - in_stream
		
		# resetting the counter
		x$state$counter <- 1
		
		while (togo > 0) {
			in_stream <- nrow(x$strm) - x$state$counter + 1
			
			# if there are some left in the stream, left than the total amount needed
			if (in_stream > 0 && in_stream < togo) {
				d <- rbind(d, x$strm[x$state$counter:nrow(x$strm),] )
				togo <- togo - in_stream
			}
			
			# if there are some left in the stream, more than the total amount needed
			else if (in_stream > 0) {
				d <- rbind(d, x$strm[x$state$counter:(x$state$counter+togo-1),])
				x$state$counter <- x$state$counter + togo
				togo <- 0
			}
			
			# there are none left in the stream
			else {
				x$state$counter <- 1
			}
		}
    }

    # we will go of the edge, but no loop, so return as much as possible and warn the user
    else if (x$state$counter + n - 1 > nrow(x$strm)) {
		remaining <- nrow(x$strm) - x$state$counter

		if (remaining >= 0) {
			d <- x$strm[x$state$counter:nrow(x$strm),]
			x$state$counter <- nrow(x$strm)
		}

		else {
			stop("no more data points in the stream")
		}

		if (nrow(d) != n) {
			warning("reached the end of the stream, returned as much as possible")
		}
    }

    # otherwise we return like normal
    else {
		d <- x$strm[x$state$counter:(x$state$counter+n-1),]
		x$state$counter <- x$state$counter + n
    }

    d
}

print.DSD_DataFrame <- function(x, ...) {
	NextMethod() # calling the super classes print()
	cat(paste('contains', nrow(x$strm), 'data points (currently at position', x$state$counter, 'loop is', x$loop, '\n'))
}