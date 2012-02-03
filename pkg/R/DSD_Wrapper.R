DSD_Wrapper <- function(x, k, loop=FALSE) {

    state <- new.env()
    assign("counter", 1L, envir = state)

    if (missing(k))
	k <- NA

    # creating the DSD object
    l <- list(description = "Data Frame/Matrix Wrapper Stream",
	    strm = x,
	    state = state,
	    d = ncol(x),
	    k = k,
	    loop = loop)
    class(l) <- c("DSD_Wrapper","DSD_R","DSD")
    l
}

get_points.DSD_Wrapper <- function(x, n=1, ...) {
    n <- as.integer(n)
   
    if(x$state$counter > nrow(x$strm)) {
	if(x$loop) x$state$counter <- 1L
	else stop("The stream is at its end!")
    }

    n_left <- nrow(x$strm) - x$state$counter + 1L
    
    if(n_left < n && !x$loop) stop("Not enought data points left in stream!")

    if(n_left >= n) {
	### regular case
	d <- x$str[x$state$counter:(x$state$counter + n -1L),]	
	x$state$counter <- x$state$counter + n
    }else{
	### we need to loop!


	# take what is left and reset counter
	d <- x$strm[x$state$counter:nrow(x$strm),] 
	togo <- n-n_left
	x$state$counter <- 1L

	while(togo > 0L) {
	    n_left <- nrow(x$strm) - x$state$counter + 1L

	    if(n_left < togo) {
		# take the whole stream
		d <- rbind(d, x$strm)
		togo <- togo - n_left
	    }else{
		# take the rest
		d <- rbind(d, x$strm[1:(x$state$counter+togo-1),])
		x$state$counter <- x$state$counter + togo
		togo <- 0L
	    }
	}
    }

    d
}

print.DSD_Wrapper <- function(x, ...) {
    NextMethod() # calling the super classes print()
    pos <- x$state$counter
    if (pos>nrow(x$strm)) 
	if (!x$loop) pos <- "'end'" else pos <- 1
    cat(paste('Contains', nrow(x$strm), 
		    'data points, currently at position', pos, 
		    'loop is', x$loop, '\n'))
}
