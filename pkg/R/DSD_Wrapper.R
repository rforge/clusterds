DSD_Wrapper <- function(x, k, loop=FALSE, assignment = NULL) {

    state <- new.env()
    assign("counter", 1L, envir = state)

    if (missing(k))
	k <- NA

    if(is.null(assignment)) assignment <-attr(x, "assignment")

    # creating the DSD object
    l <- list(description = "Data Frame/Matrix Stream Wrapper",
	    strm = x,
	    state = state,
	    d = ncol(x),
	    k = k,
	    loop = loop,
	    assignment = assignment)
    class(l) <- c("DSD_Wrapper","DSD_R","DSD")
    l
}

get_points.DSD_Wrapper <- function(x, n=1, assignment = FALSE,...) {
    n <- as.integer(n)
   
    if(x$state$counter > nrow(x$strm)) {
	if(x$loop) x$state$counter <- 1L
	else stop("The stream is at its end!")
    }

    n_left <- nrow(x$strm) - x$state$counter + 1L
    
    if(n_left < n && !x$loop) stop("Not enought data points left in stream!")

    if(n_left >= n) {
	### regular case
	d <- x$strm[x$state$counter:(x$state$counter + n -1L),]
	if(assignment) {
	    a <- x$assignment[x$state$counter:(x$state$counter + n -1L)]
	}
	x$state$counter <- x$state$counter + n
    }else{
	### we need to loop!


	# take what is left and reset counter
	d <- x$strm[x$state$counter:nrow(x$strm),] 
	if(assignment) a <- x$assignment[x$state$counter:nrow(x$strm)]
	togo <- n-n_left
	x$state$counter <- 1L

	while(togo > 0L) {
	    n_left <- nrow(x$strm) - x$state$counter + 1L

	    if(n_left < togo) {
		# take the whole stream
		d <- rbind(d, x$strm)
		if(assignment) a <- append(a, x$assignment)
		togo <- togo - n_left
	    }else{
		# take the rest
		d <- rbind(d, x$strm[1:(x$state$counter+togo-1),])
		if(assignment) {
		    a <- append(a, x$assignment[1:(x$state$counter+togo-1)])
		}
		x$state$counter <- x$state$counter + togo
		togo <- 0L
	    }
	}
    }
    
	data.frame(d)
	if(assignment) attr(d,"assignment") <- a
	
	d
}

print.DSD_Wrapper <- function(x, ...) {
    NextMethod() # calling the super classes print()
    pos <- x$state$counter
    if (pos>nrow(x$strm)) 
	if (!x$loop) pos <- "'end'" else pos <- 1
    cat(paste('Contains', nrow(x$strm), 
		    'data points - currently at position', pos, 
		    '- loop is', x$loop, '\n'))
}

reset_stream.DSD_Wrapper <- function(dsd) {
    dsd$state$counter <- 1
}
