# accepts an open connection
DSD_ReadStream <- function(x, sep=",", loop=FALSE) {

    # if the user passes a string, create a new connection and open it
    if (is(x,"character")) {
	x <- file(x)
	open(x)
	}
	
	# error out if no string or connection is passed
    else if (!is(x,"connection")) {
	stop("please pass a valid connection")
	}
	
	# open the connection if its closed
    else if (!isOpen(x)) {
	open(x)
    }

    # creating the DSD object
    l <- list(description = "File Data Stream",
	    d = NA,
	    k = NA,
	    con = x,
	    sep = sep,
	    loop = loop)
    class(l) <- c("DSD_ReadStream","DSD_R","DSD")
    l
}

## it is important that the connection is OPEN
get_points.DSD_ReadStream <- function(x, n=1, ...) {
	
	togo <- n
	
	# comment.char="" is for performance reasons
	tryCatch({
		d <- suppressWarnings(read.table(file=x$con, sep=x$sep, nrows=n, comment.char="", ...))
		togo <- n - nrow(d)
	}, error = function(ex) {
	})
	
	# this means no lines were read, we need to do a prep-read before looping
	if (x$loop && togo == n) {
		seek(x$con, where=0) # resetting the connection
		d <- suppressWarnings(read.table(file=x$con, sep=x$sep, nrows=n, comment.char="", ...))
		togo <- n - nrow(d)
	}
	
	# we need to loop
	while (x$loop && togo > 0) {
		seek(x$con, where=0) # resetting the connection
		
		prev <- nrow(d)	
		d <- suppressWarnings(rbind(d, read.table(file=x$con, sep=x$sep, nrows=togo, comment.char="", ...)))
		togo <- togo - (nrow(d)-prev)
	}
	
	# looping disabled, warn the user
	if (!x$loop && togo == n) {
	stop("looping disabled and the stream is empty")
	}
	
	else if (!x$loop && togo > 0) {
	warning("reached the end of the stream, returned as much as possible")
	}
	
	# if enough data was read, return like normal
	return(data.frame(d))
}

reset_stream.DSD_ReadStream <- function(dsd) {
	seek(dsd$con, where=0)
}
