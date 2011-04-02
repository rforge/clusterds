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
			  con = x,
	          sep = sep,
			  loop = loop)
    class(l) <- c("DSD","DSD_R","DSD_ReadStream")
    l
}

## it is important that the connection is OPEN
get_points.DSD_ReadStream <- function(x, n=1, ...) {
	
	# suppressing the warnings from read.table
	# TODO: have to think of a better way to deal with the warnings
	options(warn=-1)
	
	#suppressWarnings()
	
	# comment.char="" is for performance reasons
	tryCatch({
		d <- read.table(file=x$con, sep=x$sep, nrows=n, comment.char="", ...)
	}, error = function(ex) {
		d <- 0 # no lines were read
	})
	togo <- n - nrow(d)
	
	# we need to loop
	while (x$loop && togo > 0) {
		seek(x$con, where=0) # resetting the connection
		
		prev <- nrow(d)
		d <- rbind(d, read.table(file=x$con, sep=x$sep, nrows=togo, comment.char="", ...))
		togo <- togo - (nrow(d)-prev)
	}
	
	# restoring the warnings
	options(warn=0)
	
	# looping disabled, warn the user
	if (!x$loop && (d == 0 || nrow(d) < n)) {
		warning("reached the end of the stream, returned as much as possible")
	}
	
	# if enough data was read, return like normal
	d
}
