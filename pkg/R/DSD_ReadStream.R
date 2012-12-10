# accepts an open connection
DSD_ReadStream <- function(x, sep=",", 
	take=NULL, class=NULL, 
	center=FALSE, scale=FALSE,
	loop=FALSE) {

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
	    take = take,
	    class = class,
	    center = center,
	    scale = scale,
	    loop = loop)
    class(l) <- c("DSD_ReadStream","DSD_R","DSD")
    l
}

## it is important that the connection is OPEN
get_points.DSD_ReadStream <- function(x, n=1, assignment=FALSE, ...) {
	
	togo <- n

	# comment.char="" is for performance reasons
	tryCatch({
		d <- suppressWarnings(read.table(file=x$con, 
				sep=x$sep, nrows=n, comment.char="", ...))
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
	

	if(!is.null(x$class)) cl <- d[,x$class[1]]
	if(!is.null(x$take)) d <- d[,x$take, drop=FALSE]


	# scale
	d <- scale(d, center= x$center, scale=x$scale)
	
	# if enough data was read, return like normal
	d <- data.frame(d)
	attr(d, "assignment") <- cl

	d
}

reset_stream.DSD_ReadStream <- function(dsd) {
    invisible(seek(dsd$con, where=0))
}

close_stream <- function(dsd) {
    if(!is(dsd, "DSD_ReadStream")) 
	stop("'dsd' is not of class 'DSD_ReadStream'")
    close(dsd$con)
}


