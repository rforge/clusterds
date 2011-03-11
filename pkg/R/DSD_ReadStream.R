# accepts an open connection
DSD_ReadStream <- function(x, delimiter=",") {

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
	          delimiter = delimiter)
    class(l) <- c("DSD","DSD_ReadStream")
    l
}

getPoints.DSD_ReadStream <- function(x, n=1, loop=FALSE, ...) {

    # reading from the connection
    #TODO deal with quotation marks or other weird stuff
	
	# TODO: deal with the new parameter, loop and EOF
    lines <- strsplit(x=readLines(x$con, n=n, ok=TRUE), split=x$delimiter)
    matrix(as.numeric(unlist(lines)),ncol=length(lines[[1]]), byrow=TRUE)
}
