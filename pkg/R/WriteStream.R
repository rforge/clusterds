WriteStream <- function(dsd, con, n=100, delim=',') {	
	# string w/ file name
	if (is(con, "character")) {
		con <- file(con)
		open(con)
	}
	
	# error
	else if (!is(con, "connection")) {
		stop("please pass a valid connection")
	}
	
	# needs opening
	else if (!isOpen(con)) {
		open(con)
	}
	
	# getting a matrix of data
	d <- getPoints(dsd, n)
	write.table(d, con, row.names=FALSE, col.names=FALSE, sep=delim)
}