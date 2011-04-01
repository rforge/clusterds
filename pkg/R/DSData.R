# DSDData - DataStreamData interface
# all DSD classes have these functions
# and an additional function to create the DSD

get_points.default <- function(x, n=1, ...) {
   stop(gettextf("get_points not implemented for class '%s'.", class(x)))
}

get_points <- function(x, n=1, ...) UseMethod("get_points")

write_stream.default <- function(dsd, con, n=100, sep=",", 
	col.names=FALSE, row.names=FALSE, ...) {
   stop(gettextf("write_stream not implemented for class '%s'.", class(x)))
}

write_stream <- function(dsd, con, n=100, sep=",", 
	col.names=FALSE, row.names=FALSE, ...) UseMethod("write_stream")

write_stream.DSD <- function(dsd, con, n=100, sep=",", 
	col.names=FALSE, row.names=FALSE, ...) {	
	# string w/ file name
	if (is(con, "character")) {
	con <- file(con, open="w")
	}
	
	# error
	else if (!is(con, "connection")) {
	stop("please pass a valid connection")
	}
	
	# needs opening
	else if (!isOpen(con)) {
	open(con)
	}
	
	# clearing the file before appending data
	d <- get_points(dsd, 1)
	write.table(d, con, sep=sep, col.names=col.names, row.names=row.names, ...)
	
	# all following calls have to have col.names=FALSE regardless
	if (n > 1) {
		for (i in 2:n) {
			d <- get_points(dsd, 1)
			write.table(d, con, sep=sep, append=TRUE, col.names=FALSE, row.names=row.names, ...)
		}
	}
	
	close(con)
}

print.DSD <- function(x, ...) {
    cat(paste('DSD - Data Stream Datasource:', x$description, '\n'))
	cat(paste('Number of clusters:', x$k, '\n'))
	cat(paste('Number of dimensions:', x$d, '\n'))
    
	### FIXME: print other stats
}

