write_stream.default <- function(dsd, con, n=100, sep=",", 
	col.names=FALSE, row.names=FALSE, ...) {
    stop(gettextf("write_stream not implemented for class '%s'.", class(dsd)))
}

write_stream <- function(dsd, con, n=100, sep=",",
	col.names=FALSE, row.names=FALSE, ...) UseMethod("write_stream")

write_stream.DSD <- function(dsd, con, n=100, sep=",",
	col.names=FALSE, row.names=FALSE, ...) {	
    
    # string w/ file name (clears the file)
    if (is(con, "character")) con <- file(con, open="w")

    # error	
    else if (!is(con, "connection")) stop("Please pass a valid connection!")

    # needs opening
    else if (!isOpen(con)) open(con)

    # all following calls have to have col.names=FALSE regardless
    for (i in 1:n) write.table(get_points(dsd, 1), 
	    con, sep=sep, append=TRUE, col.names=FALSE,
		row.names=row.names, ...)

    close(con)
}

