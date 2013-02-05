### write data from a stream to a file

write_stream <- function(dsd, file, n=100, block=100000L,
	assignment=FALSE, sep=",", 
	col.names=FALSE, row.names=FALSE, ...) UseMethod("write_stream")

write_stream.default <- function(dsd, file, n=100, block=100000L, 
	assignment=FALSE, sep=",", col.names=FALSE, row.names=FALSE, ...) {
    stop(gettextf("write_stream not implemented for class '%s'.", class(dsd)))
}

write_stream.DSD <- function(dsd, file, n=100, block=100000L, assignment=FALSE, 
	sep=",", col.names=FALSE, row.names=FALSE, ...) {	

    # string w/ file name (clears the file)
    if (is(file, "character")) file <- file(file, open="w")

    # error	
    else if (!is(file, "connection")) stop("Please pass a valid connection!")

    # needs opening
    else if (!isOpen(file)) open(file)

    # all following calls have to have col.names=FALSE regardless
    for (bl in .make_block(n, block)) {
	p <- get_points(dsd, bl, assignment=assignment)
	if(assignment) p <- cbind(p, attr(p, "assignment"))
	write.table(p, 
		file, sep=sep, append=TRUE, col.names=FALSE,
		row.names=row.names, ...)
    }
    close(file)
}

