get_points.DSD_MOA <- function(x, n=1, ...) {
	
	if (n < 1)
		stop("n must be > 0")
	
	# pre-allocating the space for the matrix
	data <- matrix(NA, nrow=n, ncol=x$d)

	# unpackaging the java instances
	for (i in 1:n) {
		row <- .jcall(x$javaObj, "Lweka/core/Instance;", "nextInstance")
		row <- .jcall(row, "[D", "toDoubleArray")
		data[i,] <- row[1:x$d]
	}
	
	data.frame(data)
}
