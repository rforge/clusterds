get_points.DSD_MOA <- function(x, n=1, assignment=FALSE, ...) {
	
	if (n < 1)
		stop("n must be > 0")
	
	# pre-allocating the space for the matrix
	data <- matrix(NA, nrow=n, ncol=x$d)
	
	if(assignment) a <- numeric()

	# unpackaging the java instances
	for (i in 1:n) {
		instance <- .jcall(x$javaObj, "Lweka/core/Instance;", "nextInstance")
		row <- .jcall(instance, "[D", "toDoubleArray")
		class <- .jcall(instance, "D", "classValue")
		data[i,] <- row[1:x$d]
		
		if(assignment) {
			 a[i] <- class
		}
	}
	
	data <- data.frame(data)
	
	if(assignment) {
		a[which(a==-1)] <- NA
		attr(data,"assignment") <- a + 1
	}
	
	data
}
