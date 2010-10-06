#setMethod("build", signature(clusterer = "clusterer"),
#	function(clusterer) {
#
#	.jinit()
#	
#	}
#)

helloworld <- function() {

	.jinit()
	s <- .jnew("java/lang/String", "Hello World")
	.jstrVal(s)

	}

