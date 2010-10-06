
## constructor
clusterer <- function(algorithm="denstream") {

	new("clusterer", algorithm=algorithm)	
}

## show
#setMethod("show", signature(object = "clusterer"),
#        function(object) {
#            cat("Clusterer\n", 
#                    "Algorithm:", object@algorithm, "\n",
#                    "Stream:", object@stream, "\n"
#                    )
#            invisible(NULL)
#        }
#)

cluster <- function() {
	.jpackage("ClusterDS", jars="*")
	c = .jnew("moa/clusterers/denstream/DenStream")

}

#setMethod("cluster", signature(),
#	function() {
 #          .jinit()
#	}
#)
