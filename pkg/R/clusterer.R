
## constructor
clusterer <- function(algorithm="denstream") {

	new("clusterer", algorithm=algorithm)	
}

## show
setMethod("show", signature(object = "clusterer"),
        function(object) {
            cat("Clusterer\n", 
                    "Algorithm:", object@algorithm, "\n"
                    )
            invisible(NULL)
        }
)

cluster <- function(clusterer) {
   if (clusterer@algorithm == "denstream") {
      c <- .jnew("moa/clusterers/denstream/DenStream")
      .jcall(c, "V", "prepareForUse")
   }

   else if (clusterer@algorithm == "clustream") {
      c <- .jnew("moa/clusterers/clustream/Clustream")
      .jcall(c, "V", "prepareForUse")
   }

   else if (clusterer@algorithm == "cobweb") {
      c <- .jnew("moa/clusterers/CobWeb")
      .jcall(c, "V", "prepareForUse")
   }

   else {
      return;
   }

   panel <- .jnew("moa/gui/clustertab/ClusteringAlgoPanel")
   strm <- .jcall(panel, "Lmoa/streams/clustering/ClusteringStream;", "getStream")
   .jcall(strm, "V", "prepareForUse")

   #RandomRBFGeneratorEvents extends abstract class ClusteringStream
   #strm <- .jnew("moa/streams/clustering/RandomRBFGeneratorEvents")
   #.jcall(strm, "V", "prepareForUse")
   
   #looping through the stream, feeding the new datapoints into 
   #the algorithm
   for (i in 1:1000) {

      #TODO: this call isn't working, but when calling
      # .jmethods(strm), the method signature does indeed exist
      inst <- .jnew("weka/core/Instance")
      inst <- .jcall(strm, "Lweka/core/Instance;", "nextInstance")
      .jcall(c, "V", "trainOnInstanceImpl", inst)
   }
	

}

#setMethod("cluster", signature(),
#	function() {
 #          .jinit()
#	}
#)
