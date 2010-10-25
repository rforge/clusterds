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

cluster <- function(clusterer, options="") {
   if (clusterer@algorithm == "denstream") {
      # denstream options:
      # -e epsilon 	0.01 (defines the epsilon neighborhood, range: 0 to 1)
      # -p minPoints 	10 (min. num. points a cluster must have)
      # -l lambda	0.006 (range: 0 to 1)
      # -b beta		0.001 (range: 0 to 1)
      # -m mu		1 (range: 0 to max(double))
      # -i initPoints	10000 (number of points to use for initialization)
      # -M		false (evaluate micro clustering flag)

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

   # if the user passes options to the function, the cluster's parameters can be set
   if (options != "") {
      o <- .jcall(c, "Lmoa/options/Options;", "getOptions")
      .jcall(o, "V", "setViaCLIString", options)
      test <- .jcall(o, "Ljava/lang/String;", "getAsCLIString")
      print(test)
   }
   else {
      print("using default options")
   }

   panel <- .jnew("moa/gui/clustertab/ClusteringAlgoPanel")
   strm <- .jcall(panel, "Lmoa/streams/clustering/ClusteringStream;", "getStream")
   .jcall(strm, "V", "prepareForUse")
   
   #looping through the stream, feeding the new datapoints into 
   #the algorithm
   for (i in 1:1000) {
      inst <- .jnew("weka/core/Instance")
      inst <- .jcall(strm, "Lweka/core/Instance;", "nextInstance")
      .jcall(c, "V", "trainOnInstanceImpl", inst)
   }
	

}
