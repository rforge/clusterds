cluster <- function(algorithm="denstream", options="") {
   if (algorithm == "denstream") {
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

   else if (algorithm == "clustream") {
      # clustream options:
      # -t timeWindow (default: 1000)
      # Range of the window.
      # -k maxNumKernels (default: 100)
      # Maximum number of micro kernels to use.
      # -M evaluateMicroClustering
      # Evaluate the underlying microclustering instead of the macro clustering

      c <- .jnew("moa/clusterers/clustream/Clustream")
      .jcall(c, "V", "prepareForUse")
   }

   else if (algorithm == "cobweb") {
      # cobweb options:
      # -a acuity (default: 1.0)
      # Acuity (minimum standard deviation)
      # -c cutoff (default: 0.002)
      # Cutoff (minimum category utility)
      # -r randomSeed (default: 1)
      # Seed for random noise.

      c <- .jnew("moa/clusterers/CobWeb")
      .jcall(c, "V", "prepareForUse")
   }

   else {
      stop("invalid clusterer.")
   }

   # if the user passes options to the function, the cluster's parameters can be set
   if (options != "") {
      o <- .jcall(c, "Lmoa/options/Options;", "getOptions")
      .jcall(o, "V", "setViaCLIString", options)
      test <- .jcall(o, "Ljava/lang/String;", "getAsCLIString")
      print(test)
   }
   else {
      print("no CLI options defined, using default parameters for clusterer")
   }

   panel <- .jnew("moa/gui/clustertab/ClusteringAlgoPanel")
   strm <- .jcall(panel, "Lmoa/streams/clustering/ClusteringStream;", "getStream")
   .jcall(strm, "V", "prepareForUse")
   
   #TODO: be able to expand the number of instances we loop through, and give
   #      the user the ability to pause or resume the datastream

   #looping through the stream, feeding the new datapoints into 
   #the algorithm
   for (i in 1:1000) {
      inst <- .jnew("weka/core/Instance")
      inst <- .jcall(strm, "Lweka/core/Instance;", "nextInstance")
      .jcall(c, "V", "trainOnInstanceImpl", inst)
   }
	

}
