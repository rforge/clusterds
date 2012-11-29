# clustream options:
# IntOption("horizon", 'h', "Rang of the window.", 1000)
# IntOption("maxNumKernels", 'k', "Maximum number of micro kernels to use.", 100);
# IntOption("kernelRadiFactor", 't', "Multiplier for the kernel radius", 2);

# CluStream uses a modified k-means to recluster!
# Modifications:
#
#At the initialization stage, the seeds are no longer
#picked randomly, but are sampled with probability
#proportional to the number of points in a given micro-
#cluster. The corresponding seed is the centroid of that
#micro-cluster.
#
#At the partitioning stage, the distance of a seed
#from a given pseudo-point (or micro-cluster) is equal
#to the distance of the seed from the centroid of the
#corresponding micro-cluster.
#
#At the seed adjustment stage, the new seed for a
#given partition is defined as the weighted centroid of
#the micro-clusters in that partition.
#It is important to note that a give
#

DSC_CluStream <- function(
	horizon=1000, 
	maxNumKernels=100,
	kernelRadiFactor=2
	) {
  
  if (horizon < 0)
    stop("invalid horizon, must be > 0")

  if (maxNumKernels < 0)
    stop("invalid maxNumKernels, must be > 0")

  paramList <- list(h = horizon,
                    k = maxNumKernels,
		    t = kernelRadiFactor)

  # converting the param list to a cli string to use in java
  cliParams <- convert_params(paramList)

  # initializing the clusterer
  clusterer <- .jnew("moa/clusterers/clustream/Clustream")
  options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
  .jcall(options, "V", "setViaCLIString", cliParams)
  .jcall(clusterer, "V", "prepareForUse")

  # initializing the R object
  l <- list(description = "CluStream",
            options = cliParams,
            javaObj = clusterer)

  class(l) <- c("DSC_CluStream","DSC_Micro","DSC_MOA","DSC")
  
  l
}
