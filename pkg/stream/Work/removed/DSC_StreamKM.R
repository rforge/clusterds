# StreamKM options:
# IntOption("sizeCoreset", 's', "Size of the coreset.", 10000);
# IntOption("numClusters", 'k', "Number of clusters to compute.", 5);
# IntOption("width", 'w', "Size of Window for training learner.", 100000, 0, Integer.MAX_VALUE);
# IntOption("randomSeed", 'r', "Seed for random behaviour of the classifier.", 1);

DSC_StreamKM <- function(sizeCoreset=10000, k=5, 
	width=100000, randomSeed=NULL) {

    sizeCoreset <- as.integer(sizeCoreset)
    k <- as.integer(k)
    width <- as.integer(width)
    
    if (sizeCoreset < 0) stop("invalid sizeCoreset")
    if (k < 0) stop("invalid k")
    if (width < 0) stop("invalid width")

    if(is.null(randomSeed)) randomSeed <- as.integer(
	    runif(1L, 0, .Machine$integer.max))
    else randomSeed <- as.integer(randomSeed)

    paramList <- list(s=sizeCoreset,
	    k=k,
	    w=width,
	    r=randomSeed)

    # converting the param list to a cli string to use in java
    cliParams <- convert_params(paramList)

    # initializing the clusterer
    clusterer <- .jnew("moa/clusterers/streamkm/StreamKM")
    options <- .jcall(clusterer, "Lmoa/options/Options;", "getOptions")
    .jcall(options, "V", "setViaCLIString", cliParams)
    .jcall(clusterer, "V", "prepareForUse")

    # initializing the R object
    l <- list(description = "StreamKM",
	    options = cliParams,
	    javaObj = clusterer)

    class(l) <- c("DSC_StreamKM","DSC_MOA","DSC")
    l  
}
