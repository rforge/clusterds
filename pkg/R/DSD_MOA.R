# RandomRBFGeneratorEvents options:
# -m modelRandomSeed
# -i instanceRandomSeed
# -K numCluster
# -k numClusterRange
# -R kernelRadius
# -r kernelRadiusRange
# -d densityRange
# -V speed
# -v speedRange
# -N noiseLevel
# -E eventFrequency
# -M eventMergeWeight
# -P eventSplitWeight
# -a numAtts (dimensionality)
# because there are so many parameters, let's only use a few key ones...
DSD_MOA <- function(k=3, d=2, avgRadius=0,
					modelSeed=1, instanceSeed=1) {
    #TODO: need error checking on the params

    # we leave the other parameters as defaults
	# TODO: if the random seeds aren't defined, should we pass them anyway??
	#		the user may be left unaware that they are producing the same data
    paramList <- list(
		m=modelSeed,
	    i=instanceSeed,
	    K=k,
#	    k=3L,
#	    R=avgRadius,
#	    r=0.0,
#	    d=density,
#	    V=100L,
#	    v=0L,
#	    N=0.1,
#	    E=15000L,
#	    M=0.5,
#	    P=0.5,
		a=d)

    # converting the param list to a cli string to use in java
    cliParams <- convertParams(paramList)

    # initializing the clusterer
    strm <- .jnew("moa/streams/clustering/RandomRBFGeneratorEvents")
    options <- .jcall(strm, "Lmoa/options/Options;", "getOptions")
    .jcall(options, "V", "setViaCLIString", cliParams)
	.jcall(strm, "V", "prepareForUse")

    l <- list(description = "RandomRBFGeneratorEvents",
		k = k,
		d = d,
	    cliParams = cliParams,
	    javaObj = strm)

    class(l) <- c("DSD","DSD_MOA","DSD_RandomRBF")
    l
}

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
	
	data
}
