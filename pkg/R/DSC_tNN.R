tNN <- setRefClass("tNN", 
	fields = list(
		measure     = "character",
		minPoints   = "numeric",
		distFun	    = "ANY",
		centroids   = "logical",
		threshold   = "numeric",
		lambda      = "numeric",
		lambdaFactor   = "numeric",

		centers	    = "matrix",
		counts	    = "numeric",
		varThresholds = "numeric",
		last	    = "character"
	), 

	methods = list(
		initialize = function(
			measure	    = "Euclidean",
			distFun	    = NULL,
			minPoints   = 2,
			centroids   = TRUE,
			threshold   = 0.2,
			lambda      = 0.01
			) {
		    
		    measure	<<- measure 
		    minPoints	<<- minPoints
		    centroids   <<- centroids
		    threshold   <<- threshold
		    lambda	<<- lambda
		    lambdaFactor <<- 2^(-lambda)

		    if(!is.null(distFun)) distFun <<- distFun
		    else distFun <<- pr_DB[[measure]]

		    centers	<<- matrix(nrow=0, ncol=0)
		    counts	<<- numeric()
		    varThresholds <<- numeric()
		    last	<<- as.character(NA)
		    
		    .self
		}

	),
)


tNN$methods(cluster = function(newdata, verbose = FALSE) {
	    'Cluster new data.' ### online help

	    nclusters <- function(x) nrow(centers)

	    last <<- character(nrow(newdata))

	    for(i in 1:nrow(newdata)) {

		nd <- newdata[i,, drop = FALSE]
		if(verbose && i%%50==0) 
		    cat("Added", i, "observations - ",
			nclusters(x), "clusters.\n")

		## fade cluster structure?
		if(lambda>0) 
		    counts <<- counts * lambdaFactor

		## first cluster
		if(nclusters(x)<1) {
		    sel <- "1"
		    rownames(nd) <- sel
		    centers <<- nd
		    counts[sel] <<- 1 
		    ## initialize variable threshold
		    varThresholds[sel] <<- threshold

		}else{
		    inside <- dist(nd, centers, 
			    method=distFun) - varThresholds
		    min <- which.min(inside)
		    
		    if(inside[min]<=0) sel <- rownames(centers)[min]
		    else sel <- NA

			## NA means no match -> create a new node
			if(is.na(sel)) {
			    ## New node
			    ## get new node name (highest node 
			    ## number is last entry in count)
			    sel <- as.character(as.integer(
					    tail(names(counts),1)) + 1)

			    rownames(nd) <- sel
			    centers <<- rbind(centers, nd)
			    counts[sel] <<- 1
			    ## initialize threshold
			    varThresholds[sel] <<- threshold

			}else{ 
			    ## assign observation to existing node

			    ## update center (if we use centroids)
			    if(centroids) {

				nnas <- !is.na(nd)
				centers[sel,nnas] <<- 
				(centers[sel,nnas] * counts[sel] 
					+ nd[nnas])/(counts[sel]+1)
				nas <- is.na(centers[sel,])
				centers[sel,nas] <<- nd[nas]

			    }

			    ## update counts 
			    counts[sel] <<- counts[sel] + 1
			}
		    }

		    last[i] <<- sel

		} # end for loop

		if(verbose) cat ("Done -", nclusters(x), "clusters.\n")


	    }
)

tNN$methods(clusters = function() centers[counts>minPoints,])

### creator    
DSC_tNN <- function(threshold = 0.2, minPoints = 2, measure = "euclidean",
	centroids = identical(tolower(measure), "euclidean"), lambda=0.5) {

    tnn <- tNN$new(threshold=threshold, minPoints=minPoints, 
	    measure=measure, centroids=centroids,
	    lambda=lambda)

    l <- list(description = "tNN",
	    RObj = tnn)

    class(l) <- c("DSC_tNN","DSC_R","DSC")
    l
    l
}

### get centers
get_centers.DSC_tNN <- function(x, ...) x$RObj$clusters()
