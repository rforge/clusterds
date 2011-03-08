
setClass("tNN",
	representation(
		measure	    = "character",
		centroids   = "logical",
		threshold   = "numeric",
		lambda	    = "numeric",
		lambda_factor	= "numeric",
		### this is all in an environment now   
		#centers	= "matrix", ## row names are cluster names
		#counts	    = "numeric",
		#var_thresholds	= "numeric",
		#last	    = "character"
		tnn_d	    = "environment"
		),

	prototype(
		measure	    = "euclidean", 
		centroids   = TRUE,
		threshold   = 0.2,
		lambda	    = 0,
		lambda_factor	= 1,
		tnn_d	    = emptyenv()
		### these are in data as a reference
		#centers	= matrix(nrow=0, ncol=0),
		#counts	    = numeric(),
		#var_thresholds  = numeric(),
		#last	    = as.character(NA)
		)

	## FIXME: Implement check
	#validity= function(object) {}
	)

setMethod("initialize", "tNN", function(.Object, 
		threshold = 0.2, 
		measure = "euclidean", 
		centroids = identical(tolower(measure), "euclidean"), 
		lambda=0, ...){

	    .Object@threshold <- threshold
	    .Object@measure <- measure
	    .Object@centroids <- centroids
	    .Object@lambda <- lambda
	    .Object@lambda_factor <- 2^(-lambda)

	    .Object@tnn_d <- new.env()
	    assign("centers", matrix(nrow=0, ncol=0), envir = .Object@tnn_d)
	    assign("counts", numeric(), envir = .Object@tnn_d)
	    assign("var_thresholds", numeric(), envir = .Object@tnn_d)
	    assign("last", as.character(NA), envir = .Object@tnn_d)

	    #cat("tNN initializes.\n")
	    #validObject(.Object)
	    #.Object <- callNextMethod(.Object, ...)

	    .Object
	})



.cluster_tNN <-	function(RObj, newdata, verbose = FALSE) {
   
    ### 
    x <- RObj$RObj
    nclusters <- function(x) nrow(x@tnn_d$centers)
    ###

    tnn_d <- x@tnn_d

    tnn_d$last <- character(nrow(newdata))

    for(i in 1:nrow(newdata)) {

	nd <- newdata[i,, drop = FALSE]
	if(verbose && i%%50==0) 
	    cat("Added", i, "observations - ",
		nclusters(x), "clusters.\n")

	## cluster is NA for rows with all NAs
	#if(all(is.na(nd))) {
	#    tnn_d$last[i] <- as.character(NA)
	#    next
	#}

	## fade cluster structure?
	if(x@lambda>0) 
	    tnn_d$counts <- tnn_d$counts * x@lambda_factor

	## first cluster
	if(nclusters(x)<1) {
	    sel <- "1"
	    rownames(nd) <- sel
	    tnn_d$centers <- nd
	    tnn_d$counts[sel] <- 1 
	    ## initialize variable threshold
	    tnn_d$var_thresholds[sel] <- x@threshold

	}else{
	    ## find a matching state
	    #sel <- find_clusters(x, nd, match_cluster="exact")

	    ## doing it inline is much faster
	    inside <- dist(nd, tnn_d$centers, 
		    method=x@measure) - tnn_d$var_thresholds
	    min <- which.min(inside)
	    
	    if(inside[min]<=0) sel <- rownames(tnn_d$centers)[min]
	    else sel <- NA

		## NA means no match -> create a new node
		if(is.na(sel)) {
		    ## New node
		    ## get new node name (highest node 
		    ## number is last entry in count)
		    sel <- as.character(as.integer(
				    tail(names(tnn_d$counts),1)) + 1)

		    rownames(nd) <- sel
		    tnn_d$centers <- rbind(tnn_d$centers, nd)
		    tnn_d$counts[sel] <- 1
		    ## initialize threshold
		    tnn_d$var_thresholds[sel] <- x@threshold

		}else{ 
		    ## assign observation to existing node

		    ## update center (if we use centroids)
		    if(x@centroids) {

			nnas <- !is.na(nd)
			tnn_d$centers[sel,nnas] <- 
			(tnn_d$centers[sel,nnas] * 
				tnn_d$counts[sel] 
				+ nd[nnas])/(tnn_d$counts[sel]+1)
			nas <- is.na(tnn_d$centers[sel,])
			tnn_d$centers[sel,nas] <- nd[nas]

		    }

		    ## update counts 
		    tnn_d$counts[sel] <- tnn_d$counts[sel] + 1
		}
	    }

	    tnn_d$last[i] <- sel

	}


	if(verbose) cat ("Done -", nclusters(x), "clusters.\n")

	# invisible(x)
	
	###
	RObj$RObj <- x
	invisible(RObj)
	###
    }


### creator    
DSC_tNN <- function(threshold = 0.2, measure = "euclidean",
	centroids = identical(tolower(measure), "euclidean"), lambda=0) {

    tnn <- new("tNN", threshold=threshold, measure=measure, centroids=centroids,
	    lambda=lambda)

    l <- list(description = "tNN",
	    clusterFun = .cluster_tNN,
	    RObj = tnn)

    class(l) <- c("DSC", "DSC_R", "DSC_tNN")
    l
    l
}

### get centers
getCenters.DSC_tNN <- function(x) x$RObj@tnn_d$centers


