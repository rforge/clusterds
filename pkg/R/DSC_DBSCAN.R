DBSCAN <- setRefClass("DBSCAN", 
	fields = list(
		data	    = "data.frame",
		eps	    = "numeric",
		MinPts	    = "numeric",
		scale	    = "logical",
		method	    = "character",
		seeds	    = "logical",
		showplot    = "logical",
		countmode   = "ANY",
		assignment  = "numeric",
		details	    = "ANY",
		weights	    = "numeric",
		clusterCenters = "data.frame",
		clusterWeights = "numeric"
		), 

	methods = list(
		initialize = function(
			MinPts	= 5,
			scale	= FALSE,
			method	= c("hybrid", "raw","dist"),
			seeds	= TRUE,
			showplot = FALSE,
			countmode = NULL
			) {

		    data    <<- data.frame()
		    weights <<- numeric()
		    MinPts  <<- MinPts
		    scale   <<- scale
		    method  <<- method
		    seeds   <<- seeds
		    showplot <<- showplot
		    countmode <<- countmode
		    clusterWeights <<- numeric()
		    clusterCenters <<- data.frame()

		    .self
		}

		),
	)

DBSCAN$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
	    if(length(data)>0) {
		warning("DBSCAN: Previous data is being overridden")
	    }

	    weights <<- weight
	    data <<- x

	    DBSCAN <- dbscan(data, eps, MinPts = 5, scale = FALSE, 
		    method = c("hybrid", "raw", "dist"), seeds = TRUE, 
		    showplot = FALSE, countmode = NULL)

	    assignment <<- DBSCAN$cluster

	    ### FIXME: we currently remove unassigned data!
	    row_sub <- unlist(lapply(assignment, function(x) all(x !=0 )))
	    data <<- data[row_sub,]
	    assignment <<- assignment[row_sub]
	    details <<- DBSCAN


	    k <- max(assignment)
	    clusterCenters <<- as.data.frame(t(sapply(1:k, FUN=
				    function(i) colMeans(data[assignment==i,]))))
	    clusterWeights <<- sapply(1:k, FUN =
		    function(i) sum(weights[assignment==i], na.rm=TRUE))



	}
	)

### creator    
DSC_DBSCAN <- function(eps, MinPts = 5, scale = FALSE, method = c("hybrid", "raw",
		"dist"), seeds = TRUE, showplot = FALSE, countmode = NULL) {

    DBSCAN <- DBSCAN$new(
	    MinPts = MinPts, scale = scale,
	    method = method,seeds=seeds,showplot=showplot,countmode=countmode)

    DBSCAN$eps <- eps

    l <- list(description = "DBSCAN",
	    RObj = DBSCAN)

    class(l) <- c("DSC_DBSCAN","DSC_Macro","DSC_R","DSC")
    l
}

get_macroclusters.DSC_DBSCAN <- function(x) x$RObj$clusterCenters
get_macroweights.DSC_DBSCAN <- function(x) x$RObj$clusterWeights

get_microclusters.DSC_DBSCAN <- function(x) x$RObj$data
get_microweights.DSC_DBSCAN <- function(x) x$RObj$weights

microToMacro.DSC_DBSCAN <- function(x, micro) x$RObj$assignment[micro]

