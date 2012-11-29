#TODO: DBSCAN is all caps

DBSCAN <- setRefClass("DBSCAN", 
	fields = list(
		data     = "data.frame",
		eps   = "numeric",
		MinPts	    = "numeric",
		scale   = "logical",
		method   = "character",
		seeds = "logical",
		showplot = "logical",
		countmode = "ANY",
		assignment = "numeric",
		details = "ANY",
		weights = "numeric"
	), 

	methods = list(
		initialize = function(
			MinPts = 5,
			scale = FALSE,
			method = c("hybrid", "raw","dist"),
			seeds = TRUE,
			showplot = FALSE,
			countmode = NULL
			) {
		    
		    data <<- data.frame()
		    weights <<- numeric()
		    MinPts	<<- MinPts
		    scale   <<- scale
		    method <<- method
		    seeds <<- seeds
		    showplot <<- showplot
		    countmode <<- countmode
		    
		    .self
		}

	),
)

DBSCAN$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
		if(length(data)>0) {warning("DBSCAN: Previous data is being overridden")}
		weights <<- weight
	    data <<- x
		DBSCAN <- dbscan(data, eps, MinPts = 5, scale = FALSE, method = c("hybrid", "raw",
    "dist"), seeds = TRUE, showplot = FALSE, countmode = NULL)

		assignment <<- DBSCAN$cluster
		row_sub = unlist(lapply(assignment, function(x) all(x !=0 )))
		data <<- data[row_sub,]
		assignment <<- assignment[row_sub]
		details <<- DBSCAN
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
