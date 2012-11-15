dbscan <- setRefClass("dbscan", 
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

dbscan$methods(cluster = function(x, weight = rep(1,nrow(x)), ...) {
		if(length(data)>0) {warning("DBSCAN: Previous data is being overridden")}
		weights <<- weight
	    data <<- x
		dbscan <- dbscan(data, eps, MinPts = 5, scale = FALSE, method = c("hybrid", "raw",
    "dist"), seeds = TRUE, showplot = FALSE, countmode = NULL)

		assignment <<- dbscan$cluster
		row_sub = unlist(lapply(assignment, function(x) all(x !=0 )))
		data <<- data[row_sub,]
		assignment <<- assignment[row_sub]
		details <<- dbscan
	}
)

### creator    
DSC_Dbscan <- function(eps, MinPts = 5, scale = FALSE, method = c("hybrid", "raw",
    "dist"), seeds = TRUE, showplot = FALSE, countmode = NULL) {

    dbscan <- dbscan$new(
	    MinPts = MinPts, scale = scale,
	    method = method,seeds=seeds,showplot=showplot,countmode=countmode)

	dbscan$eps <- eps

    l <- list(description = "DBSCAN",
	    RObj = dbscan)

    class(l) <- c("DSC_Dbscan","DSC_R","DSC_Macro","DSC")
    l
}
