dbscan <- setRefClass("dbscan", 
	fields = list(
		data     = "matrix",
		eps   = "numeric",
		MinPts	    = "numeric",
		scale   = "logical",
		method   = "character",
		seeds = "logical",
		showplot = "logical",
		countmode = "logical",
		assignment = "numeric",
		details = "ANY",
		time = "numeric"
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

dbscan$methods(cluster = function(x, ...) {
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

    l <- list(description = "dbscan",
	    RObj = dbscan)

    class(l) <- c("DSC_Dbscan","DSC_Macro","DSC")
    l
}

### get centers
get_centers.DSC_Dbscan <- function(x, ...) x$RObj$data

nclusters.DSC_Dbscan <- function(x)  length(unique(x$RObj$assignment))

get_assignment.DSC_Dbscan <- function(x, ...) x$RObj$assignment