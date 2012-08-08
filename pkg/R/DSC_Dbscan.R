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
		weights = "ANY"
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

    class(l) <- c("DSC_Dbscan","DSC_Macro","DSC")
    l
}

### get centers
get_centers.DSC_Dbscan <- function(x, ...) {
	#stop("Not implemented for DBSCAN")
	nclusters <- unique(x$RObj$assignment)
	do.call(rbind,lapply(nclusters,function(clusters){apply(x$RObj$data[which(x$RObj$assignment==clusters),],2, mean)}))
}

nclusters.DSC_Dbscan <- function(x)  {
	length(unique(x$RObj$assignment))
}

get_microclusters.DSC_Dbscan <- function(x, ...) x$RObj$data 

get_assignment.DSC_Dbscan <- function(dsc,points) {
	d <- points
	c <- get_microclusters(dsc)
	dist <- dist(d,c)
	#Find the minimum distance and save the class
	predict <- apply(dist, 1, which.min)
	predict <- unlist(lapply(predict, function(y) dsc$RObj$assignment[y]))
	predict[is.null(predict)] <- 1
	predict[is.na(predict)] <- 1	
	
	predict
}

get_weights.DSC_Dbscan <- function(x, scale=NULL) {
		
	nclusters <- unique(x$RObj$assignment)
	m <- unlist(lapply(nclusters,function(clusters){sum(x$RObj$weights[which(x$RObj$assignment==clusters)])}))
	
	if(!is.null(scale)) m <- map(m, scale)
	
	m
}
