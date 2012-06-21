birch <- setRefClass("birch", 
	fields = list(
		x = "ANY",
		radius     = "numeric",
		compact   = "numeric",
		keeptree	    = "logical",
		columns = "ANY",
		details = "ANY"
	), 

	methods = list(
		initialize = function(
			keeptree    = FALSE,
			columns   = NULL
			) {
		    
		    keeptree <<- keeptree 
		    columns	<<- columns
		    
		    
		    .self
		}

	),
)

birch $methods(cluster = function(points, ...) {

		birchObj <- birch(data.matrix(points), radius, compact=compact, keeptree=keeptree, columns=columns)
		
		#tree <- birch.getTree(birchObj)
		
		x <<- birchObj $sumXi/birchObj $N
		
	    #xtemp <- unlist(lapply(seq_along(birchObj$members),function(x) {birchObj$members[[x]]}))
		#ytemp <- unlist(lapply(seq_along(birchObj$members),function(x) { rep(x,length(birchObj$members[[x]]))}))
		#id <- order(xtemp)
		#assignment <<- ytemp[id]
		
		details <<- birchObj
	}
)

### creator    
DSC_Birch <- function(radius, compact=radius, keeptree = FALSE, columns = NULL) {

    birch <- birch $new( 
	    keeptree = keeptree, columns = columns)

	birch$radius <- radius
	birch$compact <- compact

    l <- list(description = "hierarchical",
	    RObj = birch)

    class(l) <- c("DSC_Birch","DSC_Macro","DSC")
    l
}

### get centers
get_centers.DSC_Birch <- function(x, ...) x$RObj$x

nclusters.DSC_Birch <- function(x) length(x$RObj$x)

get_assignment.DSC_Birch <- function(x, ...) 1:length(x$RObj$x)
