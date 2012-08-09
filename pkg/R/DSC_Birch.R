Birch <- setRefClass("Birch", 
	fields = list(
		birch	    = "ANY",
		radius	    = "numeric",
		compact	    = "numeric",
		keeptree    = "logical",
		columns	    = "ANY"
	), 

	methods = list(
		initialize = function(
			radius,
			compact,
			keeptree = FALSE,
			columns = NULL
			) {
		    
		    birch <<- NULL
		    keeptree <<- keeptree 
		    columns <<- columns
		    radius <<- radius
		    compact <<- compact

		    .self
		},
		
		cluster = function(points, ...) {
		    if(is.null(birch))
			birch <<- birch(data.matrix(points), 
			    radius, compact=compact, keeptree=keeptree, 
			    columns=columns)
		    else birch.addToTree(data.matrix(points), birch)
		    }
	)
)

### creator    
DSC_Birch <- function(radius, compact=radius, keeptree = FALSE, columns = NULL) {

    l <- list(description = "Birch",
	    RObj = new("Birch", keeptree = keeptree, columns = columns, 
		    radius = radius, compact = compact)
	    )

    class(l) <- c("DSC_Birch","DSC_Macro","DSC")
    l
}

### get centers, etc.
get_centers.DSC_Birch <- function(x, ...) as.data.frame(x$RObj$birch$sumXi/x$RObj$birch$N)

get_weights.DSC_Birch <- function(x, scale=NULL) {
    weight <- x$RObj$birch$N

    if(!is.null(scale)) weight <- map(weight, scale)
    weight
}
