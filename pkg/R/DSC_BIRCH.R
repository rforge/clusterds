BIRCH <- setRefClass("BIRCH", 
	fields = list(
		data 		= "data.frame",
		weights 	= "numeric",
		BIRCH	    = "ANY",
		radius	    = "numeric",
		compact	    = "numeric",
		keeptree    = "logical"
#		assignment	= "numeric"
		), 

	methods = list(
		initialize = function(keeptree = FALSE, radius, compact) {

		    data <<- data.frame()
#		    assignment <<- numeric()
		    weights <<- numeric()
		    BIRCH <<- NULL
		    keeptree <<- keeptree 
		    radius <<- radius
		    compact <<- compact

		    .self
		},
		finalize = function() {
		    ### Seems to be not necessary!
		    #    if(!is.null(BIRCH)) birch.killTree(BIRCH)
		},

		cluster = function(points,  weight = rep(1,nrow(points)), ...) {
		    if(any(is.na(points))) {
			warning("BIRCH: Throwing out point with NA.")
		    } else {
			data <<- rbind(data,points)
			weights <<- c(weights,weight)
			if(is.null(BIRCH)) {
			    BIRCH <<- birch.getTree(birch(data.matrix(points), 
				    radius, compact=compact, keeptree=keeptree, 
				    columns=columns))
			} else {
			    birch.addToTree(data.matrix(points), BIRCH)
			    BIRCH <<- birch.getTree(BIRCH)
			}

#			for(i in 1:length(BIRCH$members)) 
#			    assignment[BIRCH$members[[i]]] <<- i

		    }
		}
		)
	)

### creator    
DSC_BIRCH <- function(radius, compact=radius, keeptree = TRUE) {

    l <- list(description = "BIRCH",
	    RObj = BIRCH$new(keeptree = keeptree, 
		    radius=radius, compact=compact)
	    )

    class(l) <- c("DSC_BIRCH","DSC_Micro","DSC_R","DSC")
    l
}

### get centers, etc.
get_microclusters.DSC_BIRCH <- function(x) {
    centers <- x$RObj$BIRCH$sumXi/x$RObj$BIRCH$N
    as.data.frame(centers)
}

get_microweights.DSC_BIRCH <- function(x) {
    weight <- x$RObj$BIRCH$N
    weight
}
