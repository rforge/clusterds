BIRCH <- setRefClass("BIRCH", 
	fields = list(
		data 		= "data.frame",
		weights 	= "numeric",
		BIRCH	    = "ANY",
		radius	    = "numeric",
		compact	    = "numeric",
		keeptree    = "logical",
		columns	    = "ANY",
		assignment	= "numeric"
		), 

	methods = list(
		initialize = function(
			keeptree = FALSE,
			columns = NULL
			) {

		    data <<- data.frame()
		    assignment <<- numeric()
		    weights <<- numeric()
		    BIRCH <<- NULL
		    keeptree <<- keeptree 
		    columns <<- columns
		    radius <<- numeric()
		    compact <<- numeric()

		    .self
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

			for(i in 1:length(BIRCH$members)) assignment[x] <<- i

		    }
		}
		)
	)

### creator    
DSC_BIRCH <- function(radius, compact=radius, keeptree = TRUE, columns = NULL) {

    l <- list(description = "BIRCH",
	    RObj = BIRCH$new(keeptree = keeptree, columns = columns)
	    )

    l$RObj$radius <- radius
    l$RObj$compact <- compact

    class(l) <- c("DSC_BIRCH","DSC_Micro","DSC_R","DSC")
    l
}

### get centers, etc.
get_microclusters.DSC_BIRCH <- function(x, ...) {
    centers <- x$RObj$BIRCH$sumXi/x$RObj$BIRCH$N
    if(length(centers)==0) warning(paste(class(x)[1],": There are no clusters",sep=""))

    as.data.frame(centers)
}

get_weights.DSC_BIRCH <- function(x, scale=NULL) {
    weight <- x$RObj$BIRCH$N

    if(length(weight)==0) warning(paste(class(x)[1],": There are no clusters",sep=""))

    if(!is.null(scale)) weight <- map(weight, scale)
    weight
}
