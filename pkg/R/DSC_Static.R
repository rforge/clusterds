

Static <- setRefClass("Static", 
	fields = list(
		centers		    = "data.frame",
		weights		    = "numeric",
		macro		    = "logical"
		), 

	methods = list(
		initialize = function(
			centers	    = data.frame(),
			weights	    = numeric(),
			macro	    = FALSE
			) {

		    centers	    <<- centers 
		    weights	    <<- weights
		    macro	    <<- macro

		    .self
		}

		),
	)

Static$methods(cluster = function(newdata, ...) {
	    stop("DSC_Static: cluster not implemented!")
	}
	)

DSC_Static <- function(x, type=c("auto", "micro", "macro")) {
    
    ### figure out type
    type <- get_type(x, type)
    if(type=="macro") macro <- TRUE
    else macro <- FALSE

    static <- Static$new(get_centers(x, type), get_weights(x, type), 
	macro=macro)

    l <- list(description = "Static", RObj = static)
    
    if(macro) micromacro <- "DSC_Macro"
    else micromacro <- "DSC_Micro"

    class(l) <- c("DSC_Static", micromacro, "DSC_R","DSC")

    l
}


get_macroweights.DSC_Static <- function(x) {
    if(!x$RObj$macro) stop("This is a micro-clustering!")
    x$RObj$weights
}
get_macroclusters.DSC_Static <- function(x) {
    if(!x$RObj$macro) stop("This is a micro-clustering!")
    x$RObj$centers
}
get_microweights.DSC_Static <- function(x) {
    if(x$RObj$macro) stop("This is a macro-clustering!")
    x$RObj$weights
}
get_microclusters.DSC_Static <- function(x) {
    if(x$RObj$macro) stop("This is a macro-clustering!")
    x$RObj$centers
}
