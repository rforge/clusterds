## wrapper for recluster functions

recluster <- function(macro, dsc, type="auto", ...) UseMethod("recluster")

recluster.DSC <- function(macro, dsc, type="auto", ...) {
    stop(gettextf("recluster not implemented for class '%s'.", 
		    paste(class(macro), collapse=", ")))
}

recluster.DSC_Macro <- function(macro, dsc, type="auto", ...) {
    
    x <- get_centers(dsc, type=type)
    weight <- get_weights(dsc, scale=NULL, type=type)
    macro$RObj$cluster(x, weight=weight, ...)
}

### tNN uses an internal reclustering!
recluster.tNN <- function(macro, dsc, type="auto", ...) {
	stop(gettextf("recluster not implemented for class '%s'.", 
		    paste(class(macro), collapse=", ")))
}

