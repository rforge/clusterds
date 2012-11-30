## wrapper for recluster functions

recluster <- function(macro, dsc, ...) UseMethod("recluster")

recluster.DSC <- function(macro, dsc, ...) {
    stop(gettextf("recluster not implemented for class '%s'.", class(macro)))
}

recluster.DSC_Macro <- function(macro, dsc, ...) {
    
    x <- as.data.frame(get_centers(dsc))
    weight <- get_weights(dsc,scale=NULL)
    macro$RObj$cluster(x, weight=weight, ...)
}

### tNN uses an internal reclustering!
recluster.tNN <- function(macro, dsc, ...) {
	stop(gettextf("recluster not implemented for class '%s'.", class(macro)))
}

