### methods common for R implementations of DSC

### make a deep copy of the reference class in RObj 
get_copy.DSC_R <- function(x) {
	temp <- x
	temp$RObj <- x$RObj$copy(TRUE)
	temp
}


microToMacro <- function(x, micro) UseMethod("microToMacro")
microToMacro.default <- function(x, micro) {
    stop(gettextf("microToMacro not implemented for class '%s'.", 
		    paste(class(x), collapse=", ")))
}

