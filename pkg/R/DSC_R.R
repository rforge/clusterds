### methods common for R implementations of DSC

### make a deep copy of the reference class in RObj 
get_copy.DSC_R <- function(x) {
	temp <- x
	temp$RObj <- x$RObj$copy(TRUE)
	temp
}
