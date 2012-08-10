get_copy.DSC_R <- function(x) {
	temp <- x
	temp$RObj <- x$RObj$copy(TRUE)
	temp
}