# DSDData - DataStreamData interface
# all DSD classes have these functions
# and an additional function to create the DSD

get_instance <- function(x, numPoints=1, ...) UseMethod("get_instance")

get_instance.default <- function(x, numPoints=1, ...) {
   stop(gettextf("get_instance not implemented for class '%s'.", class(x)))
}
