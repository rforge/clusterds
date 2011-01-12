# DSDData - DataStreamData interface
# all DSD classes have these functions
# and an additional function to create the DSD

getPoints.default <- function(x, numPoints=1, ...) {
   stop(gettextf("getPoints not implemented for class '%s'.", class(x)))
}
