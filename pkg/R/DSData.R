# DSDData - DataStreamData interface
# all DSD classes have these functions
# and an additional function to create the DSD

getPoints.default <- function(x, n=1, ...) {
   stop(gettextf("getPoints not implemented for class '%s'.", class(x)))
}

getPoints <- function(x, n=1, ...) UseMethod("getPoints")

print.DSD <- function(x, ...) {
    cat(paste('DSD - Data Stream Datasource:', x$description, '\n'))
    ### FIXME: print number of clusters and other stats
}

