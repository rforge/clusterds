# DSDData - DataStreamData interface
# all DSD classes have these functions
# and an additional function to create the DSD

getPoints.default <- function(x, n=1, ...) {
   stop(gettextf("getPoints not implemented for class '%s'.", class(x)))
}

getPoints <- function(x, n=1, ...) UseMethod("getPoints")

print.DSD <- function(x, ...) {
    cat(paste('DSD - Data Stream Datasource:', x$description, '\n'))
	cat(paste('Number of clusters:', x$k, '\n'))
	cat(paste('Number of dimensions:', x$d, '\n'))
    
	### FIXME: print other stats
}

