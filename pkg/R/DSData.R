# DSDData - DataStreamData interface
# all DSD classes have these functions
# and an additional function to create the DSD

get_points.default <- function(x, n=1, ...) {
   stop(gettextf("get_points not implemented for class '%s'.", class(x)))
}

get_points <- function(x, n=1, ...) UseMethod("get_points")

print.DSD <- function(x, ...) {
    cat(paste('DSD - Data Stream Datasource:', x$description, '\n'))
	cat(paste('With', x$k, 'clusters', 'in', x$d, 'dimensions', '\n'))
}

