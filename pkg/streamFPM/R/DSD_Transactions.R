
DSD_Transactions <- function(...) stop("DSD_Transactions is an abstract class and cannot be instantiated!")

#get_points <- function(x, n=1, ...) UseMethod("get_points")
#get_points.default <- function(x, n=1, ...) {
#  stop(gettextf("get_points not implemented for class '%s'.",
#                paste(class(x), collapse=", ")))
#}

### in case the stream can be reset (e.g., a stream from a file)
reset_stream <- function(dsd, pos=1) UseMethod("reset_stream")
reset_stream.DSD_Transactions <- function(dsd, pos=1) {
  stop(gettextf("reset_stream not implemented for class '%s'.",
                paste(class(dsd), collapse=", ")))
}
