reset_stream.default <- function(x) {
    stop(gettextf("reset_stream not implemented for class '%s'.", class(x)))
}

reset_stream <- function(x) UseMethod("reset_stream")

reset_stream.DSD_Wrapper <- function(x) {
    x$state$counter <- 1
}

reset_stream.DSD_ReadStream <- function(x) {
	seek(x$con, where=0)
}
