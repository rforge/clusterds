reset_stream.default <- function(dsd) {
    stop(gettextf("reset_stream not implemented for class '%s'.", class(dsd)))
}

reset_stream <- function(x) UseMethod("reset_stream")

reset_stream.DSD_Wrapper <- function(dsd) {
    dsd$state$counter <- 1
}

reset_stream.DSD_ReadStream <- function(dsd) {
	seek(dsd$con, where=0)
}
