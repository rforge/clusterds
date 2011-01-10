# accepts an open connection
DSD_ReadStream <- function(x, delimiter=",") {
  if (class(x) == "character") {
    x <- file(x)
    open(x)
  } else if (class(x) != "connection") {
    stop("please pass a valid connection")
  } else if (isOpen(x) == "FALSE") {
    open(x)
  }

  l <- list(Description = "File Data Stream",
            con = x,
            delimiter = delimiter)
  class(l) <- "fDS"
  l
}

get_instance.ReadStream <- function(x, numPoints=1, ...) {
  # reading from the connection
  lines <- strsplit(x=readLines(x$con, n=numPoints, ok=TRUE), split=x$delimiter)
  
  if (numPoints == 1) {
    inst <- as.numeric(lines[[1]])
  } else if (numPoints > 1) {
    inst <- as.numeric(lines[[1]])

    # converting the strings to a numeric matrix
    for (i in 1:numPoints) {
      inst <- rbind(inst, as.numeric(lines[[i]]))
    }
  } else {
    stop("invalid numPoints")
  }
  
  inst
}
