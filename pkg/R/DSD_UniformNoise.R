
DSD_UniformNoise <- function(d=2) { 

    l <- list(description = "Unoform Noise Data Stream", d = d)
    class(l) <- c("DSD_UniformNoise","DSD_R","DSD")
    l
}

get_points.DSD_UniformNoise <- function(x, n=1, assignment = FALSE, ...) {

    data <- as.data.frame(t(replicate(n, runif(x$d))))
    
    if(assignment) attr(data, "assignment") <- rep(NA, n)
    data
}
