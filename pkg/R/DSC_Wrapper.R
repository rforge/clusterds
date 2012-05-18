DSC_Wrapper <- function(x) {
    r <- list(centers = x)
    class(r) <- c("DSC_Wrapper", "DSC_R", "DSC")

    r
}

nclusters.DSC_Wrapper <- function(x, ...) nrow(x$centers)

get_centers.DSC_Wrapper <- function(x, ...) x$centers
