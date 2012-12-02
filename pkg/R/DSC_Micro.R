get_centers.DSC_Micro <- function(x, type=c("auto", "micro", "macro"), ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "micro"

    if(type=="macro") stop("No macro-clusters available for a DSC_Micro subclass!")
    
    get_microclusters(x)
}

get_weights.DSC_Micro <- function(x, type=c("auto", "micro", "macro"),
	scale=NULL) {
    type <- match.arg(type)
    if(type=="auto") type <- "micro"

    if(type=="macro") stop("No macro-weights available for a DSC_Micro subclass!")
    w <- get_microweights(x)

    if(!is.null(scale)) w <- map(w, scale)
    w
}

