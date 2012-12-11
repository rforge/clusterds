
get_centers.DSC_Micro <- function(x, type=c("auto", "micro", "macro"), ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "micro"

    if(type=="macro") return(get_macroclusters(x))
    else return(get_microclusters(x))
}


get_weights.DSC_Micro <- function(x, type=c("auto", "micro", "macro"),
	scale=NULL, ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "micro"

    if(type=="macro") w <- get_macroweights(x)
    else w <- get_microweights(x)


    if(!is.null(scale)) w <- map(w, range=scale, 
	    from.range=c(0, max(w, na.rm=TRUE)))
    
    w
}

