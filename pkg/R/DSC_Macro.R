
get_centers.DSC_Macro <- function(x, type=c("auto", "micro", "macro"), ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "macro"

    if(type=="macro") return(get_macroclusters(x))
    else return(get_microclusters(x))
}


get_weights.DSC_Macro <- function(x, type=c("auto", "micro", "macro"),
	scale=NULL, ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "macro"

    if(type=="macro") w <- get_macroweights(x)
    else w <- get_microweights(x)

    if(!is.null(scale)) { 
	if(length(unique(w)) ==1) w <- rep(mean(scale), length(w))
	else w <- map(w, range=scale, from.range=c(0,  
			    max(w, na.rm=TRUE))) 
    }   


    w
}

