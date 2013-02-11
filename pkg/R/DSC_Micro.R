
get_centers.DSC_Micro <- function(x, type=c("auto", "micro", "macro"), assigned = FALSE, ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "micro"

    if(type=="macro") return(get_macroclusters(x))
    else if(!assigned) return(get_microclusters(x))
    else { 
	### only return micro-clusters assigned to macro-clusters
	### used in get_assignment 
	return(get_microclusters(x)[!is.na(microToMacro(x)),])
    }
}


get_weights.DSC_Micro <- function(x, type=c("auto", "micro", "macro"),
	scale=NULL, ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "micro"

    if(type=="macro") w <- get_macroweights(x)
    else w <- get_microweights(x)


    if(!is.null(scale)) { 
	if(length(unique(w)) ==1)  w <- rep(mean(scale), length(w))
	else w <- map(w, range=scale, from.range=c(0,  
			    max(w, na.rm=TRUE))) 
    }
    
    w
}

