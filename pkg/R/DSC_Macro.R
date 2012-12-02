
get_centers.DSC_Macro <- function(x, type=c("auto", "micro", "macro"), ...) {
    type <- match.arg(type)
    if(type=="auto") type <- "macro"

    if(type=="micro") get_microclusters(x)
    else get_macroclusters(x)
    }

get_weights.DSC_Macro <- function(x, type=c("auto", "micro", "macro"), 
	scale=NULL) {
    type <- match.arg(type)
    if(type=="auto") type <- "macro"

    if(type=="micro") w <- get_microweights(x)
    else w <- get_macroweights(x)

    if(!is.null(scale)) w <- map(w, scale)
    w
}

get_assignment.DSC_Macro <- function(dsc, points, 
	type=c("auto", "micro", "macro")) {

    type <- match.arg(type)
    if(type=="auto") type <- "macro"

    .get_assignment(get_centers(dsc, type=type), points)
}

