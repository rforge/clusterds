

static <- setRefClass("static", 
	fields = list(
		macroclusters = "data.frame",
		microclusters = "data.frame",
		weights		= "numeric"
		), 

	methods = list(
		initialize = function(
			macroclusters	    = data.frame(),
			microclusters	    = data.frame(),
			weights		    = numeric()
			) {

		    macroclusters	<<- macroclusters 
		    microclusters	<<- microclusters
		    weights		<<- weights

		    .self
		}

		),
	)

static$methods(cluster = function(newdata, verbose = FALSE) {
	    stop("DSC_Static: cluster not implemented")
	}
	)

DSC_Static <- function(macroclusters=NULL,microclusters=NULL,weights=NULL) {
    class <- c("DSC_Static")

    if(is.null(macroclusters)) {
	class <- c(class,"DSC_Micro")
	macroclusters <- data.frame()
    } else {
	class <- c(class,"DSC_Macro")
    }


    if(is.null(microclusters)) {
	microclusters <- data.frame()
    }

    static <- static$new(macroclusters=macroclusters,microclusters=microclusters,weights=weights)

    l <- list(description = "Static",
	    RObj = static)

    class(l) <- c(class,"DSC_R","DSC")
    l
    l
}

### get macroclusters
get_macroclusters.DSC_Static <- function(x, ...) {
    if(length(x$RObj$macroclusters) == 0) warning(paste(class(x)[1],": There are no clusters",sep=""))
    x$RObj$macroclusters
}

get_microclusters.DSC_Static <- function(x, ...) {
    if(length(x$RObj$microclusters) == 0) warning(paste(class(x)[1],": There are no microclusters",sep=""))
    x$RObj$microclusters
}

### FIXME: We need two types of weights!
#get_macroweights.DSC_Static <- function(x) x$RObj$weights
#get_microweights.DSC_Static <- function(x) x$RObj$weights
get_macroweights.DSC_Static <- function(x) rep(1, nclusters(x, type="macro"))
get_microweights.DSC_Static <- function(x) rep(1, nclusters(x, type="micro"))

