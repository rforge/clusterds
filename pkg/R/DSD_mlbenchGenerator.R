DSD_mlbenchGenerator <- function(method, ...) {

    methods <- c("2dnormals","cassini","circle","cuboids","friedman1",
	    "friedman2","friedman3","hypercube", "peak","ringnorm",
	    "shapes","simplex","smiley","spirals","threenorm",
	    "twonorm","waveform","xor")


    if(missing(method)) {
	cat("Available generators are:\n")
	print(methods)
	return()
    }

    #finds index of partial match in array of methods
    m <- pmatch(tolower(method),tolower(methods)) 
    if(is.na(m)) stop("DSD_mlbenchGenerator: Invalid data generator")

    # creating the DSD object
    l <- list(description = paste("mlbench",method),
	    method = method,
	    variables = list(...)
	    )
    class(l) <- c("DSD_mlbenchGenerator","DSD_R","DSD")
    l
}

get_points.DSD_mlbenchGenerator <- function(x, n=1, assignment = FALSE,...) {

    if(is.null(unlist(x$variables)))
	d <- do.call(paste("mlbench.",x$method,sep=""),list(n*20))
    else
	d <- do.call(paste("mlbench.",x$method,sep=""),list(n*20,unlist(x$variables)))

    rand <- sample(1:n*20,n,replace=F)

    dat <- d$x[rand,]

    df <- data.frame()

    df <- rbind(df,dat)

    if(assignment) {
	attr(df,"assignment")<-as.integer(d$classes[rand])
    }

    names(df) <- 1:ncol(df)

    df
}

